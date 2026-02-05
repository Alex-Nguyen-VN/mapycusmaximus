library(shiny)
library(tidyverse)
library(sf)
library(ggthemes)
library(mapycusmaximus)

# Load data once for responsiveness
vic  <- mapycusmaximus::vic
conn <- mapycusmaximus::conn_fish

prepare_network <- function(n_hosp = 10, n_racf = 10) {
  conn_sample <- conn |>
    st_drop_geometry() |>
    select(source, destination, long_racf, lat_racf, long_hosp, lat_hosp) |>
    distinct() |>
    slice_sample(n = max(n_hosp, n_racf)) |>
    mutate(transfer_n = sample(20:80, n(), replace = TRUE))

  hospitals <- conn_sample |>
    select(destination, long_hosp, lat_hosp) |>
    distinct() |>
    slice_head(n = n_hosp) |>
    st_as_sf(coords = c("long_hosp", "lat_hosp"), crs = 4326) |>
    st_transform(st_crs(vic)) |>
    mutate(type = "hospital")

  racfs <- conn_sample |>
    select(source, long_racf, lat_racf) |>
    distinct() |>
    slice_head(n = n_racf) |>
    st_as_sf(coords = c("long_racf", "lat_racf"), crs = 4326) |>
    st_transform(st_crs(vic)) |>
    mutate(type = "racf")

  transfers <- conn_sample |>
    mutate(
      geometry = pmap(
        list(long_racf, lat_racf, long_hosp, lat_hosp),
        ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = TRUE))
      )
    ) |>
    st_as_sf(crs = 4326) |>
    st_transform(st_crs(vic))

  list(hospitals = hospitals, racfs = racfs, transfers = transfers)
}

# --- NEW: helper for polygons (not lines!) ---
polygons_from_sf <- function(sf_obj, id_col = NULL) {
  geoms <- sf::st_geometry(sf_obj)
  res <- lapply(seq_along(geoms), function(i) {
    # Handle both POLYGON and MULTIPOLYGON
    geom <- geoms[[i]]
    geom_type <- sf::st_geometry_type(geom)
    
    rings <- list()
    
    if (geom_type == "POLYGON") {
      coords_list <- sf::st_coordinates(geom)
      # Group by ring (L2 column distinguishes exterior/holes)
      ring_ids <- unique(coords_list[, "L2"])
      rings <- lapply(ring_ids, function(rid) {
        ring_coords <- coords_list[coords_list[, "L2"] == rid, c("X", "Y"), drop = FALSE]
        lapply(seq_len(nrow(ring_coords)), function(j) as.numeric(ring_coords[j, ]))
      })
    } else if (geom_type == "MULTIPOLYGON") {
      coords_list <- sf::st_coordinates(geom)
      # Group by L3 (polygon part) and L2 (ring within polygon)
      poly_ids <- unique(coords_list[, "L3"])
      for (pid in poly_ids) {
        poly_coords <- coords_list[coords_list[, "L3"] == pid, , drop = FALSE]
        ring_ids <- unique(poly_coords[, "L2"])
        for (rid in ring_ids) {
          ring_coords <- poly_coords[poly_coords[, "L2"] == rid, c("X", "Y"), drop = FALSE]
          rings[[length(rings) + 1]] <- lapply(seq_len(nrow(ring_coords)), function(j) as.numeric(ring_coords[j, ]))
        }
      }
    }
    
    if (length(rings) == 0) return(NULL)
    
    list(
      id = if (!is.null(id_col) && id_col %in% names(sf_obj)) as.character(sf_obj[[id_col]][i]) else paste0("poly-", i),
      rings = rings  # array of rings (each ring is array of [x,y] coords)
    )
  })
  purrr::compact(res)
}

# --- helpers: sf -> plain coords for JS/SVG ---
lines_from_sf <- function(sf_obj, id_col = NULL) {
  geoms <- sf::st_geometry(sf_obj)
  res <- lapply(seq_along(geoms), function(i) {
    coords <- sf::st_coordinates(geoms[[i]])
    if (nrow(coords) == 0) return(NULL)
    xy <- coords[, c("X", "Y"), drop = FALSE]
    list(
      id = if (!is.null(id_col) && id_col %in% names(sf_obj)) as.character(sf_obj[[id_col]][i]) else paste0("ln-", i),
      coords = lapply(seq_len(nrow(xy)), function(j) as.numeric(xy[j, ]))
    )
  })
  purrr::compact(res)
}

points_from_sf <- function(sf_obj, id_col = NULL) {
  coords <- sf::st_coordinates(sf_obj)
  n <- nrow(coords)
  ids <- if (!is.null(id_col) && id_col %in% names(sf_obj)) as.character(sf_obj[[id_col]]) else as.character(seq_len(n))
  lapply(seq_len(n), function(i) {
    list(
      id = ids[i],
      x = as.numeric(coords[i, 1]),
      y = as.numeric(coords[i, 2])
    )
  })
}

ui <- fluidPage(
  titlePanel("Focus–Glue–Context lens explorer (drag lens in browser)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("centre", "Initial lens centre (LGA)", choices = sort(unique(vic$LGA_NAME)), selected = "MELBOURNE"),
      sliderInput("r_in", "Inner radius (focus)", min = 0.05, max = 0.6, value = 0.33, step = 0.01),
      sliderInput("r_out", "Outer radius (glue)", min = 0.2, max = 0.95, value = 0.6, step = 0.01),
      sliderInput("zoom", "Zoom factor", min = 1, max = 25, value = 12, step = 1),
      sliderInput("squeeze", "Squeeze", min = 0.05, max = 0.95, value = 0.35, step = 0.01),
      sliderInput("n_fac", "Sample size per layer", min = 5, max = 40, value = 10, step = 1),
      actionButton("resample", "Resample facilities"),
      checkboxInput("show_lines", "Show transfer lines", value = TRUE),
      tags$hr(),
      tags$small("Tip: Drag anywhere on the fisheye panel to move the lens in real-time (no re-render flash).")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Fisheye (drag lens)",
          tags$div(id = "lensWrap", style = "width:100%; height:650px;"),
          tags$script(HTML(
"(function () {
  const wrap = document.getElementById('lensWrap');
  const svgNS = 'http://www.w3.org/2000/svg';

  let svg = wrap.querySelector('svg');
  if (!svg) {
    svg = document.createElementNS(svgNS, 'svg');
    svg.style.width = '100%';
    svg.style.height = '650px';
    svg.style.background = '#ffffff';  // WHITE BACKGROUND
    svg.style.touchAction = 'none'; // smooth pointer dragging
    wrap.appendChild(svg);
  }

  // ---- state ----
  let base = null; // base geometry + bbox
  let params = { r_in: 0.33, r_out: 0.6, zoom: 12, squeeze: 0.35, show_lines: true };
  let lens = { x: 0, y: 0 }; // data coords
  let dragging = false;
  let rafPending = false;

  // ---- groups ----
  function ensureG(cls) {
    let g = svg.querySelector('g.' + cls);
    if (!g) {
      g = document.createElementNS(svgNS, 'g');
      g.setAttribute('class', cls);
      svg.appendChild(g);
    }
    return g;
  }
  const gVic = ensureG('vic');
  const gTransfers = ensureG('transfers');
  const gRacf = ensureG('racf');
  const gHosp = ensureG('hosp');
  const gUI = ensureG('ui');

  function svgSize() {
    const r = svg.getBoundingClientRect();
    return { w: r.width || 900, h: r.height || 650 };
  }

  // --- uniform projector (like coord_equal): circles stay circles ---
  function projector() {
    const { w, h } = svgSize();
    const m = 20;

    const dx = base.bbox.xmax - base.bbox.xmin;
    const dy = base.bbox.ymax - base.bbox.ymin;

    // one scale for both axes
    const k = Math.min((w - 2*m) / dx, (h - 2*m) / dy);

    // letterbox center
    const padX = (w - 2*m - k*dx) / 2;
    const padY = (h - 2*m - k*dy) / 2;

    const xScale = x => m + padX + (x - base.bbox.xmin) * k;
    const yScale = y => h - m - padY - (y - base.bbox.ymin) * k;

    const xInv = px => base.bbox.xmin + (px - m - padX) / k;
    const yInv = py => base.bbox.ymin + (h - m - padY - py) / k;

    return { xScale, yScale, xInv, yInv, k };
  }

  // ---- fisheye warp (MATCHES mapycusmaximus::sf_fisheye + fisheye_fgc) ----
  function sFromBBox() {
    const sx = (base.bbox.xmax - base.bbox.xmin) / 2 || 1;
    const sy = (base.bbox.ymax - base.bbox.ymin) / 2 || 1;
    return Math.max(sx, sy);
  }

  function fisheyeFGC_one(nx, ny, r_in, r_out, zoom_factor, squeeze_factor, method, revolution) {
    const radius = Math.hypot(nx, ny);
    const angle  = Math.atan2(ny, nx);

    let radius_new = radius;
    let angle_new  = angle;

    if (radius <= r_in) {
      const norm_r = radius / r_in;
      const expanded_r = norm_r * zoom_factor;
      radius_new = Math.min(expanded_r, 1.0) * r_in;

    } else if (radius <= r_out) {
      const u = (radius - r_in) / (r_out - r_in);

      if (method === 'outward') {
        const u_compressed = Math.pow(u, 1 / squeeze_factor);
        const compressed_width = (r_out - r_in) * squeeze_factor;
        radius_new = r_out - (1 - u_compressed) * compressed_width;

      } else {
        const u_inner = Math.min(u * 2, 1);
        const expansion_factor_inner = Math.pow(u_inner, 1 / squeeze_factor);
        const radius_inner = r_in + (1 - expansion_factor_inner * squeeze_factor) * (radius - r_in);

        const u_outer = Math.max((u - 0.5) * 2, 0);
        const expansion_factor_outer = Math.pow(u_outer, 1 / squeeze_factor);
        const radius_outer = radius + expansion_factor_outer * squeeze_factor * (r_out - radius);

        radius_new = (u <= 0.5) ? radius_inner : radius_outer;
      }

      if (revolution && revolution !== 0) {
        const rotation_amount = revolution * u * (1 - u) * 4;
        angle_new = angle + rotation_amount;
      }
    }

    const x_new = radius_new * Math.cos(angle_new);
    const y_new = radius_new * Math.sin(angle_new);
    return [x_new, y_new];
  }

  function warpPoint(x, y) {
    const s = sFromBBox();
    const nx = (x - lens.x) / s;
    const ny = (y - lens.y) / s;

    const method = 'expand';
    const revolution = 0.0;

    const t = fisheyeFGC_one(nx, ny, params.r_in, params.r_out, params.zoom, params.squeeze, method, revolution);
    return [lens.x + t[0] * s, lens.y + t[1] * s];
  }

  // NEW: Build path for a polygon ring
  function ringPathD(ringCoords) {
    const P = projector();
    let d = '';
    for (let i = 0; i < ringCoords.length; i++) {
      const pt = ringCoords[i];
      const wpt = warpPoint(pt[0], pt[1]);
      d += (i === 0 ? 'M ' : ' L ') + P.xScale(wpt[0]) + ' ' + P.yScale(wpt[1]);
    }
    return d + ' Z';  // Close the path
  }

  function pathD(lineCoords) {
    const P = projector();
    let d = '';
    for (let i = 0; i < lineCoords.length; i++) {
      const pt = lineCoords[i];
      const wpt = warpPoint(pt[0], pt[1]);
      d += (i === 0 ? 'M ' : ' L ') + P.xScale(wpt[0]) + ' ' + P.yScale(wpt[1]);
    }
    return d;
  }

  // Build DOM once (no replaceChildren = no flashing)
  function buildOnce() {
    // POLYGONS (not lines!)
    gVic.innerHTML = '';
    base.vic.forEach((poly) => {
      const p = document.createElementNS(svgNS, 'path');
      p.setAttribute('fill', '#d9d9d9');      // LIGHT GREY FILL
      p.setAttribute('stroke', '#ffffff');     // WHITE BOUNDARIES
      p.setAttribute('stroke-width', '1.2');   // Visible boundaries
      p.__data__ = poly;
      gVic.appendChild(p);
    });

    gTransfers.innerHTML = '';
    base.transfers.forEach((ln) => {
      const p = document.createElementNS(svgNS, 'path');
      p.setAttribute('stroke', 'grey');
      p.setAttribute('stroke-width', '0.6');
      p.setAttribute('fill', 'none');
      p.setAttribute('opacity', '0.5');
      p.__data__ = ln;
      gTransfers.appendChild(p);
    });

    gRacf.innerHTML = '';
    base.racfs.forEach((pt) => {
      const c = document.createElementNS(svgNS, 'circle');
      c.setAttribute('r', '3');
      c.setAttribute('fill', '#2c7fb8');
      c.setAttribute('opacity', '0.9');
      c.__data__ = pt;
      gRacf.appendChild(c);
    });

    gHosp.innerHTML = '';
    base.hospitals.forEach((pt) => {
      const c = document.createElementNS(svgNS, 'circle');
      c.setAttribute('r', '4');
      c.setAttribute('fill', '#d7191c');
      c.setAttribute('opacity', '0.9');
      c.__data__ = pt;
      gHosp.appendChild(c);
    });

    // Lens rings - use CIRCLES not paths
    const ringIn = document.createElementNS(svgNS, 'circle');
    ringIn.setAttribute('class', 'ring-in');
    ringIn.setAttribute('fill', 'none');
    ringIn.setAttribute('stroke', '#111');
    ringIn.setAttribute('stroke-dasharray', '4 3');
    ringIn.setAttribute('stroke-width', '1.2');
    gUI.appendChild(ringIn);

    const ringOut = document.createElementNS(svgNS, 'circle');
    ringOut.setAttribute('class', 'ring-out');
    ringOut.setAttribute('fill', 'none');
    ringOut.setAttribute('stroke', '#111');
    ringOut.setAttribute('opacity', '0.45');
    ringOut.setAttribute('stroke-width', '1.0');
    gUI.appendChild(ringOut);

    const title = document.createElementNS(svgNS, 'text');
    title.setAttribute('x', '20');
    title.setAttribute('y', '22');
    title.setAttribute('font-size', '14');
    title.setAttribute('font-weight', 'bold');
    title.setAttribute('fill', '#111');
    title.textContent = 'Fisheye view (drag lens)';
    gUI.appendChild(title);
  }

  function updateAll() {
    if (!base) return;
    const P = projector();

    // Vic POLYGONS - rebuild each polygon from all its rings
    gVic.querySelectorAll('path').forEach((p) => {
      const poly = p.__data__;
      let d = '';
      poly.rings.forEach((ring) => {
        d += ringPathD(ring) + ' ';
      });
      p.setAttribute('d', d);
    });

    // Transfers
    gTransfers.style.display = params.show_lines ? 'block' : 'none';
    if (params.show_lines) {
      gTransfers.querySelectorAll('path').forEach((p) => {
        p.setAttribute('d', pathD(p.__data__.coords));
      });
    }

    // Points
    gRacf.querySelectorAll('circle').forEach((c) => {
      const pt = c.__data__;
      const wpt = warpPoint(pt.x, pt.y);
      c.setAttribute('cx', P.xScale(wpt[0]));
      c.setAttribute('cy', P.yScale(wpt[1]));
    });

    gHosp.querySelectorAll('circle').forEach((c) => {
      const pt = c.__data__;
      const wpt = warpPoint(pt.x, pt.y);
      c.setAttribute('cx', P.xScale(wpt[0]));
      c.setAttribute('cy', P.yScale(wpt[1]));
    });

    // Lens rings - draw as SIMPLE CIRCLES (no warp!)
    // These mark the boundaries in data space, not warped space
    const s = sFromBBox();
    const cx = P.xScale(lens.x);
    const cy = P.yScale(lens.y);

    const ringIn  = gUI.querySelector('circle.ring-in');
    const ringOut = gUI.querySelector('circle.ring-out');

    // Radius in data coords * projection scale = pixel radius
    const rInPx  = params.r_in  * s * P.k;
    const rOutPx = params.r_out * s * P.k;

    ringIn.setAttribute('cx', cx);
    ringIn.setAttribute('cy', cy);
    ringIn.setAttribute('r', rInPx);

    ringOut.setAttribute('cx', cx);
    ringOut.setAttribute('cy', cy);
    ringOut.setAttribute('r', rOutPx);
  }

  function scheduleUpdate() {
    if (rafPending) return;
    rafPending = true;
    requestAnimationFrame(() => {
      rafPending = false;
      updateAll();
    });
  }

  // Drag lens
  svg.addEventListener('pointerdown', (e) => {
    if (!base) return;
    dragging = true;
    svg.setPointerCapture(e.pointerId);
    const rect = svg.getBoundingClientRect();
    const P = projector();
    lens.x = P.xInv(e.clientX - rect.left);
    lens.y = P.yInv(e.clientY - rect.top);
    scheduleUpdate();
  });

  svg.addEventListener('pointermove', (e) => {
    if (!dragging || !base) return;
    const rect = svg.getBoundingClientRect();
    const P = projector();
    lens.x = P.xInv(e.clientX - rect.left);
    lens.y = P.yInv(e.clientY - rect.top);
    scheduleUpdate();
  });

  svg.addEventListener('pointerup', () => { dragging = false; });
  svg.addEventListener('pointercancel', () => { dragging = false; });

  window.addEventListener('resize', scheduleUpdate);

  // Shiny message handlers
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('lens-base', function (payload) {
      base = payload;
      base.bbox = payload.bbox;
      lens.x = payload.centre.x;
      lens.y = payload.centre.y;
      buildOnce();
      scheduleUpdate();
    });

    Shiny.addCustomMessageHandler('lens-params', function (p) {
      params = Object.assign(params, p);
      scheduleUpdate();
    });
  }
})();"
          ))
        ),
        tabPanel("Original (static)", plotOutput("original_plot", height = "650px"))
      )
    )
  )
)

server <- function(input, output, session) {

  # shared bbox so static plot matches JS framing
  bbox_val <- reactiveVal(NULL)

  # store sampled network (changes only when resample / n_fac changes)
  sampled_layers <- reactiveVal(prepare_network(n_hosp = 10, n_racf = 10))

  observeEvent(list(input$resample, input$n_fac), {
    sampled_layers(prepare_network(n_hosp = input$n_fac, n_racf = input$n_fac))
  }, ignoreInit = TRUE)

  # initial lens centre (LGA point) in vic CRS
  centre_point <- reactive({
    req(input$centre)
    coords <- vic |>
      dplyr::filter(LGA_NAME == input$centre) |>
      sf::st_point_on_surface() |>
      sf::st_geometry() |>
      sf::st_coordinates()
    list(x = as.numeric(coords[1, 1]), y = as.numeric(coords[1, 2]), label = input$centre)
  })

  # Send base geometry when: centre changes, resample happens, or n_fac changes
  observeEvent(list(input$centre, input$resample, input$n_fac), {
    layers <- sampled_layers()

    hosp <- layers$hospitals |>
      mutate(type = "hospital", id = destination)

    racf <- layers$racfs |>
      mutate(type = "racf", id = source)

    pts <- bind_rows(hosp, racf)

    bind0 <- dplyr::bind_rows(
      vic |> dplyr::mutate(.layer = "vic"),
      pts |> dplyr::mutate(.layer = "pts"),
      layers$transfers |> dplyr::mutate(.layer = "transfers")
    )

    bb <- sf::st_bbox(bind0)

    bbox_val(bb)

    payload <- list(
      bbox = as.list(bb),
      centre = centre_point(),
      vic = polygons_from_sf(vic, id_col = "LGA_NAME"),  # POLYGONS, not lines!
      transfers = lines_from_sf(layers$transfers),
      hospitals = points_from_sf(hosp, id_col = "id"),
      racfs = points_from_sf(racf, id_col = "id")
    )

    session$sendCustomMessage("lens-base", payload)
  }, ignoreInit = FALSE)

  # Send lens params frequently (tiny message, no redraw on server)
  observeEvent(list(input$r_in, input$r_out, input$zoom, input$squeeze, input$show_lines), {
    session$sendCustomMessage("lens-params", list(
      r_in = input$r_in,
      r_out = input$r_out,
      zoom = input$zoom,
      squeeze = input$squeeze,
      show_lines = isTRUE(input$show_lines)
    ))
  }, ignoreInit = FALSE)

  output$original_plot <- renderPlot({
    layers <- sampled_layers()
    bb <- bbox_val()
    req(bb)

    ggplot() +
      geom_sf(data = vic, fill = "#d9d9d9", color = "white", linewidth = 0.5) +  # Match JS styling
      { if (isTRUE(input$show_lines)) geom_sf(data = layers$transfers, aes(linewidth = transfer_n), color = "grey50", alpha = 0.45) } +
      geom_sf(data = layers$racfs, color = "#2c7fb8", size = 1, alpha = 0.9) +
      geom_sf(data = layers$hospitals, color = "#d7191c", size = 1.2, alpha = 0.9) +
      scale_linewidth(range = c(0.2, 1.2), guide = "none") +
      coord_sf(
        crs = st_crs(vic),
        xlim = c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])),
        ylim = c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])),
        expand = FALSE
      ) +
      ggtitle("Original Victoria (matched framing)") +
      theme_map() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  # WHITE background
        plot.background = element_rect(fill = "white", color = NA)
      )
  }, res = 110)
}

shinyApp(ui, server)
