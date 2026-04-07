library(shiny)
library(tidyverse)
library(sf)
library(ggthemes)
library(mapycusmaximus)

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

polygons_from_sf <- function(sf_obj, id_col = NULL) {
  geoms <- sf::st_geometry(sf_obj)
  res <- lapply(seq_along(geoms), function(i) {
    geom <- geoms[[i]]
    geom_type <- sf::st_geometry_type(geom)
    rings <- list()

    if (geom_type == "POLYGON") {
      coords_list <- sf::st_coordinates(geom)
      ring_ids <- unique(coords_list[, "L2"])
      rings <- lapply(ring_ids, function(rid) {
        ring_coords <- coords_list[coords_list[, "L2"] == rid, c("X", "Y"), drop = FALSE]
        lapply(seq_len(nrow(ring_coords)), function(j) as.numeric(ring_coords[j, ]))
      })
    } else if (geom_type == "MULTIPOLYGON") {
      coords_list <- sf::st_coordinates(geom)
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
      rings = rings
    )
  })
  purrr::compact(res)
}

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
  titlePanel("FGC lens explorer - debug canvas"),
  tags$script(HTML("
(function () {
  function reportStaticDebug() {
    var wrap = document.getElementById('staticWrap');
    var plot = document.getElementById('original_plot');
    if (!wrap || !plot || !window.Shiny) return;

    var wrapRect = wrap.getBoundingClientRect();
    var plotRect = plot.getBoundingClientRect();

    Shiny.setInputValue('static_dom_debug', {
      wrap_width: wrapRect.width,
      wrap_height: wrapRect.height,
      plot_width: plotRect.width,
      plot_height: plotRect.height,
      ratio: plotRect.height > 0 ? plotRect.width / plotRect.height : null,
      time: Date.now()
    }, {priority: 'event'});
  }

  window.addEventListener('resize', reportStaticDebug);

  setTimeout(reportStaticDebug, 500);
  setTimeout(reportStaticDebug, 1200);

  document.addEventListener('shown.bs.tab', function() {
    setTimeout(reportStaticDebug, 100);
    setTimeout(reportStaticDebug, 400);
    setTimeout(reportStaticDebug, 800);
  });
})();
")),
  sidebarLayout(
    sidebarPanel(
      selectInput("centre", "Initial lens centre (LGA)", choices = sort(unique(vic$LGA_NAME)), selected = "MELBOURNE"),
      sliderInput("r_out", "Outer radius (glue)", min = 0.2, max = 0.95, value = 0.6, step = 0.01),
      sliderInput("r_in", "Inner radius (focus)", min = 0.05, max = 0.59, value = 0.33, step = 0.01),
      sliderInput("zoom", "Zoom factor", min = 1, max = 25, value = 12, step = 1),
      sliderInput("squeeze", "Squeeze", min = 0.05, max = 0.95, value = 0.35, step = 0.01),
      sliderInput("n_fac", "Sample size per layer", min = 5, max = 40, value = 10, step = 1),
      actionButton("resample", "Resample facilities"),
      checkboxInput("show_lines", "Show transfer lines", value = TRUE),
      tags$hr(),
      h4("Debug outputs"),
      verbatimTextOutput("debug_bbox"),
      verbatimTextOutput("debug_svg"),
      verbatimTextOutput("debug_projector"),
      verbatimTextOutput("debug_static_dom"),
      verbatimTextOutput("debug_clientdata")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Fisheye (drag lens)",
          tags$h4("Fisheye view (drag lens)", style = "margin-top:0; margin-bottom:10px;"),
          tags$div(
            id = "lensWrap",
            style = "width:100%; height:650px; border:1px solid red;"
          ),
          tags$script(HTML(
"(function () {
  const wrap = document.getElementById('lensWrap');
  const svgNS = 'http://www.w3.org/2000/svg';

  let svg = wrap.querySelector('svg');
  if (!svg) {
    svg = document.createElementNS(svgNS, 'svg');
    svg.style.width = '100%';
    svg.style.height = '650px';
    svg.style.background = '#ffffff';
    svg.style.display = 'block';
    svg.style.touchAction = 'none';
    svg.style.border = '1px solid red';
    wrap.appendChild(svg);
  }

  let base = null;
  let params = { r_in: 0.33, r_out: 0.6, zoom: 12, squeeze: 0.35, show_lines: true };
  let lens = { x: 0, y: 0 };
  let dragging = false;
  let rafPending = false;

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
    // Use fixed CSS height (650) to avoid tab-bar offset inflating getBoundingClientRect
    return { w: r.width || 900, h: 650 };
  }

  function projector() {
    const { w, h } = svgSize();
    const m = 0;

    const dx = base.bbox.xmax - base.bbox.xmin;
    const dy = base.bbox.ymax - base.bbox.ymin;

    const k = Math.min(w / dx, h / dy);
    const padX = (w - k * dx) / 2;
    const padY = (h - k * dy) / 2;

    const xScale = x => padX + (x - base.bbox.xmin) * k;
    const yScale = y => h - padY - (y - base.bbox.ymin) * k;

    const xInv = px => base.bbox.xmin + (px - padX) / k;
    const yInv = py => base.bbox.ymin + (h - padY - py) / k;

    return { xScale, yScale, xInv, yInv, k, padX, padY, w, h, dx, dy };
  }

  function reportSvgDebug() {
    if (!window.Shiny) return;
    const wrapRect = wrap.getBoundingClientRect();
    const svgRect  = svg.getBoundingClientRect();

    Shiny.setInputValue('svg_debug', {
      wrap_width: wrapRect.width,
      wrap_height: wrapRect.height,
      svg_width: svgRect.width,
      svg_height: svgRect.height,
      time: Date.now()
    }, {priority: 'event'});
  }

  function reportProjectorDebug() {
    if (!base || !window.Shiny) return;
    const P = projector();

    Shiny.setInputValue('projector_debug', {
      svg_w: P.w,
      svg_h: P.h,
      dx: P.dx,
      dy: P.dy,
      bbox_ratio: P.dx / P.dy,
      canvas_ratio: P.w / P.h,
      k: P.k,
      padX: P.padX,
      padY: P.padY,
      time: Date.now()
    }, {priority: 'event'});
  }

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

    return [radius_new * Math.cos(angle_new), radius_new * Math.sin(angle_new)];
  }

  function warpPoint(x, y) {
    const s = sFromBBox();
    const nx = (x - lens.x) / s;
    const ny = (y - lens.y) / s;
    const t = fisheyeFGC_one(nx, ny, params.r_in, params.r_out, params.zoom, params.squeeze, 'expand', 0.0);
    return [lens.x + t[0] * s, lens.y + t[1] * s];
  }

  function ringPathD(ringCoords) {
    const P = projector();
    let d = '';
    for (let i = 0; i < ringCoords.length; i++) {
      const pt = ringCoords[i];
      const wpt = warpPoint(pt[0], pt[1]);
      d += (i === 0 ? 'M ' : ' L ') + P.xScale(wpt[0]) + ' ' + P.yScale(wpt[1]);
    }
    return d + ' Z';
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

  function buildOnce() {
    gVic.innerHTML = '';
    gTransfers.innerHTML = '';
    gRacf.innerHTML = '';
    gHosp.innerHTML = '';
    gUI.innerHTML = '';

    base.vic.forEach((poly) => {
      const p = document.createElementNS(svgNS, 'path');
      p.setAttribute('fill', '#d9d9d9');
      p.setAttribute('stroke', '#ffffff');
      p.setAttribute('stroke-width', '1.2');
      p.__data__ = poly;
      gVic.appendChild(p);
    });

    base.transfers.forEach((ln) => {
      const p = document.createElementNS(svgNS, 'path');
      p.setAttribute('stroke', 'grey');
      p.setAttribute('stroke-width', '0.6');
      p.setAttribute('fill', 'none');
      p.setAttribute('opacity', '0.5');
      p.__data__ = ln;
      gTransfers.appendChild(p);
    });

    base.racfs.forEach((pt) => {
      const c = document.createElementNS(svgNS, 'circle');
      c.setAttribute('r', '3');
      c.setAttribute('fill', '#2c7fb8');
      c.__data__ = pt;
      gRacf.appendChild(c);
    });

    base.hospitals.forEach((pt) => {
      const c = document.createElementNS(svgNS, 'circle');
      c.setAttribute('r', '4');
      c.setAttribute('fill', '#d7191c');
      c.__data__ = pt;
      gHosp.appendChild(c);
    });

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
  }

  function updateAll() {
    if (!base) return;
    const P = projector();

    gVic.querySelectorAll('path').forEach((p) => {
      const poly = p.__data__;
      let d = '';
      poly.rings.forEach((ring) => {
        d += ringPathD(ring) + ' ';
      });
      p.setAttribute('d', d);
    });

    gTransfers.style.display = params.show_lines ? 'block' : 'none';
    if (params.show_lines) {
      gTransfers.querySelectorAll('path').forEach((p) => {
        p.setAttribute('d', pathD(p.__data__.coords));
      });
    }

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

    const s = sFromBBox();
    const cx = P.xScale(lens.x);
    const cy = P.yScale(lens.y);

    const ringIn  = gUI.querySelector('circle.ring-in');
    const ringOut = gUI.querySelector('circle.ring-out');

    ringIn.setAttribute('cx', cx);
    ringIn.setAttribute('cy', cy);
    ringIn.setAttribute('r', params.r_in * s * P.k);

    ringOut.setAttribute('cx', cx);
    ringOut.setAttribute('cy', cy);
    ringOut.setAttribute('r', params.r_out * s * P.k);

    reportSvgDebug();
    reportProjectorDebug();
  }

  function scheduleUpdate() {
    if (rafPending) return;
    rafPending = true;
    requestAnimationFrame(() => {
      rafPending = false;
      updateAll();
    });
  }

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
  function reportSvgWidth() {
    if (window.Shiny) {
      const sz = svgSize();
      Shiny.setInputValue('svg_w_px', sz.w, {priority: 'event'});
    }
  }

  window.addEventListener('resize', function() {
    scheduleUpdate();
    reportSvgWidth();
  });

  // Shiny message handlers
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('lens-base', function (payload) {
      base = payload;
      base.bbox = payload.bbox;
      lens.x = payload.centre.x;
      lens.y = payload.centre.y;
      buildOnce();
      scheduleUpdate();
      // Report the exact SVG pixel width so renderPlot can match it
      setTimeout(reportSvgWidth, 50); // slight delay ensures DOM is laid out
    });
  }
})();"
          ))
        ),
        tabPanel(
          "Original (static)",
          tags$h4("Original Victoria (matched framing)", style = "margin-top:0; margin-bottom:10px;"),
          div(
            id = "staticWrap",
            style = "border:1px solid blue; display:inline-block; width:100%;",
            plotOutput("original_plot", height = "650px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  bbox_val <- reactiveVal(NULL)
  sampled_layers <- reactiveVal(prepare_network(n_hosp = 10, n_racf = 10))

  observeEvent(list(input$resample, input$n_fac), {
    sampled_layers(prepare_network(n_hosp = input$n_fac, n_racf = input$n_fac))
  }, ignoreInit = TRUE)

  centre_point <- reactive({
    req(input$centre)
    coords <- vic |>
      dplyr::filter(LGA_NAME == input$centre) |>
      sf::st_point_on_surface() |>
      sf::st_geometry() |>
      sf::st_coordinates()
    list(x = as.numeric(coords[1, 1]), y = as.numeric(coords[1, 2]), label = input$centre)
  })

  observeEvent(list(input$centre, input$resample, input$n_fac), {
    layers <- sampled_layers()

    hosp <- layers$hospitals |> mutate(type = "hospital", id = destination)
    racf <- layers$racfs |> mutate(type = "racf", id = source)
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
      vic = polygons_from_sf(vic, id_col = "LGA_NAME"),
      transfers = lines_from_sf(layers$transfers),
      hospitals = points_from_sf(hosp, id_col = "id"),
      racfs = points_from_sf(racf, id_col = "id")
    )

    session$sendCustomMessage("lens-base", payload)
  }, ignoreInit = FALSE)

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

    # Use the SVG's actual pixel width reported by JS (same element, same measurement).
    # Fall back to clientData width only if JS value not yet available.
    svg_w_px <- input$svg_w_px
    if (is.null(svg_w_px) || svg_w_px <= 0) {
      svg_w_px <- session$clientData$output_original_plot_width
    }
    req(!is.null(svg_w_px) && svg_w_px > 0)

    plot_h_px <- 650  # matches SVG CSS height exactly

    dx <- as.numeric(bb["xmax"]) - as.numeric(bb["xmin"])
    dy <- as.numeric(bb["ymax"]) - as.numeric(bb["ymin"])

    # Replicate JS projector() with m = 0 (confirmed by k = h/dy = 126.12)
    m <- 0
    k        <- min((svg_w_px - 2*m) / dx, (plot_h_px - 2*m) / dy)
    padX_data <- ((svg_w_px  - 2*m) - k * dx) / 2 / k
    padY_data <- ((plot_h_px - 2*m) - k * dy) / 2 / k

    xlim <- c(as.numeric(bb["xmin"]) - padX_data, as.numeric(bb["xmax"]) + padX_data)
    ylim <- c(as.numeric(bb["ymin"]) - padY_data, as.numeric(bb["ymax"]) + padY_data)

    ggplot() +
      geom_sf(data = vic, fill = "#d9d9d9", color = "white", linewidth = 0.5) +
      { if (isTRUE(input$show_lines)) geom_sf(data = layers$transfers, aes(linewidth = transfer_n), color = "grey50", alpha = 0.45) } +
      geom_sf(data = layers$racfs, color = "#2c7fb8", size = 1, alpha = 0.9) +
      geom_sf(data = layers$hospitals, color = "#d7191c", size = 1.2, alpha = 0.9) +
      scale_linewidth(range = c(0.2, 1.2), guide = "none") +
      coord_sf(
        crs    = st_crs(vic),
        xlim   = xlim,
        ylim   = ylim,
        expand = FALSE
      ) +
      theme_map() +
      theme(
        plot.margin      = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
      )
  }, height = 650, res = 110)

  output$debug_bbox <- renderPrint({
    bb <- bbox_val()
    req(bb)

    dx <- as.numeric(bb["xmax"] - bb["xmin"])
    dy <- as.numeric(bb["ymax"] - bb["ymin"])

    list(
      xmin = as.numeric(bb["xmin"]),
      xmax = as.numeric(bb["xmax"]),
      ymin = as.numeric(bb["ymin"]),
      ymax = as.numeric(bb["ymax"]),
      dx = dx,
      dy = dy,
      bbox_ratio = dx / dy
    )
  })

  output$debug_svg <- renderPrint({
    input$svg_debug
  })

  output$debug_projector <- renderPrint({
    input$projector_debug
  })

  output$debug_static_dom <- renderPrint({
    input$static_dom_debug
  })

  output$debug_clientdata <- renderPrint({
    w <- session$clientData$output_original_plot_width
    h <- session$clientData$output_original_plot_height

    list(
      original_plot_width = w,
      original_plot_height = h,
      original_plot_ratio = if (!is.null(w) && !is.null(h) && h != 0) w / h else NULL
    )
  })
}

shinyApp(ui, server)