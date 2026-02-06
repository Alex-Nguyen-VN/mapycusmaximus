# Launch Interactive Fisheye Lens Explorer

Launches an interactive Shiny application for exploring
Focus–Glue–Context (FGC) fisheye lens transformations on geographic
data. The app provides real-time lens positioning, adjustable distortion
parameters, and side-by-side comparison of transformed and original
views.

The application demonstrates fisheye effects on Victoria, Australia
Local Government Areas (LGAs) with a network of healthcare facilities
and aged care connections. Users can drag the lens to any position and
adjust parameters without server-side re-rendering for smooth,
responsive interaction.

## Usage

``` r
shiny_fisheye(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). Common
  options include:

  - `launch.browser`: Logical or function to handle app launch (default
    = `TRUE`)

  - `port`: Integer port number (default = random available port)

  - `host`: Character host IP address (default = `"127.0.0.1"`)

  - `display.mode`: Character display mode, `"auto"`, `"normal"`, or
    `"showcase"`

## Value

Invisible `NULL`. The function is called for its side effect of
launching the Shiny application. The R session will be blocked until the
app is stopped (press Escape or close the browser window).

## Details

### Features

- **Interactive lens dragging**: Click and drag anywhere on the map to
  reposition the fisheye lens in real-time

- **Parameter controls**: Adjust inner radius (focus), outer radius
  (glue), zoom factor, and squeeze factor

- **Facility sampling**: Randomly sample healthcare facilities and
  residential aged care facilities (RACFs) with adjustable sample size

- **Transfer visualization**: Toggle display of patient transfer
  connections between facilities

- **Side-by-side comparison**: Compare fisheye-transformed and original
  static views

### Requirements

The Shiny app requires the following suggested packages:

- `shiny`

- `tidyr`

- `dplyr`

- `purrr`

- `ggthemes`

If any are missing, install with:

    install.packages(c("shiny", "tidyr", "dplyr", "purrr", "ggthemes"))

### Implementation Notes

The app uses client-side JavaScript for smooth lens dragging without
server round-trips. Fisheye transformations match the mathematical
implementation in
[`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
and
[`sf_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/sf_fisheye.md),
applied to polygons, lines, and points in real-time using SVG rendering.

## See also

- [`fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/fisheye_fgc.md)
  for the core transformation function

- [`sf_fisheye()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/sf_fisheye.md)
  for transforming spatial geometries

- [`plot_fisheye_fgc()`](https://alex-nguyen-vn.github.io/mapycusmaximus/reference/plot_fisheye_fgc.md)
  for static visualizations

- [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) for
  additional launch options

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the app with default settings
shiny_fisheye()

# Launch in browser on specific port
shiny_fisheye(launch.browser = TRUE, port = 8080)

# Launch in RStudio Viewer pane
shiny_fisheye(launch.browser = rstudioapi::viewer)
} # }
```
