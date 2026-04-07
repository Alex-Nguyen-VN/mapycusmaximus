# Launch Interactive Fisheye Lens Explorer

Launch Interactive Fisheye Lens Explorer

## Usage

``` r
shiny_fisheye(debug = "off", ...)
```

## Arguments

- debug:

  Controls whether the Debug tab is shown. Accepted values are `"off"`
  (default), `"on"`, `FALSE`, and `TRUE`.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

The value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), called
primarily for its side effect of launching the application.
