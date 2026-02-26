# Chico Shiny App

# Installation
``` r
remotes::install_github("jaspershen-lab/chico_shiny")
```

# Start
``` r
library(chicoshiny)
run_chico_shiny()
```

# Local development (run from source)
If you are running this project directly from the repository (without installing the package first),
`system.file()` may not find files under `inst/` unless the app code handles local paths.

This repo now supports local source runs. Use one of the following approaches from the project root:

## Option 1 (recommended): `devtools::load_all()`
``` r
devtools::load_all()
run_chico_shiny()
```

## Option 2: `source()` files manually
``` r
source("R/utils_paths.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/run_app.R")
run_chico_shiny()
```

## Common error
If you see an error like:

``` r
Couldn't normalize path in `addResourcePath`
```

it usually means the app was trying to read `inst/www` via `system.file()` before the package was installed,
or the working directory is not the repository root.
