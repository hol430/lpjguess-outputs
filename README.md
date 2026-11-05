# lpjguessOutput

Read, plot, and explore LPJ-GUESS outputs from DAVE OzFlux site simulations
and simulations by `lpjguessRunner`.

## Installation

Install the latest published release from GitHub:

```r
install.packages("remotes")  # Only needed once
install.packages(c("shiny", "plotly"))  # If you want the Shiny explorer
remotes::install_github(
    "hol430/lpjguess-outputs@*release",
    build_vignettes = TRUE,
    dependencies = TRUE)
```

Runner support requires the optional
[`lpjguessRunner` package](https://github.com/hie-dave/lpjg-gui/tree/master/r).
The existing DAVE APIs can be used independently.

Run the same command again to update to a newer release.

## Basic Usage

```r
library(lpjguessOutput)

# Plot LAI at one site (e.g. if running in repository via vscode)
repo <- "/path/to/dave"
plot_dave(repo, "dave_lai", "CumberlandPlain")

# Plot outputs from a simulation run by lpjguessRunner
run_dir <- "/path/to/out/dir"
plot_runner(run_dir, "dave_lai")

# Explore outputs in a Shiny app
dave_shiny(repo)
runner_shiny(run_dir)
```

## Documentation

Consult the package vignettes:

```r
browseVignettes("lpjguessOutput")
```
