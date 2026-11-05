# lpjguessOutput

Read, plot, and explore LPJ-GUESS outputs from DAVE OzFlux site simulations
and simulations by `lpjguessRunner`.

## Installation

Install the latest published release from GitHub:

```r
install.packages("remotes")  # Only needed once
install.packages(c("shiny", "plotly"))  # If you want the Shiny explorer
remotes::install_github("hol430/lpjguess-outputs@*release")
```

Runner support requires the optional
[`lpjguessRunner` package](https://github.com/hie-dave/lpjg-gui/tree/master/r).
The existing DAVE APIs can be used independently.

Run the same command again to update to a newer release.

## Documentation

Consult the package vignettes:

```r
browseVignettes("lpjguessOutput")
```
