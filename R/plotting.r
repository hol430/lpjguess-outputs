
# Get the variables (ie data columns) which are present in all outputs in the
# data table.
.get_common_vars <- function(dt) {
    outputs <- unique(dt$output)

    cols <- unique(dt$variable)

    # Filter down to only those columns that are present in all outputs.
    cols <- cols[sapply(cols, function(col) {
        all(sapply(outputs, function(out) {
            col %in% unique(dt[output == out]$variable)
        }))
    })]

    if (length(cols) == 0) {
        stop("Cannot decide which column to plot: no columns are present in ",
             "all outputs. Per-output columns: ",
             paste(sapply(outputs, function(out) {
                 paste0(out, ": ", paste(unique(dt[output == out]$variable),
                                         collapse = ", "))
             }), collapse = "; "))
    }

    return(cols)
}

.plot <- function(dt, vars = NULL, common_yaxis = TRUE) {
    if (is.null(vars)) {
        vars <- .get_common_vars(dt)
    } else {
        # Check that all specified columns exist in the data.
        missing_vars <- setdiff(vars, unique(dt$variable))
        if (length(missing_vars) > 0) {
            stop("The following columns are not present in the data: ",
                 paste(missing_vars, collapse = ", "))
        }
    }

    # Extract subset of table which contains only variable == columns.
    dt <- dt[variable %in% vars]

    .get_ylab <- function(output) {
        print(paste0(".get_ylab: output = ", output))
        metadata <- get_output_metadata(paste0("file_", output))
        if (is.null(metadata)) {
            warning("No metadata found for output: ", output)
            return(output)
        }
        units <- metadata@units
        if (units == "") {
            return(output)
        }
        return(paste0(metadata@name, " (", units, ")"))
    }

    scales <- if (common_yaxis) "fixed" else "free_y"
    plt <- ggplot2::ggplot(dt, ggplot2::aes(x = Date, y = value,
                                            color = variable)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~site + output, scales = scales,
                            labeller = ggplot2::labeller(output = .get_ylab)) +
        ggplot2::labs(x = "Date",
                      y = NULL,
                      color = "Variable") +
        ggplot2::theme_bw()

    plt
}

#' Plot outputs from a DAVE repository.
#'
#' @param repo Path(s) to one or more DAVE repositories.
#' @param output Output name(s) to plot.
#' @param site Site name(s) to plot.
#' @param columns Optional vector of column names to plot. If NULL, all columns
#'        will be plotted.
#'
#' @export
plot_dave <- function(repos, outputs, sites, columns = NULL) {
    data <- read_dave(repos, outputs, sites)

    .plot(data, columns)
}
