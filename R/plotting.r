
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

.plot_label_with_units <- function(name, units) {
    if (is.na(units) || !nzchar(units) || units == "-") {
        return(name)
    }
    paste0(name, " (", units, ")")
}

.disambiguate_plot_labels <- function(labels) {
    duplicate <- duplicated(unname(labels)) |
        duplicated(unname(labels), fromLast = TRUE)
    labels[duplicate] <- paste0(labels[duplicate], " [",
                                names(labels)[duplicate], "]")
    labels
}

.lookup_plot_labels <- function(values, labels) {
    values <- as.character(values)
    result <- unname(labels[values])
    result[is.na(result)] <- values[is.na(result)]
    result
}

.plot_dimension_labels <- function(values, dimension) {
    prefixes <- c(layer = "Layer", patch = "Patch", stand = "Stand",
                  indiv = "Individual")
    values <- as.character(values)
    if (!(dimension %in% names(prefixes))) {
        return(values)
    }
    result <- paste(prefixes[[dimension]], values)
    result[is.na(values)] <- NA_character_
    result
}

.get_soil_layer_labels <- function(dt) {
    if (!("layer" %in% names(dt)) ||
            !all(unique(as.character(dt$output)) %in% .SOIL_LAYER_OUTPUTS)) {
        return(NULL)
    }

    layers <- unique(as.character(dt$layer))
    layers <- layers[!is.na(layers)]
    labels <- setNames(vapply(layers, .soil_layer_number, character(1)), layers)

    # New soil state files carry the layer bounds on every row. Older files
    # have no bounds, so their labels remain layer numbers.
    state <- if (all(c("depth_top", "depth_bottom") %in% names(dt))) {
        unique(dt[output == "dave_soil_state",
                  .(layer, depth_top, depth_bottom)])
    } else {
        NULL
    }

    # Soil properties records the same geometry as data columns.
    properties <- unique(dt[output == "dave_soil_properties" &
                            variable %in% c("thickness", "depth_bot"),
                            .(layer, variable, value)])

    varying_layers <- character(0)
    for (layer_id in layers) {
        tops <- bottoms <- numeric(0)
        if (!is.null(state)) {
            state_rows <- state[as.character(layer) == layer_id]
            tops <- c(tops, unique(state_rows$depth_top))
            bottoms <- c(bottoms, unique(state_rows$depth_bottom))
        }

        property_rows <- properties[as.character(layer) == layer_id]
        if (nrow(property_rows) > 0L) {
            thickness <- unique(property_rows[
                as.character(property_rows$variable) == "thickness"]$value)
            bottom <- unique(property_rows[
                as.character(property_rows$variable) == "depth_bot"]$value)
            if (length(thickness) == 1L && length(bottom) == 1L) {
                tops <- c(tops, bottom - thickness)
                bottoms <- c(bottoms, bottom)
            } else {
                tops <- c(tops, NA_real_)
                bottoms <- c(bottoms, NA_real_)
            }
        }

        if (length(tops) == 0L) next
        if (length(unique(tops)) == 1L && length(unique(bottoms)) == 1L &&
                is.finite(tops[[1]]) && is.finite(bottoms[[1]]) &&
                tops[[1]] >= 0 && bottoms[[1]] > tops[[1]]) {
            labels[[layer_id]] <- .soil_layer_label(layer_id, tops[[1]],
                                                    bottoms[[1]])
        } else {
            varying_layers <- c(varying_layers, layer_id)
        }
    }
    if (length(varying_layers) > 0L) {
        warning("Soil layer depths vary or are unavailable for layers: ",
                paste(varying_layers, collapse = ", "),
                "; showing layer numbers", call. = FALSE)
    }
    labels
}

.get_plot_label_maps <- function(dt, facet, colour) {
    pairs <- unique(dt[, .(output, variable)])
    pairs$output <- as.character(pairs$output)
    pairs$variable <- as.character(pairs$variable)
    outputs <- unique(pairs$output)
    variables <- unique(pairs$variable)

    metadata <- setNames(lapply(outputs, function(output) {
        get_output_metadata(paste0("file_", output))
    }), outputs)
    missing_metadata <- outputs[vapply(metadata, is.null, logical(1))]
    if (length(missing_metadata) > 0) {
        warning("No metadata found for outputs: ",
                paste(missing_metadata, collapse = ", "),
                "; using raw names", call. = FALSE)
    }

    output_names <- setNames(vapply(outputs, function(output) {
        meta <- metadata[[output]]
        if (is.null(meta) || !nzchar(meta@name)) output else meta@name
    }, character(1)), outputs)

    pairs$variable_name <- vapply(seq_len(nrow(pairs)), function(i) {
        output <- pairs$output[[i]]
        variable <- pairs$variable[[i]]
        if (is.null(metadata[[output]])) variable else
            get_display_label(output, variable)
    }, character(1))
    pairs$units <- vapply(seq_len(nrow(pairs)), function(i) {
        output <- pairs$output[[i]]
        if (is.null(metadata[[output]])) return("")
        units <- get_display_units(paste0(output, ".out"), pairs$variable[[i]])
        if (is.null(units) || is.na(units)) "" else units
    }, character(1))

    output_units <- lapply(outputs, function(output) {
        unique(pairs$units[pairs$output == output])
    })
    mixed_outputs <- outputs[lengths(output_units) > 1L]
    output_labels <- setNames(vapply(seq_along(outputs), function(i) {
        units <- output_units[[i]]
        .plot_label_with_units(output_names[[i]],
                               if (length(units) == 1L) units else "")
    }, character(1)), outputs)

    variable_names <- lapply(variables, function(variable) {
        unique(pairs$variable_name[pairs$variable == variable])
    })
    variable_units <- lapply(variables, function(variable) {
        unique(pairs$units[pairs$variable == variable])
    })
    mixed_names <- variables[lengths(variable_names) > 1L]
    mixed_units <- variables[lengths(variable_units) > 1L]
    variable_labels <- setNames(vapply(seq_along(variables), function(i) {
        name <- if (length(variable_names[[i]]) == 1L)
            variable_names[[i]] else variables[[i]]
        units <- if (length(variable_units[[i]]) == 1L)
            variable_units[[i]] else ""
        .plot_label_with_units(name, units)
    }, character(1)), variables)

    pairs$label <- mapply(.plot_label_with_units, pairs$variable_name,
                          pairs$units, USE.NAMES = FALSE)
    for (output in outputs) {
        indices <- which(pairs$output == output)
        labels <- setNames(pairs$label[indices], pairs$variable[indices])
        pairs$label[indices] <- unname(.disambiguate_plot_labels(labels))
    }
    pairs$key <- paste(pairs$output, pairs$variable, sep = "\r")

    output_needs_global_label <- colour == "output" ||
        ("output" %in% facet && !("variable" %in% facet))
    variable_needs_global_label <- colour == "variable" ||
        ("variable" %in% facet && !("output" %in% facet))
    if (output_needs_global_label && length(mixed_outputs) > 0) {
        warning("Multiple units for plotted columns in outputs: ",
                paste(mixed_outputs, collapse = ", "),
                "; omitting units from output labels", call. = FALSE)
    }
    if (variable_needs_global_label && length(mixed_names) > 0) {
        warning("Conflicting column names across outputs for: ",
                paste(mixed_names, collapse = ", "),
                "; using raw column names", call. = FALSE)
    }
    if (variable_needs_global_label && length(mixed_units) > 0) {
        warning("Multiple units across outputs for columns: ",
                paste(mixed_units, collapse = ", "),
                "; omitting units from column labels", call. = FALSE)
    }

    list(output = .disambiguate_plot_labels(output_labels),
         variable = .disambiguate_plot_labels(variable_labels),
         output_name = .disambiguate_plot_labels(output_names),
         pairs = pairs)
}

.plot <- function(dt, vars, common_yaxis, facet, colour) {
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

    layer_labels <- if ("layer" %in% c(facet, colour))
        .get_soil_layer_labels(dt) else NULL

    # Extract subset of table which contains only variable == columns.
    dt <- dt[variable %in% vars]
    label_maps <- .get_plot_label_maps(dt, facet, colour)
    series_cols <- intersect(c("repo", "site", "output", "variable", "Lon",
                               "Lat", "stand", "patch", "pft", "indiv",
                               "layer"), names(dt))
    dt[, .plot_series := .GRP, by = series_cols]

    .facet_label <- function(labels) {
        if (all(c("output", "variable") %in% names(labels))) {
            keys <- paste(labels$output, labels$variable, sep = "\r")
            indices <- match(keys, label_maps$pairs$key)
            pair_labels <- label_maps$pairs$label[indices]
            missing <- is.na(pair_labels)
            pair_labels[missing] <- .lookup_plot_labels(labels$variable[missing],
                                                        label_maps$variable)
            labels$output <- .lookup_plot_labels(labels$output,
                                                 label_maps$output_name)
            labels$variable <- pair_labels
        } else if ("output" %in% names(labels)) {
            labels$output <- .lookup_plot_labels(labels$output, label_maps$output)
        } else if ("variable" %in% names(labels)) {
            labels$variable <- .lookup_plot_labels(labels$variable,
                                                  label_maps$variable)
        }
        for (dimension in intersect(c("layer", "patch", "stand", "indiv"),
                                    names(labels))) {
            labels[[dimension]] <- if (dimension == "layer" &&
                                     !is.null(layer_labels)) {
                .lookup_plot_labels(labels[[dimension]], layer_labels)
            } else {
                .plot_dimension_labels(labels[[dimension]], dimension)
            }
        }
        ggplot2::label_value(labels)
    }
    class(.facet_label) <- "labeller"

    colour_titles <- c(variable = "Column", site = "Site", output = "Output",
                       patch = "Patch", stand = "Stand", pft = "PFT",
                       indiv = "Individual", layer = "Layer")
    scales <- if (common_yaxis) "fixed" else "free_y"
    plt <- ggplot2::ggplot(dt, ggplot2::aes(x = Date, y = value,
                                            color = as.factor(!!ggplot2::sym(colour)),
                                            group = .plot_series)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(facet, scales = scales, labeller = .facet_label) +
        ggplot2::labs(x = "Date",
                      y = NULL,
                      color = unname(colour_titles[[colour]])) +
        ggplot2::theme_bw()

    colour_labels <- if (colour %in% c("output", "variable")) {
        label_maps[[colour]]
    } else if (colour == "layer") {
        layer_labels
    } else {
        NULL
    }
    plt <- plt + ggplot2::scale_colour_discrete(labels = function(breaks) {
        if (!is.null(colour_labels)) {
            return(.lookup_plot_labels(breaks, colour_labels))
        }
        .plot_dimension_labels(breaks, colour)
    })

    plt
}

.factors <- c("column", "site", "output", "patch", "stand", "pft", "indiv",
              "layer")
.sanitise_factor <- function(factor, allow_multiple = FALSE) {
    res <- match.arg(factor, .factors, several.ok = allow_multiple)

    # Remap "column" to "variable". Column makes more sense for users, but
    # variable is used internally.
    res[res == "column"] <- "variable"

    return(res)
}

#' Plot OzFlux site outputs from a DAVE repository.
#'
#' @param repos Path(s) to top-level LPJ-GUESS DAVE repositories.
#' @param outputs Output file stem(s), such as `"dave_lai"`.
#' @param sites OzFlux site directory name(s), such as `"CumberlandPlain"`.
#' @param columns Optional data column names to plot. If `NULL`, columns common
#'   to all requested outputs are plotted.
#' @param common_yaxis If TRUE, all plots will share a common y-axis scale.
#' @param facet One or more faceting dimensions.
#' @param colour One colouring dimension.
#'
#' Valid factors are: "column", "site", "output", "patch", "stand", "pft",
#' "indiv", "layer".
#'
#' @return a ggplot2 object.
#' @seealso [read_dave()], `vignette("plotting-dave")`
#'
#' @export
plot_dave <- function(repos,
                      outputs,
                      sites,
                      columns = NULL,
                      common_yaxis = length(outputs) > 1,
                      facet = c("site", "output"),
                      colour = "column") {
    data <- read_dave(repos, outputs, sites)
    colour <- .sanitise_factor(colour)
    facet <- .sanitise_factor(facet, allow_multiple = TRUE)
    .plot(data, columns, common_yaxis, facet, colour)
}
