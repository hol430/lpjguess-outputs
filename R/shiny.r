# Filesystem discovery deliberately uses file metadata, not the quantity
# registry: custom outputs must remain selectable without registered metadata.
.dave_sources <- function(repos) {
    if (!is.character(repos) || anyNA(repos) || any(!nzchar(repos))) {
        stop("repos must be a character vector of repository paths.", call. = FALSE)
    }
    paths <- vapply(repos, .sanitise_repo, character(1), USE.NAMES = FALSE)
    labels <- names(repos)
    if (is.null(labels)) labels <- rep("", length(paths))
    labels[is.na(labels)] <- ""
    labels[!nzchar(labels)] <- basename(paths[!nzchar(labels)])
    duplicate <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    labels[duplicate] <- paste0(labels[duplicate], " [", paths[duplicate], "]")
    keep <- !duplicated(paths)
    stats::setNames(paths[keep], labels[keep])
}

.dave_common_sites <- function(repos) {
    if (!length(repos)) return(character())
    sites <- lapply(repos, function(repo) {
        root <- file.path(repo, "benchmarks", "ozflux")
        dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
        basename(dirs[dir.exists(file.path(dirs, "out"))])
    })
    sort(Reduce(intersect, sites))
}

.dave_empty_inventory <- function() {
    data.frame(repo = character(), site = character(), output = character(),
               path = character(), size = numeric(), mtime = numeric(),
               status = character(), stringsAsFactors = FALSE)
}

.dave_inventory <- function(repos, sites, stale_threshold_secs) {
    result <- list(.dave_empty_inventory())
    for (repo in repos) {
        for (site in sites) {
            directory <- file.path(repo, "benchmarks", "ozflux", site, "out")
            files <- list.files(directory, pattern = "\\.out$", full.names = TRUE)
            info <- file.info(files)
            keep <- !is.na(info$isdir) & !info$isdir & !is.na(info$mtime)
            files <- files[keep]
            info <- info[keep, , drop = FALSE]
            if (!length(files)) next
            age <- as.numeric(difftime(max(info$mtime), info$mtime, units = "secs"))
            status <- ifelse(age <= stale_threshold_secs, "Available", "Stale")
            status[info$size == 0] <- "Empty"
            status[file.access(files, 4L) != 0L] <- "Unreadable"
            result[[length(result) + 1L]] <- data.frame(
                repo = repo, site = site,
                output = sub("\\.out$", "", basename(files)), path = files,
                size = info$size, mtime = as.numeric(info$mtime), status = status,
                stringsAsFactors = FALSE)
        }
    }
    do.call(rbind, result)
}

.dave_common_outputs <- function(inventory, repos, sites) {
    if (!length(repos) || !length(sites)) return(character())
    outputs <- list()
    for (repo in repos) {
        for (site in sites) {
            keep <- inventory$repo == repo & inventory$site == site &
                inventory$status == "Available"
            outputs[[length(outputs) + 1L]] <- inventory$output[keep]
        }
    }
    sort(Reduce(intersect, outputs))
}

.dave_output_choices <- function(outputs) {
    groups <- vapply(outputs, function(output) {
        metadata <- guess_metadata_from_filename(paste0(output, ".out"))
        if (is.null(metadata) || !nzchar(trimws(metadata@name))) {
            "Other outputs"
        } else metadata@name
    }, character(1))
    labels <- vapply(outputs, function(output) {
        label <- get_display_name(paste0(output, ".out"))
        if (is.null(label)) output else paste0(label, " [", output, "]")
    }, character(1))
    split(stats::setNames(outputs, labels), groups)
}

.dave_keep_selection <- function(selected, choices, default = utils::head(choices, 1L)) {
    keep <- intersect(selected, choices)
    if (length(keep)) keep else default
}

# Retain warnings alongside the result so they can be shown in the app.
.dave_capture <- function(expr) {
    warnings <- character()
    error <- NULL
    value <- tryCatch(withCallingHandlers(expr, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    }), error = function(e) {
        error <<- conditionMessage(e)
        NULL
    })
    list(value = value, warnings = unique(warnings), error = error)
}

.dave_plot_code <- function(repos, sites, outputs, columns, common_yaxis,
                            facet, colour) {
    arguments <- list(repos = unname(repos), outputs = outputs, sites = sites,
                      columns = columns, common_yaxis = common_yaxis,
                      facet = facet, colour = colour)
    lines <- vapply(names(arguments), function(name) {
        value <- paste(utils::capture.output(dput(arguments[[name]])), collapse = "\n    ")
        paste0("    ", name, " = ", value)
    }, character(1))
    paste0("library(lpjguessOutput)\n\nplot_dave(\n", paste(lines, collapse = ",\n"), "\n)")
}

# Only the Shiny view is converted. Keep the ggplot and its data intact for
# static downloads and for callers of plot_dave().
.dave_interactive_plot <- function(plot) {
    data <- data.table::copy(plot$data)
    labels <- .get_plot_label_maps(data, character(), "site")
    pairs <- match(paste(data$output, data$variable, sep = "\r"), labels$pairs$key)
    fields <- list(Date = format(data$Date, "%Y-%m-%d %H:%M:%S %Z"),
                   Source = data$repo, Site = data$site, Output = data$output,
                   Column = labels$pairs$label[pairs],
                   Value = format(data$value, digits = 10, trim = TRUE))
    dimensions <- c(Lon = "Longitude", Lat = "Latitude", stand = "Stand",
                    patch = "Patch", pft = "PFT", indiv = "Individual",
                    layer = "Soil layer (file index)")
    for (dimension in intersect(names(dimensions), names(data))) {
        fields[[dimensions[[dimension]]]] <- data[[dimension]]
    }
    # Escape file paths and raw column names: Plotly hover text accepts HTML.
    lines <- lapply(names(fields), function(name) {
        values <- fields[[name]]
        ifelse(is.na(values), "", paste0(name, ": ", htmltools::htmlEscape(as.character(values))))
    })
    data$.dave_tooltip <- Reduce(function(text, line) {
        paste0(text, ifelse(nzchar(line), paste0("<br>", line), ""))
    }, lines)
    plot$data <- data
    plot$mapping$text <- ggplot2::aes(text = !!ggplot2::sym(".dave_tooltip"))$text
    # ggplotly gives facet strips a single-line background even when their
    # labels span several lines. Use unboxed headings and reserve space for
    # every faceting dimension, including between rows of panels.
    facet_lines <- max(1L, length(plot$facet$params$facets))
    plot <- plot + ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = "transparent"),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 9, margin = ggplot2::margin(4, 0, 4, 0)),
        panel.spacing.y = ggplot2::unit(16 * (facet_lines + 1), "pt"),
        plot.margin = ggplot2::margin(16 * facet_lines + 16, 8, 8, 8))
    widget <- plotly::ggplotly(plot, tooltip = "text", dynamicTicks = TRUE)
    widget$x$layout$shapes <- Filter(function(shape) {
        !(identical(shape$type, "rect") && identical(shape$ysizemode, "pixel"))
    }, widget$x$layout$shapes)
    # Plotly's automatic date ticks can span two lines after zooming/resizing.
    # Leave room below them for the shared x-axis title.
    widget$x$layout$annotations <- lapply(widget$x$layout$annotations, function(label) {
        if (identical(label$annotationType, "axis") && isTRUE(label$textangle == 0)) {
            label$yshift <- label$yshift - 20
        }
        label
    })
    widget$x$layout$margin$b <- widget$x$layout$margin$b + 20

    # ggplotly uses raw colour values for these traces, ignoring the label
    # function on the discrete scale. Reuse it to retain units and soil depths.
    colour_labels <- plot$scales$get_scales("colour")$labels
    widget$x$data <- lapply(widget$x$data, function(trace) {
        if (!is.null(trace$name)) {
            trace$name <- htmltools::htmlEscape(unname(colour_labels(trace$name)))
        }
        trace
    })
    widget <- plotly::layout(widget, hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(bgcolor = "rgba(0,0,0,0)", borderwidth = 0))
    plotly::config(widget, displaylogo = FALSE, scrollZoom = TRUE, responsive = TRUE)
}

.dave_plotly_style <- function() {
    # currentColor follows the page's computed foreground, including changes
    # made by extensions such as Dark Reader, without recolouring data series.
    shiny::tags$style(shiny::HTML("
        #plot .plot-container, #plot .svg-container, #plot .main-svg,
        #plot .modebar, #plot .modebar-btn {
            color: inherit;
        }
        #plot .main-svg, #plot .modebar-group {
            background: transparent !important;
        }
        #plot .xaxislayer-above text, #plot .yaxislayer-above text,
        #plot .xaxislayer-below text, #plot .yaxislayer-below text,
        #plot .annotation-text,
        #plot .legendtext, #plot .legendtitletext, #plot .gtitle,
        #plot .xtitle, #plot .ytitle {
            fill: currentColor !important;
        }
        #plot .gridlayer path {
            stroke: currentColor !important;
            stroke-opacity: 0.15 !important;
        }
        #plot .xaxislayer-above path, #plot .yaxislayer-above path,
        #plot .xaxislayer-below path, #plot .yaxislayer-below path,
        #plot .shapelayer path {
            stroke: currentColor !important;
            stroke-opacity: 0.5 !important;
        }
        #plot .modebar-btn path {
            fill: currentColor !important;
        }
    "))
}

.dave_shiny_ui <- function(repos) {
    dimensions <- c(Column = "column", Site = "site", Output = "output",
                    Source = "repo", Patch = "patch", Stand = "stand",
                    PFT = "pft", Individual = "indiv", `Soil layer` = "layer")
    shiny::fluidPage(
        .dave_plotly_style(),
        shiny::titlePanel("DAVE output explorer"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(width = 3,
                shiny::h4("Sources"),
                shiny::textAreaInput("source_paths", "Repository directories",
                    placeholder = "Paste one directory per line", rows = 3),
                shiny::actionButton("add_sources", "Add sources"),
                shiny::helpText("Use paths on the machine running R. Each repository must contain benchmarks/ozflux/settings.ins."),
                shiny::selectizeInput("sources", "Selected sources", choices = repos,
                    selected = unname(repos), multiple = TRUE),
                shiny::selectizeInput("sites", "Sites shared by all sources",
                    choices = character(), multiple = TRUE),
                shiny::selectizeInput("outputs", "Outputs shared by all sites and sources",
                    choices = character(), multiple = TRUE),
                shiny::actionButton("refresh", "Refresh files"),
                shiny::hr(),
                shiny::h4("Plot options"),
                shiny::selectInput("column_mode", "Data columns",
                    choices = c("Common columns (all if none shared)" = "common",
                                "All columns" = "all", "Choose columns" = "selected")),
                shiny::conditionalPanel("input.column_mode == 'selected'",
                    shiny::selectizeInput("columns", "Choose data columns",
                        choices = character(), multiple = TRUE)),
                shiny::helpText("Common columns are shared by every selected file. Choose columns to compare particular variables from different outputs."),
                shiny::selectizeInput("facet", "Facet by", dimensions,
                    selected = if (length(repos) > 1L) c("repo", "site", "output") else c("site", "output"),
                    multiple = TRUE),
                shiny::selectInput("colour", "Colour by", dimensions, selected = "column"),
                shiny::selectInput("yaxis", "Y-axis scales",
                    choices = c("Automatic (plot_dave default)" = "auto",
                                "Shared" = "shared", "Independent" = "free"))
            ),
            shiny::mainPanel(width = 9,
                shiny::uiOutput("availability"),
                shiny::uiOutput("warnings"),
                shiny::tabsetPanel(
                    shiny::tabPanel("Plot",
                        shiny::helpText("Hover for details, drag to zoom, and click legend entries to show or hide series. PNG/PDF downloads contain the full static plot."),
                        plotly::plotlyOutput("plot", height = "720px"),
                        shiny::downloadButton("download_png", "Download PNG"),
                        shiny::downloadButton("download_pdf", "Download PDF"),
                        shiny::downloadButton("download_data", "Download CSV")),
                    shiny::tabPanel("Data preview",
                        shiny::helpText("First 100 plotted rows. CSV downloads contain all plotted rows."),
                        shiny::tableOutput("preview")),
                    shiny::tabPanel("Files", shiny::tableOutput("files")),
                    shiny::tabPanel("R code", shiny::verbatimTextOutput("code"))
                )
            )
        )
    )
}

.dave_shiny_server <- function(repos, stale_threshold_secs, refresh_interval) {
    force(repos)
    force(stale_threshold_secs)
    force(refresh_interval)
    function(input, output, session) {
        sources <- shiny::reactiveVal(repos)
        site_choices <- shiny::reactiveVal(character())
        snapshot <- shiny::reactiveVal(list(repos = character(), sites = character(),
                                           files = .dave_empty_inventory()))
        selected_repos <- shiny::reactive({
            intersect(input$sources, unname(sources()))
        })
        shiny::observeEvent(input$add_sources, {
            paths <- trimws(strsplit(input$source_paths, "\n", fixed = TRUE)[[1L]])
            paths <- paths[nzchar(paths)]
            if (!length(paths)) return()
            added <- .dave_capture(.dave_sources(paths))
            if (!is.null(added$error)) {
                shiny::showNotification(added$error, type = "error", duration = NULL)
                return()
            }
            combined <- .dave_sources(c(sources(), added$value))
            selected <- union(input$sources, unname(added$value))
            sources(combined)
            shiny::updateSelectizeInput(session, "sources", choices = combined,
                                        selected = selected)
            shiny::updateTextAreaInput(session, "source_paths", value = "")
        })

        # reactiveVal suppresses identical snapshots: polling does not reread
        # large output files or reset controls when nothing has changed.
        shiny::observe({
            if (refresh_interval > 0) shiny::invalidateLater(refresh_interval, session)
            input$refresh
            current_repos <- selected_repos()
            sites <- .dave_common_sites(current_repos)
            site_choices(sites)
            selected_sites <- intersect(input$sites, sites)
            files <- .dave_inventory(current_repos, selected_sites, stale_threshold_secs)
            snapshot(list(repos = current_repos, sites = selected_sites, files = files))
        })

        shiny::observeEvent(site_choices(), {
            shiny::updateSelectizeInput(session, "sites", choices = site_choices(),
                selected = .dave_keep_selection(shiny::isolate(input$sites), site_choices()))
        }, ignoreNULL = FALSE)

        output_choices <- shiny::reactive({
            current <- snapshot()
            .dave_common_outputs(current$files, current$repos, current$sites)
        })
        shiny::observeEvent(output_choices(), {
            choices <- output_choices()
            default <- if ("dave_lai" %in% choices) "dave_lai" else utils::head(choices, 1L)
            shiny::updateSelectizeInput(session, "outputs", choices = .dave_output_choices(choices),
                selected = .dave_keep_selection(shiny::isolate(input$outputs), choices, default))
        }, ignoreNULL = FALSE)

        selection <- shiny::reactive({
            current <- snapshot()
            shiny::validate(
                shiny::need(length(current$repos) > 0L, "Add and select at least one source directory."),
                shiny::need(length(site_choices()) > 0L, "No sites with output directories are shared by all selected sources."),
                shiny::need(length(input$sites) && all(input$sites %in% site_choices()),
                            "Select one or more shared sites."),
                shiny::need(identical(current$repos, selected_repos()) &&
                            setequal(current$sites, input$sites), "Refreshing available files..."),
                shiny::need(length(output_choices()) > 0L, "No fresh outputs are shared by all selected sites and sources."),
                shiny::need(length(input$outputs) && all(input$outputs %in% output_choices()),
                            "Select one or more shared outputs."))
            list(repos = current$repos, sites = input$sites, outputs = input$outputs)
        })

        read_result <- shiny::reactive({
            chosen <- selection()
            .dave_capture(read_dave(chosen$repos, chosen$outputs, chosen$sites))
        })
        data <- shiny::reactive({
            result <- read_result()
            shiny::validate(shiny::need(is.null(result$error), result$error))
            shiny::validate(shiny::need(nrow(result$value) > 0L, "Selected output files contain no data rows."))
            chosen <- selection()
            pairs <- unique(result$value[, c("repo", "site", "output"), with = FALSE])
            expected <- length(chosen$repos) * length(chosen$sites) * length(chosen$outputs)
            shiny::validate(shiny::need(nrow(pairs) == expected,
                "One or more selected output files contain no data rows."))
            result$value
        })

        column_sets <- shiny::reactive({
            dt <- data()
            per_file <- dt[, list(columns = list(unique(as.character(get("variable"))))),
                           by = c("repo", "site", "output")]
            list(common = Reduce(intersect, per_file$columns),
                 all = sort(unique(as.character(dt$variable))))
        })
        column_choices <- shiny::reactive(column_sets()$all)
        default_columns <- shiny::reactive({
            common <- column_sets()$common
            if (length(common)) common else column_choices()
        })
        shiny::observeEvent(column_choices(), {
            choices <- column_choices()
            shiny::updateSelectizeInput(session, "columns", choices = choices,
                selected = .dave_keep_selection(shiny::isolate(input$columns), choices, default_columns()))
        })

        dimensions <- shiny::reactive({
            choices <- .factors
            mapped <- .sanitise_factor(choices, allow_multiple = TRUE)
            choices[mapped %in% names(data())]
        })
        shiny::observeEvent(dimensions(), {
            choices <- dimensions()
            labels <- c(column = "Column", site = "Site", output = "Output",
                        repo = "Source", patch = "Patch", stand = "Stand", pft = "PFT",
                        indiv = "Individual", layer = "Soil layer")
            named <- stats::setNames(choices, unname(labels[choices]))
            shiny::updateSelectizeInput(session, "facet", choices = named,
                selected = .dave_keep_selection(shiny::isolate(input$facet), choices,
                    c(if (length(selected_repos()) > 1L) "repo", "site", "output")))
            shiny::updateSelectInput(session, "colour", choices = named,
                selected = .dave_keep_selection(shiny::isolate(input$colour), choices, "column"))
        })

        plot_options <- shiny::reactive({
            selection()
            shiny::req(input$column_mode, input$yaxis)
            columns <- switch(input$column_mode,
                common = default_columns(), all = column_choices(), selected = input$columns)
            shiny::validate(
                shiny::need(length(columns) && all(columns %in% column_choices()),
                            "Select one or more data columns."),
                shiny::need(length(input$facet) && all(input$facet %in% dimensions()),
                            "Select one or more available faceting dimensions."),
                shiny::need(length(input$colour) == 1L && input$colour %in% dimensions(),
                            "Select an available colour dimension."))
            common <- switch(input$yaxis, shared = TRUE, free = FALSE,
                              length(selection()$outputs) > 1L)
            list(columns = columns, common_yaxis = common,
                 facet = input$facet, colour = input$colour)
        })
        plot_result <- shiny::reactive({
            options <- plot_options()
            # .plot adds a series column; keep the cached reader data intact.
            .dave_capture(.plot(data.table::copy(data()), options$columns,
                options$common_yaxis, .sanitise_factor(options$facet, TRUE),
                .sanitise_factor(options$colour)))
        })
        plot <- shiny::reactive({
            result <- plot_result()
            shiny::validate(shiny::need(is.null(result$error), result$error))
            result$value
        })
        interactive_result <- shiny::reactive({
            static_plot <- plot()
            .dave_capture(.dave_interactive_plot(static_plot))
        })
        interactive_plot <- shiny::reactive({
            result <- interactive_result()
            shiny::validate(shiny::need(is.null(result$error), result$error))
            result$value
        })
        plotted_data <- shiny::reactive({
            columns <- plot_options()$columns
            dt <- data()
            dt[dt$variable %in% columns]
        })

        output$availability <- shiny::renderUI({
            current <- snapshot()
            message <- if (!length(current$repos)) {
                "Add source directories, then select sources, sites, and outputs."
            } else if (!length(site_choices())) {
                "No sites with output directories are shared by all selected sources."
            } else if (!length(current$sites)) {
                "Select one or more sites shared by every selected source."
            } else {
                paste(length(current$repos), "source(s),", length(current$sites), "site(s),",
                      length(output_choices()), "shared fresh output(s).",
                      sum(current$files$status != "Available"), "stale, empty, or unreadable file(s) excluded.")
            }
            shiny::tags$p(role = "status", message)
        })
        output$warnings <- shiny::renderUI({
            shiny::req(length(input$outputs) > 0L, length(output_choices()) > 0L)
            warnings <- unique(c(read_result()$warnings, plot_result()$warnings,
                                 interactive_result()$warnings))
            if (length(warnings)) shiny::div(class = "alert alert-warning",
                shiny::tags$ul(lapply(warnings, shiny::tags$li)))
        })
        output$plot <- plotly::renderPlotly(interactive_plot())
        output$preview <- shiny::renderTable(utils::head(plotted_data(), 100L))
        output$files <- shiny::renderTable({
            files <- snapshot()$files
            files$modified <- as.character(as.POSIXct(files$mtime, origin = "1970-01-01"))
            files[c("repo", "site", "output", "size", "modified", "status")]
        })
        output$code <- shiny::renderText({
            chosen <- selection()
            options <- plot_options()
            .dave_plot_code(chosen$repos, chosen$sites, chosen$outputs, options$columns,
                            options$common_yaxis, options$facet, options$colour)
        })
        output$download_png <- shiny::downloadHandler(
            filename = function() "dave-plot.png",
            content = function(file) ggplot2::ggsave(file, plot = plot(), device = "png",
                                                    width = 12, height = 8, dpi = 150))
        output$download_pdf <- shiny::downloadHandler(
            filename = function() "dave-plot.pdf",
            content = function(file) ggplot2::ggsave(file, plot = plot(), device = "pdf",
                                                    width = 12, height = 8))
        output$download_data <- shiny::downloadHandler(
            filename = function() "dave-data.csv",
            content = function(file) data.table::fwrite(plotted_data(), file))
    }
}

#' Explore DAVE outputs in a Shiny app.
#'
#' Select repository directories, sites shared by all selected repositories,
#' and outputs shared by every selected repository/site pair. The app exposes
#' the column, facet, colour, and y-axis controls of [plot_dave()], including
#' `repo` to distinguish sources. It also provides plot and CSV downloads and
#' the equivalent R plotting call.
#'
#' @param repos Optional character vector of top-level DAVE repository paths.
#'   Names can supply source labels. More directories can be added in the app.
#' @param stale_threshold_secs Non-negative freshness window in seconds.
#'   Within each site's `out` directory, an output is stale if its modification
#'   time is more than this many seconds behind the newest `.out` file.
#'   Defaults to 10. This is a timestamp heuristic, not
#'   a guarantee that files belong to the same completed simulation.
#' @param refresh_interval Milliseconds between filesystem checks. Defaults to
#'   5000. Set to 0 to refresh only when selections change or Refresh files is
#'   clicked. Unchanged files are not reread on each check.
#'
#' @details Sources use the same directory layout as [read_dave()]. Only regular,
#'   non-empty, readable, fresh `.out` files are offered. Output choices are
#'   grouped by metadata short name, such as NPP, with temporal resolution and
#'   aggregation level shown for each option. Outputs without a metadata short
#'   name appear in Other outputs; unregistered outputs use their filenames.
#'   Compressed outputs are not supported.
#'   Directory paths refer to the machine running R. No simulation files are
#'   modified. Data is read only for selected outputs; plot control changes
#'   reuse the loaded data.
#'
#'   The app uses [plotly::ggplotly()] for hover details, zoom, pan, and legend
#'   toggling. The underlying plot, PNG/PDF downloads, and generated R code use
#'   ggplot2. Static downloads include the full plot regardless of browser zoom
#'   or hidden traces. Very large plots can be slow to convert and display.
#'
#'   Columns common to every selected file are selected initially. When no
#'   columns are shared, all columns are selected explicitly so disjoint outputs
#'   can still be compared. Optional dimensions are offered when present in the
#'   loaded data. Metadata and soil-depth labels follow [plot_dave()].
#'
#' @return A `shiny.appobj`. Print it to launch interactively, or pass it to
#'   [shiny::runApp()]. Requires the suggested packages \pkg{shiny} and
#'   \pkg{plotly} (version 4.12.0 or newer).
#' @seealso [plot_dave()], [read_dave()]
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'     dave_shiny()
#'     # dave_shiny(c(Baseline = "/path/to/dave", Experiment = "/path/to/dave2"))
#' }
#' @export
dave_shiny <- function(repos = character(), stale_threshold_secs = 10,
                       refresh_interval = 5000) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Install the shiny package to use dave_shiny(): install.packages(\"shiny\")",
             call. = FALSE)
    }
    if (!requireNamespace("plotly", quietly = TRUE) ||
            utils::packageVersion("plotly") < "4.12.0") {
        stop("Install plotly >= 4.12.0 to use dave_shiny(): install.packages(\"plotly\")",
             call. = FALSE)
    }
    repos <- .dave_sources(repos)
    for (name in c("stale_threshold_secs", "refresh_interval")) {
        value <- get(name)
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
            stop(name, " must be a finite non-negative number.", call. = FALSE)
        }
    }
    shiny::shinyApp(.dave_shiny_ui(repos),
                    .dave_shiny_server(repos, stale_threshold_secs, refresh_interval))
}
