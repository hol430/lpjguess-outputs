.runner_dimension_labels <- function(choices) {
    labels <- c(column = "Column", instruction_file = "Instruction file",
        simulation = "Simulation", output = "Output", run_id = "Run",
        patch = "Patch", stand = "Stand", pft = "PFT", indiv = "Individual", layer = "Soil layer")
    titles <- unname(labels[choices])
    parameters <- startsWith(choices, "param:")
    titles[parameters] <- paste0("Parameter: ", sub("^param:", "", choices[parameters]))
    stats::setNames(choices, titles)
}

.runner_filter_id <- function(parameter) {
    paste0("parameter_", paste(format(charToRaw(enc2utf8(parameter))), collapse = ""))
}

.runner_filter_choices <- function(runs, parameter) {
    values <- .runner_parameter_values(runs)[[paste0("param:", parameter)]]
    values <- sort(unique(values), na.last = TRUE)
    tokens <- vapply(values, function(value) {
        if (is.na(value)) "unset" else paste0("value_",
            paste(format(charToRaw(enc2utf8(value))), collapse = ""))
    }, character(1), USE.NAMES = FALSE)
    list(values = values, choices = c("All values" = "all",
        stats::setNames(tokens,
            ifelse(is.na(values), "Not overridden", values))))
}

.runner_plot_code <- function(runs, outputs, options, stale_threshold_secs,
                              x = NULL, catalog = runs, common_columns = NULL,
                              parameters = NULL) {
    directory <- if (is.character(x)) unname(x) else if (is.list(x) && !is.data.frame(x))
        x$output_directory else NULL
    setup <- character()
    if (!is.null(directory)) {
        # Express the selected jobs through the public filters. Remove filters
        # that add no restriction, including ones implied by parameter values.
        filters <- list(simulations = unique(runs$simulation),
            instruction_files = unique(runs$instruction_file), parameters = parameters)
        for (filter in c("simulations", "instruction_files")) {
            candidate <- filters
            candidate[filter] <- NULL
            selected <- do.call(.runner_runs, c(list(x = catalog), candidate))
            if (setequal(selected$run_id, runs$run_id)) filters <- candidate
        }
        arguments <- c(list(directory, outputs), Filter(Negate(is.null), filters))
    } else {
        # A caller-supplied table may contain arbitrary jobs or edited metadata.
        # Preserve it explicitly when no directory was supplied to the app.
        setup <- paste0("runs <- ", paste(utils::capture.output(dput(runs)), collapse = "\n"))
        arguments <- list(quote(runs), outputs)
    }
    if (length(common_columns) && setequal(options$columns, common_columns)) {
        options$columns <- NULL
    }
    options$stale_threshold_secs <- stale_threshold_secs
    defaults <- formals(plot_runner)
    for (option in names(options)) {
        default <- eval(defaults[[option]], envir = list(outputs = outputs),
                        enclos = environment(plot_runner))
        if (!identical(options[[option]], default)) arguments[option] <- options[option]
    }
    call <- as.call(c(list(quote(plot_runner)), arguments))
    paste(c("library(lpjguessOutput)", "", if (length(setup)) c(setup, ""),
            deparse(call, width.cutoff = 88L)), collapse = "\n")
}

.runner_shiny_ui <- function(x = NULL) {
    dimensions <- .runner_dimension_labels(c("column", "instruction_file", "simulation", "output"))
    path <- if (is.character(x)) x else if (is.list(x) && !is.data.frame(x)) x$output_directory else ""
    if (is.null(path)) path <- ""
    shiny::fluidPage(
        .dave_plotly_style(),
        shiny::titlePanel("Runner output explorer"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(width = 3,
                shiny::textInput("source_path", "Runner output directory", value = path),
                shiny::actionButton("open_source", "Open directory"),
                shiny::helpText("Use a directory on the machine running R that contains the runner index and successfully completed simulations."),
                shiny::selectizeInput("instruction_files", "Instruction files",
                    choices = character(), multiple = TRUE),
                shiny::selectizeInput("simulations", "Simulations",
                    choices = character(), multiple = TRUE),
                shiny::selectizeInput("filter_parameters", "Filter by parameters",
                    choices = character(), multiple = TRUE),
                shiny::uiOutput("parameter_filters"),
                shiny::selectizeInput("outputs", "Outputs shared by selected jobs",
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
                shiny::selectizeInput("facet", "Facet by", dimensions,
                    selected = c("instruction_file", "output", "column"), multiple = TRUE,
                    options = list(items = as.list(c("instruction_file", "output", "column")))),
                shiny::selectInput("colour", "Colour by", dimensions, selected = "simulation"),
                shiny::selectInput("yaxis", "Y-axis scales",
                    choices = c("Automatic (plot_runner default)" = "auto",
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
                    shiny::tabPanel("Jobs", shiny::tableOutput("jobs")),
                    shiny::tabPanel("Parameters", shiny::tableOutput("parameters")),
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

.runner_shiny_server <- function(x, stale_threshold_secs, refresh_interval) {
    force(x)
    force(stale_threshold_secs)
    force(refresh_interval)
    function(input, output, session) {
        source <- shiny::reactiveVal(x)
        catalog <- shiny::reactiveVal(.runner_empty_runs())
        catalog_error <- shiny::reactiveVal(NULL)
        snapshot <- shiny::reactiveVal(list(runs = .runner_empty_runs(),
                                           files = .runner_empty_inventory(), error = NULL))
        shiny::observeEvent(input$open_source, {
            path <- trimws(input$source_path)
            result <- .dave_capture(.runner_runs(path))
            if (!is.null(result$error)) {
                shiny::showNotification(result$error, type = "error", duration = NULL)
            } else {
                source(path)
            }
        })
        shiny::observe({
            if (refresh_interval > 0) shiny::invalidateLater(refresh_interval, session)
            input$refresh
            result <- if (is.null(source())) list(value = .runner_empty_runs(), error = NULL) else
                .dave_capture(.runner_runs(source()))
            catalog_error(result$error)
            catalog(if (is.null(result$error)) result$value else .runner_empty_runs())
        })
        shiny::observeEvent(catalog(), {
            runs <- catalog()
            instructions <- unique(runs$instruction_file)
            simulations <- unique(runs$simulation)
            parameters <- attr(runs, "runner_parameter_names")
            shiny::updateSelectizeInput(session, "instruction_files", choices = instructions,
                selected = .dave_keep_selection(shiny::isolate(input$instruction_files), instructions, instructions))
            shiny::updateSelectizeInput(session, "simulations", choices = simulations,
                selected = .dave_keep_selection(shiny::isolate(input$simulations), simulations, simulations))
            shiny::updateSelectizeInput(session, "filter_parameters", choices = parameters,
                selected = intersect(shiny::isolate(input$filter_parameters), parameters))
        }, ignoreNULL = FALSE)

        output$parameter_filters <- shiny::renderUI({
            runs <- catalog()
            parameters <- intersect(input$filter_parameters, attr(runs, "runner_parameter_names"))
            shiny::tagList(lapply(parameters, function(parameter) {
                choices <- .runner_filter_choices(runs, parameter)$choices
                id <- .runner_filter_id(parameter)
                selected <- .dave_keep_selection(shiny::isolate(input[[id]]),
                    unname(choices), "all")
                shiny::selectizeInput(id, paste(parameter, "values"), choices,
                    selected = selected, multiple = TRUE)
            }))
        })
        parameter_filter <- shiny::reactive({
            filters <- list()
            for (parameter in intersect(input$filter_parameters, attr(catalog(), "runner_parameter_names"))) {
                selected <- input[[.runner_filter_id(parameter)]]
                if (!length(selected) || "all" %in% selected) next
                choices <- .runner_filter_choices(catalog(), parameter)
                indices <- match(selected, unname(choices$choices[-1L]))
                shiny::validate(shiny::need(!anyNA(indices), "Refresh the selected parameter values."))
                filters[[parameter]] <- choices$values[indices]
            }
            if (length(filters)) filters else NULL
        })
        selected_runs <- shiny::reactive({
            runs <- catalog()
            instructions <- input$instruction_files
            simulations <- input$simulations
            if (!length(instructions) || !length(simulations) ||
                !all(instructions %in% runs$instruction_file) ||
                !all(simulations %in% runs$simulation)) return(.runner_empty_runs())
            .runner_runs(runs, simulations, instructions, parameter_filter())
        })
        shiny::observe({
            if (refresh_interval > 0) shiny::invalidateLater(refresh_interval, session)
            input$refresh
            runs <- selected_runs()
            result <- .dave_capture(.runner_inventory(runs, stale_threshold_secs))
            snapshot(list(runs = runs, files = if (is.null(result$error)) result$value else
                .runner_empty_inventory(), error = result$error))
        })
        output_choices <- shiny::reactive({
            current <- snapshot()
            .runner_common_outputs(current$files, current$runs)
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
                shiny::need(is.null(catalog_error()), catalog_error()),
                shiny::need(nrow(catalog()) > 0L, "Open an output directory containing runner simulations."),
                shiny::need(nrow(current$runs) > 0L, "Select instruction files, simulations, and parameter values matching at least one job."),
                shiny::need(is.null(current$error), current$error),
                shiny::need(length(output_choices()) > 0L, "No fresh outputs are shared by all selected jobs."),
                shiny::need(length(input$outputs) && all(input$outputs %in% output_choices()),
                            "Select one or more shared outputs."))
            list(runs = current$runs, outputs = input$outputs)
        })
        read_result <- shiny::reactive({
            chosen <- selection()
            .dave_capture(read_runner(chosen$runs, chosen$outputs,
                                      stale_threshold_secs = stale_threshold_secs))
        })
        data <- shiny::reactive({
            result <- read_result()
            shiny::validate(shiny::need(is.null(result$error), result$error))
            result$value
        })
        column_sets <- shiny::reactive(.runner_column_sets(data()))
        column_choices <- shiny::reactive(column_sets()$all)
        default_columns <- shiny::reactive({
            common <- column_sets()$common
            if (length(common)) common else column_choices()
        })
        shiny::observeEvent(column_choices(), {
            shiny::updateSelectizeInput(session, "columns", choices = column_choices(),
                selected = .dave_keep_selection(shiny::isolate(input$columns), column_choices(), default_columns()))
        })
        dimensions <- shiny::reactive(.runner_dimensions(data()))
        shiny::observeEvent(dimensions(), {
            choices <- dimensions()
            named <- .runner_dimension_labels(choices)
            shiny::updateSelectizeInput(session, "facet", choices = named,
                selected = .dave_keep_selection(shiny::isolate(input$facet), choices,
                    c("instruction_file", "output", "column")))
            shiny::updateSelectInput(session, "colour", choices = named,
                selected = .dave_keep_selection(shiny::isolate(input$colour), choices, "simulation"))
        })
        plot_options <- shiny::reactive({
            chosen <- selection()
            shiny::req(input$column_mode, input$yaxis)
            columns <- switch(input$column_mode,
                common = default_columns(), all = column_choices(), selected = input$columns)
            shiny::validate(
                shiny::need(length(columns) && all(columns %in% column_choices()), "Select one or more data columns."),
                shiny::need(length(input$facet) && all(input$facet %in% dimensions()),
                            "Select one or more available faceting dimensions."),
                shiny::need(length(input$colour) == 1L && input$colour %in% dimensions(),
                            "Select an available colour dimension."))
            list(columns = columns, common_yaxis = switch(input$yaxis,
                    shared = TRUE, free = FALSE, length(chosen$outputs) > 1L),
                 facet = input$facet, colour = input$colour)
        })
        plot_result <- shiny::reactive({
            options <- plot_options()
            .dave_capture(.runner_plot(data.table::copy(data()), options$columns,
                options$common_yaxis, options$facet, options$colour))
        })
        plot <- shiny::reactive({
            result <- plot_result()
            shiny::validate(shiny::need(is.null(result$error), result$error))
            result$value
        })
        interactive_result <- shiny::reactive(.dave_capture(.dave_interactive_plot(plot())))
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
            message <- if (!is.null(catalog_error())) catalog_error() else
                paste(nrow(current$runs), "selected job(s),", length(output_choices()),
                      "shared fresh output(s).", sum(current$files$status != "Available"),
                      "file(s) excluded; see Jobs, Parameters, and Files.")
            shiny::div(class = "alert alert-info", message)
        })
        output$warnings <- shiny::renderUI({
            selection()
            warnings <- unique(c(read_result()$warnings, plot_result()$warnings, interactive_result()$warnings))
            if (length(warnings)) shiny::div(class = "alert alert-warning",
                shiny::tags$ul(lapply(warnings, shiny::tags$li)))
        })
        output$plot <- plotly::renderPlotly(interactive_plot())
        output$preview <- shiny::renderTable(utils::head(plotted_data(), 100L))
        output$jobs <- shiny::renderTable({
            selected_runs()[c("instruction_file", "simulation", "run_id")]
        })
        output$parameters <- shiny::renderTable(.runner_factor_table(selected_runs()))
        output$files <- shiny::renderTable({
            files <- snapshot()$files
            files$modified <- as.character(as.POSIXct(files$mtime, origin = "1970-01-01"))
            files[c("instruction_file", "simulation", "output", "file", "path", "size", "modified", "status")]
        })
        output$code <- shiny::renderText({
            chosen <- selection()
            .runner_plot_code(chosen$runs, chosen$outputs, plot_options(), stale_threshold_secs,
                x = source(), catalog = catalog(), common_columns = column_sets()$common,
                parameters = parameter_filter())
        })
        output$download_png <- shiny::downloadHandler(
            filename = function() "runner-plot.png",
            content = function(file) ggplot2::ggsave(file, plot = plot(), device = "png",
                                                    width = 12, height = 8, dpi = 150))
        output$download_pdf <- shiny::downloadHandler(
            filename = function() "runner-plot.pdf",
            content = function(file) ggplot2::ggsave(file, plot = plot(), device = "pdf",
                                                    width = 12, height = 8))
        output$download_data <- shiny::downloadHandler(
            filename = function() "runner-data.csv",
            content = function(file) data.table::fwrite(plotted_data(), file))
    }
}

#' Explore runner-managed outputs in a Shiny app.
#'
#' Select instruction files, simulations, and parameter overrides, then use the
#' plotting controls of [plot_runner()]. The app shows actual selected jobs,
#' parameter metadata, and file statuses, and exports plots, CSV, and R code.
#'
#' @inheritParams read_runner
#' @param x Optional runner output directory, settings object, `lpjguess_result`,
#'   or simulation table. With `NULL`, choose an output directory in the app.
#' @inheritParams dave_shiny
#' @param launch.browser Whether to open the app in the default browser.
#'   Defaults to `interactive()`.
#' @details Freshness is calculated independently for each simulation and
#'   instruction-file job, including files not currently selected for plotting.
#'   Only outputs available in every selected job are offered. Jobs are assumed
#'   successfully completed; no completion checks are inferred from the catalog.
#'   Refreshes rediscover the index when `x` is a directory, settings, or result;
#'   a supplied simulation table retains its explicit set of jobs. Unchanged
#'   snapshots reuse cached data. Changing plot controls does not reread files.
#'   Requires optional packages \pkg{lpjguessRunner}, \pkg{shiny}, and
#'   \pkg{plotly} (version 4.12.0 or newer).
#' @return A `shiny.appobj`; print it to launch the explorer.
#' @seealso [read_runner()], [plot_runner()], [runner_parameters()]
#' @examples
#' \dontrun{
#' runner_shiny("/path/to/runner-output")
#' runner_shiny()
#' }
#' @export
runner_shiny <- function(x = NULL, stale_threshold_secs = 10,
                         refresh_interval = 5000, launch.browser = interactive()) {
    .require_runner()
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Install shiny to use runner_shiny(): install.packages(\"shiny\")", call. = FALSE)
    }
    if (!requireNamespace("plotly", quietly = TRUE) || utils::packageVersion("plotly") < "4.12.0") {
        stop("Install plotly >= 4.12.0 to use runner_shiny(): install.packages(\"plotly\")", call. = FALSE)
    }
    .runner_check_threshold(stale_threshold_secs)
    .validate_scalar(refresh_interval, "refresh_interval")
    if (!is.null(x)) .runner_runs(x)
    shiny::shinyApp(.runner_shiny_ui(x), .runner_shiny_server(x, stale_threshold_secs, refresh_interval),
                   options = list(launch.browser = launch.browser))
}
