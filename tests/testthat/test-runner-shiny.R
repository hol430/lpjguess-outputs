test_that("the runner explorer accepts directories or catalogs and validates options", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    root <- local_runner_output()
    expect_s3_class(runner_shiny(), "shiny.appobj")
    expect_s3_class(runner_shiny(root, launch.browser = FALSE), "shiny.appobj")
    expect_s3_class(runner_shiny(lpjguessRunner::list_simulations(root)), "shiny.appobj")
    expect_error(runner_shiny(stale_threshold_secs = -1), "stale_threshold_secs")
    expect_error(runner_shiny(refresh_interval = Inf), "refresh_interval")
    expect_error(runner_shiny(file.path(root, "missing")), "index")
})

test_that("the default runner plot exports a minimal directory-based call", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output(data.frame(instruction = c("SiteA", "SiteA"),
        simulation = c("baseline", "treatment")))
    shiny::testServer(.runner_shiny_server(root, 10, 0), {
        session$setInputs(instruction_files = "SiteA", simulations = c("baseline", "treatment"),
            outputs = "dave_lai", column_mode = "common", yaxis = "auto",
            facet = c("instruction_file", "output", "column"), colour = "simulation")
        expect_identical(output$code, paste0("library(lpjguessOutput)\n\nplot_runner(",
            encodeString(root, quote = '"'), ', "dave_lai")'))
        expect_equal(eval(parse(text = output$code))$data, plot()$data)
    })
    ui <- htmltools::tagQuery(.runner_shiny_ui(root))
    config <- Filter(function(tag) identical(tag$attribs[["data-for"]], "facet"),
                     ui$find("script")$selectedTags())[[1L]]
    expect_match(as.character(config),
        '"items":["instruction_file","output","column"]', fixed = TRUE)
})

test_that("the runner explorer initially selects all instruction files and simulations", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    root <- local_runner_output()
    updates <- list()
    session <- shiny::MockShinySession$new()
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    shiny::testServer(.runner_shiny_server(root, 10, 0), {
        session$flushReact()
        expect_setequal(updates$instruction_files$value, c("SiteA", "SiteB"))
        expect_setequal(updates$simulations$value, c("baseline", "treatment"))
    }, session = session)
})

test_that("runner code includes necessary job filters and non-default plot settings", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output()
    for (i in 1:3) write_runner_output(root, data.frame(Lon = 150, Lat = -33,
        Year = 2024L, Day = 0:1, total = 1:2, MRS = 3:4), i)
    shiny::testServer(.runner_shiny_server(root, 35, 0), {
        session$setInputs(instruction_files = "SiteA", simulations = "baseline",
            outputs = "dave_lai", column_mode = "selected", columns = "total", yaxis = "shared",
            facet = c("column", "instruction_file", "output"), colour = "param:pft:MRS:sla")
        call <- tail(parse(text = output$code), 1L)[[1L]]
        arguments <- as.list(call)[-1L]
        expect_identical(arguments[[1L]], root)
        expect_identical(arguments$simulations, "baseline")
        expect_identical(arguments$instruction_files, "SiteA")
        expect_identical(arguments$columns, "total")
        expect_true(arguments$common_yaxis)
        expect_identical(arguments$stale_threshold_secs, 35)
        expect_identical(eval(arguments$facet), c("column", "instruction_file", "output"))
        reproduced <- eval(call)
        expect_equal(reproduced$data, plot()$data)
        expect_equal(names(reproduced$facet$params$facets), names(plot()$facet$params$facets))
        expect_false(reproduced$facet$params$free$y)
    })
})

test_that("explicit columns are exported when the app falls back from disjoint common columns", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output(data.frame(instruction = "SiteA", simulation = "baseline"))
    write_runner_output(root, data.frame(Lon = 150, Lat = -33, Year = 2024L,
        Day = 0:1, layer = 0L, wmass = 5:6), filename = "soil.out")
    shiny::testServer(.runner_shiny_server(root, 10, 0), {
        session$setInputs(instruction_files = "SiteA", simulations = "baseline",
            outputs = c("dave_lai", "dave_soil_state"), column_mode = "common", yaxis = "auto",
            facet = c("instruction_file", "output", "column"), colour = "simulation")
        call <- tail(parse(text = output$code), 1L)[[1L]]
        arguments <- as.list(call)[-1L]
        expect_setequal(eval(arguments$columns), c("total", "wmass"))
        expect_false("common_yaxis" %in% names(arguments))
        expect_equal(eval(call)$data, plot()$data)
    })
})

test_that("settings and results export their directory while custom tables retain their data", {
    root <- local_runner_output()
    runs <- .runner_runs(root)
    options <- list(columns = "total", common_yaxis = FALSE,
        facet = c("instruction_file", "output", "column"), colour = "simulation")
    for (x in list(list(output_directory = root),
                   structure(list(output_directory = root), class = "lpjguess_result"))) {
        code <- .runner_plot_code(runs, "dave_lai", options, 10, x = x, common_columns = "total")
        expect_identical(as.list(tail(parse(text = code), 1L)[[1L]])[-1L], list(root, "dave_lai"))
        expect_equal(eval(parse(text = code))$data, plot_runner(root, "dave_lai")$data)
    }
    selected <- runs[c(1L, 3L), ]
    selected$simulation <- "custom label"
    code <- .runner_plot_code(selected, "dave_lai", options, 10,
                              x = selected, common_columns = "total")
    expect_match(code, "runs <- ", fixed = TRUE)
    expect_match(code, 'plot_runner(runs, "dave_lai")', fixed = TRUE)
    expect_equal(eval(parse(text = code))$data, plot_runner(selected, "dave_lai")$data)
})

test_that("the runner explorer caches data, preserves series and exports reproducible plots", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output()
    reads <- 0L
    original <- read_runner
    local_mocked_bindings(read_runner = function(...) {
        reads <<- reads + 1L
        original(...)
    })
    shiny::testServer(.runner_shiny_server(root, 10, 1000), {
        session$setInputs(instruction_files = c("SiteA", "SiteB"),
            simulations = c("baseline", "treatment"), outputs = "dave_lai",
            column_mode = "common", yaxis = "auto",
            facet = c("instruction_file", "column"), colour = "param:pft:MRS:sla")
        expect_equal(nrow(selected_runs()), 3L)
        expect_identical(output_choices(), "dave_lai")
        expect_equal(nrow(data()), 6L)
        expect_true(all(c("param:nindiv_max", "param:pft:MRS:sla", "simulation", "instruction_file") %in% dimensions()))
        expect_false("repo" %in% dimensions())
        expect_equal(length(unique(plot()$data$.plot_series)), 3L)
        expect_s3_class(interactive_plot(), "plotly")
        widget <- plotly::plotly_build(interactive_plot())
        hover <- paste(unlist(lapply(widget$x$data, function(trace) trace$text)), collapse = "\n")
        expect_match(hover, "Simulation: treatment", fixed = TRUE)
        expect_match(hover, "Instruction file: SiteB", fixed = TRUE)
        expect_match(hover, "pft:MRS:sla: 39", fixed = TRUE)
        expect_match(hover, "Column: total (m2/m2)", fixed = TRUE)
        expect_equal(reads, 1L)
        session$elapse(1000)
        session$setInputs(refresh = 1, colour = "simulation", yaxis = "shared")
        expect_identical(plot()$labels$colour, "Simulation")
        expect_equal(reads, 1L)
        expect_false(any(c(".plot_series", ".dave_tooltip") %in% names(plotted_data())))
        downloaded <- data.table::fread(output$download_data)
        expect_equal(nrow(downloaded), 6L)
        expect_true("param:pft:MRS:sla" %in% names(downloaded))
        expect_false(".plot_series" %in% names(downloaded))
        expect_gt(file.info(output$download_png)$size, 1000)
        expect_gt(file.info(output$download_pdf)$size, 1000)
        reproduced <- eval(parse(text = output$code))
        expect_s3_class(reproduced, "ggplot")
        expect_equal(reproduced$data, plot()$data)
        expect_match(output$jobs, "SiteB", fixed = TRUE)
        expect_match(output$parameters, "pft:MRS:sla", fixed = TRUE)
        expect_match(output$files, "custom-lai.out", fixed = TRUE)
    })
})

test_that("parameter filters select existing jobs without manufacturing combinations", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output()
    shiny::testServer(.runner_shiny_server(root, 10, 0), {
        session$setInputs(instruction_files = c("SiteA", "SiteB"),
            simulations = c("baseline", "treatment"), outputs = "dave_lai",
            column_mode = "common", yaxis = "auto", facet = "instruction_file",
            colour = "param:pft:MRS:sla", filter_parameters = "pft:MRS:sla")
        id <- .runner_filter_id("pft:MRS:sla")
        do.call(session$setInputs, stats::setNames(list("value_3339"), id))
        expect_equal(nrow(selected_runs()), 1L)
        expect_identical(selected_runs()$simulation, "treatment")
        expect_identical(selected_runs()$instruction_file, "SiteA")
        expect_equal(nrow(plot()$data), 2L)
        call <- tail(parse(text = output$code), 1L)[[1L]]
        arguments <- as.list(call)[-1L]
        expect_identical(eval(arguments$parameters), list("pft:MRS:sla" = "39"))
        expect_false(any(c("simulations", "instruction_files") %in% names(arguments)))
        expect_equal(eval(call)$data, plot()$data)
        do.call(session$setInputs, stats::setNames(list("value_3236"), id))
        expect_equal(nrow(selected_runs()), 2L)
        expect_setequal(selected_runs()$instruction_file, c("SiteA", "SiteB"))
        expect_equal(nrow(plot()$data), 4L)
        session$setInputs(filter_parameters = c("pft:MRS:sla", "nindiv_max"))
        do.call(session$setInputs, stats::setNames(list("value_31"), .runner_filter_id("nindiv_max")))
        expect_equal(nrow(selected_runs()), 0L)
        expect_error(plot(), "at least one job", class = "shiny.silent.error")
        session$setInputs(filter_parameters = character())
        expect_equal(nrow(selected_runs()), 3L)
        expect_s3_class(plot(), "ggplot")
    })
})

test_that("refresh invalidates missing or stale selections independently per job", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output()
    runs <- .runner_runs(root)
    timestamp <- as.POSIXct("2024-01-01", tz = "UTC")
    for (i in 1:3) Sys.setFileTime(file.path(runs$path[[i]], "results", "custom-lai.out"), timestamp + i * 86400)
    shiny::testServer(.runner_shiny_server(root, 10, 0), {
        session$setInputs(instruction_files = c("SiteA", "SiteB"),
            simulations = c("baseline", "treatment"), outputs = "dave_lai",
            column_mode = "common", yaxis = "auto", facet = "instruction_file", colour = "simulation")
        expect_s3_class(plot(), "ggplot")
        path <- write_runner_output(root,
            data.frame(Lon = 150, Lat = -33, Year = 2024L, wmass = 5), 2L, "soil.out")
        Sys.setFileTime(path, timestamp + 2 * 86400 + 11)
        session$setInputs(refresh = 1)
        expect_length(output_choices(), 0L)
        expect_error(plot(), "No fresh outputs", class = "shiny.silent.error")
        Sys.setFileTime(file.path(runs$path[[2]], "results", "custom-lai.out"), timestamp + 2 * 86400 + 1)
        session$setInputs(refresh = 2)
        expect_s3_class(plot(), "ggplot")
        frame <- data.frame(Lon = 150, Lat = -33, Year = 2024L, Day = 0:1, total = c(100, 200))
        write_runner_output(root, frame, 2L)
        session$setInputs(refresh = 3)
        expect_equal(data()[simulation == "treatment"]$value, c(100, 200))
        unlink(file.path(runs$path[[3]], "results", "custom-lai.out"))
        session$setInputs(refresh = 4)
        expect_error(plot(), "No fresh outputs", class = "shiny.silent.error")
    })
})

test_that("an explorer opened without a source can load a directory", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    root <- local_runner_output()
    shiny::testServer(.runner_shiny_server(NULL, 10, 0), {
        expect_equal(nrow(catalog()), 0L)
        session$setInputs(source_path = root, open_source = 1)
        session$setInputs(instruction_files = "SiteA", simulations = "baseline", outputs = "dave_lai",
            column_mode = "common", yaxis = "auto", facet = "column", colour = "simulation")
        expect_equal(nrow(catalog()), 3L)
        expect_equal(nrow(selected_runs()), 1L)
        expect_s3_class(plot(), "ggplot")
    })
})
