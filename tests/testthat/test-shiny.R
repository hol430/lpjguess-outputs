test_that("source paths are validated, normalised, labelled and deduplicated", {
    repo <- local_dave_repo()
    expect_identical(unname(.dave_sources(c(repo, paste0(repo, "/.")))), repo)
    expect_identical(names(.dave_sources(c(Baseline = repo))), "Baseline")
    expect_identical(.dave_sources(character()), stats::setNames(character(), character()))
    expect_error(.dave_sources(NA_character_), "character vector")
    expect_error(.dave_sources(1), "character vector")
    expect_error(.dave_sources(dirname(repo)), "missing settings.ins")
})

test_that("site discovery intersects sources and requires output directories", {
    first <- local_dave_repo(c("SiteA", "SiteB", "OnlyFirst"))
    second <- local_dave_repo(c("SiteA", "SiteB", "OnlySecond"))
    dir.create(file.path(first, "benchmarks", "ozflux", "NoOutputs"))
    expect_identical(.dave_common_sites(character()), character())
    expect_identical(.dave_common_sites(c(first, second)), c("SiteA", "SiteB"))
    expect_false("NoOutputs" %in% .dave_common_sites(first))
    unlink(file.path(second, "benchmarks", "ozflux", "SiteB"), recursive = TRUE)
    expect_identical(.dave_common_sites(c(first, second)), "SiteA")
    expect_identical(.dave_common_sites(c(first, file.path(second, "missing"))), character())
})

test_that("freshness is measured within each site and includes the threshold boundary", {
    repo <- local_dave_repo(c("SiteA", "SiteB"))
    timestamp <- as.POSIXct("2024-01-01", tz = "UTC")
    frame <- data.frame(Lon = 150, Lat = -33, Year = 2024L, MRS = 1)
    for (site in c("SiteA", "SiteB")) {
        offset <- if (site == "SiteA") 0 else 3600
        for (output in c("newest", "boundary", "stale", "empty")) {
            path <- write_dave_output(repo, frame, output, site)
            if (output == "empty") writeLines(character(), path)
            age <- switch(output, newest = 0, boundary = 10, stale = 11, empty = 0)
            Sys.setFileTime(path, timestamp + offset - age)
        }
        directory <- file.path(repo, "benchmarks", "ozflux", site, "out")
        dir.create(file.path(directory, "directory.out"))
        writeLines("unrelated", file.path(directory, "guess.log"))
    }
    files <- .dave_inventory(repo, c("SiteA", "SiteB"), 10)
    expect_equal(nrow(files), 8L)
    expect_true(all(files$status[files$output == "boundary"] == "Available"))
    expect_true(all(files$status[files$output == "stale"] == "Stale"))
    expect_true(all(files$status[files$output == "empty"] == "Empty"))
    expect_setequal(.dave_common_outputs(files, repo, c("SiteA", "SiteB")),
                    c("newest", "boundary"))
    expect_identical(.dave_common_outputs(.dave_inventory(repo, "SiteA", 0),
                                         repo, "SiteA"), "newest")
    expect_setequal(.dave_common_outputs(.dave_inventory(repo, "SiteA", 11),
                                         repo, "SiteA"), c("newest", "boundary", "stale"))
})

test_that("outputs must exist and be fresh in every source/site pair", {
    first <- local_dave_repo(c("SiteA", "SiteB"))
    second <- local_dave_repo(c("SiteA", "SiteB", "EmptySite"))
    frame <- data.frame(Lon = 150, Lat = -33, Year = 2024L, MRS = 1)
    timestamp <- as.POSIXct("2024-01-01", tz = "UTC")
    for (repo in c(first, second)) {
        for (site in c("SiteA", "SiteB")) {
            for (output in c("dave_lai", "custom_output", "not_everywhere", "old_in_one")) {
                if (repo == second && site == "SiteB" && output == "not_everywhere") next
                path <- write_dave_output(repo, frame, output, site)
                old <- repo == second && site == "SiteB" && output == "old_in_one"
                Sys.setFileTime(path, timestamp - if (old) 60 else 0)
            }
        }
    }
    files <- .dave_inventory(c(first, second), c("SiteA", "SiteB"), 10)
    expect_setequal(.dave_common_outputs(files, c(first, second), c("SiteA", "SiteB")),
                    c("custom_output", "dave_lai"))
    expect_length(.dave_common_outputs(files, c(first, second), c("SiteA", "EmptySite")), 0L)
    expect_length(.dave_common_outputs(files, character(), "SiteA"), 0L)
    expect_length(.dave_common_outputs(files, first, character()), 0L)
    choices <- .dave_output_choices(c("dave_lai", "custom_output", "dave_npp",
                                      "dave_anpp", "mnpp", "dave_indiv_npp"))
    expect_identical(names(choices), c("LAI", "NPP", "Other outputs"))
    expect_match(names(choices$LAI)[1], "Leaf Area Index.*dave_lai")
    expect_identical(choices[["Other outputs"]], c(custom_output = "custom_output"))
    expect_setequal(unname(choices$NPP), c("dave_npp", "dave_anpp", "mnpp", "dave_indiv_npp"))
    expect_match(names(choices$NPP)[choices$NPP == "dave_npp"], "Daily Patch-level")
    expect_match(names(choices$NPP)[choices$NPP == "dave_anpp"], "Annual Patch-level")
    expect_match(names(choices$NPP)[choices$NPP == "mnpp"], "Monthly Gridcell-level")
    expect_match(names(choices$NPP)[choices$NPP == "dave_indiv_npp"], "Daily Individual-level")
})

test_that("the public app validates configuration and produces a Shiny app", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    expect_s3_class(dave_shiny(), "shiny.appobj")
    expect_error(dave_shiny(stale_threshold_secs = -1), "stale_threshold_secs")
    expect_error(dave_shiny(stale_threshold_secs = NA_real_), "finite non-negative")
    expect_error(dave_shiny(refresh_interval = Inf), "refresh_interval")
    expect_error(dave_shiny(refresh_interval = "1000"), "refresh_interval")
})

test_that("the server caches data, exposes plot controls and exports reproducible code", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    first <- local_dave_repo(c("SiteA", "SiteB"))
    second <- local_dave_repo(c("SiteA", "SiteB"))
    for (repo in c(first, second)) {
        for (site in c("SiteA", "SiteB")) {
            write_dave_output(repo, data.frame(Lon = 150, Lat = -33,
                Year = 2024L, Day = 0:1, patch = 0L, MRS = 1:2, total = 3:4),
                "dave_lai", site)
            write_dave_output(repo, data.frame(Lon = 150, Lat = -33,
                Year = 2024L, Day = 0:1, layer = 0L, wmass = 5:6),
                "dave_soil_state", site)
        }
    }
    reads <- 0L
    original <- read_dave
    local_mocked_bindings(read_dave = function(...) {
        reads <<- reads + 1L
        original(...)
    })
    server <- .dave_shiny_server(.dave_sources(c(first, second)), 10, 0)
    shiny::testServer(server, {
        session$setInputs(sources = c(first, second), sites = c("SiteA", "SiteB"),
            outputs = "dave_lai", columns = "MRS", facet = c("repo", "site", "output"),
            colour = "column", column_mode = "selected", yaxis = "auto")
        expect_setequal(site_choices(), c("SiteA", "SiteB"))
        expect_setequal(output_choices(), c("dave_lai", "dave_soil_state"))
        expect_equal(nrow(data()), 16L)
        expect_setequal(column_choices(), c("MRS", "total"))
        expect_true("patch" %in% dimensions())
        expect_false("layer" %in% dimensions())
        expect_s3_class(plot(), "ggplot")
        expect_equal(nrow(plot()$data), 8L)
        expect_true(plot()$facet$params$free$y)
        expect_s3_class(interactive_plot(), "plotly")
        expect_match(output$plot, "plotly")
        expect_equal(reads, 1L)
        session$setInputs(colour = "repo", facet = c("site", "column"),
                           yaxis = "shared", columns = "total")
        expect_equal(nrow(plot()$data), 8L)
        expect_identical(plot()$labels$colour, "Source")
        expect_false(plot()$facet$params$free$y)
        expect_equal(reads, 1L)
        expect_false(".plot_series" %in% names(data()))
        expect_false(".plot_series" %in% names(plotted_data()))
        expect_false(".dave_tooltip" %in% names(plot()$data))
        expect_false(".dave_tooltip" %in% names(plotted_data()))
        csv <- output$download_data
        expect_true(file.exists(csv))
        downloaded <- data.table::fread(csv)
        expect_equal(nrow(downloaded), nrow(plotted_data()))
        expect_setequal(downloaded$variable, "total")
        expect_false(".plot_series" %in% names(downloaded))
        expect_gt(file.info(output$download_png)$size, 1000)
        expect_gt(file.info(output$download_pdf)$size, 1000)
        code <- output$code
        reproduced <- eval(parse(text = code))
        expect_s3_class(reproduced, "ggplot")
        expect_equal(reproduced$data, plot()$data)

        session$setInputs(outputs = c("dave_lai", "dave_soil_state"),
                           columns = c("MRS", "wmass"), facet = c("repo", "output"))
        expect_setequal(column_choices(), c("MRS", "total", "wmass"))
        session$setInputs(column_mode = "common")
        expect_setequal(plot_options()$columns, c("MRS", "total", "wmass"))
        session$setInputs(column_mode = "selected")
        expect_true(all(c("patch", "layer") %in% dimensions()))
        expect_setequal(plot()$data$variable, c("MRS", "wmass"))
        expect_equal(nrow(plotted_data()), 16L)
        expect_no_error(ggplot2::ggplot_build(plot()))
        expect_no_error(plotly::plotly_build(interactive_plot()))

        session$setInputs(outputs = "dave_soil_state", columns = "wmass",
                           colour = "layer", facet = "layer", yaxis = "free")
        expect_false("patch" %in% dimensions())
        expect_identical(plot()$labels$colour, "Layer")
        expect_true(plot()$facet$params$free$y)
        expect_s3_class(interactive_plot(), "plotly")
    })
})

test_that("refreshes invalidate stale plots and never silently drop selected sites", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    repo <- local_dave_repo(c("SiteA", "SiteB"))
    frame <- data.frame(Lon = 150, Lat = -33, Year = 2024L, Day = 0:1, MRS = 1:2)
    for (site in c("SiteA", "SiteB")) write_dave_output(repo, frame, site = site)
    timestamp <- as.POSIXct("2024-01-01", tz = "UTC")
    for (site in c("SiteA", "SiteB")) {
        Sys.setFileTime(.get_out_file(repo, "dave_lai", site), timestamp)
    }
    server <- .dave_shiny_server(.dave_sources(repo), 10, 0)
    shiny::testServer(server, {
        session$setInputs(sources = repo, sites = c("SiteA", "SiteB"), outputs = "dave_lai",
            columns = "MRS", facet = "site", colour = "column", column_mode = "selected", yaxis = "auto")
        expect_s3_class(plot(), "ggplot")
        fresh <- write_dave_output(repo, frame, "dave_alai", "SiteB")
        Sys.setFileTime(fresh, timestamp + 11)
        session$setInputs(refresh = 1)
        expect_length(output_choices(), 0L)
        expect_error(plot(), "No fresh outputs", class = "shiny.silent.error")
        expect_error(interactive_plot(), "No fresh outputs", class = "shiny.silent.error")
        Sys.setFileTime(.get_out_file(repo, "dave_lai", "SiteB"), timestamp + 1)
        session$setInputs(refresh = 2)
        expect_identical(output_choices(), "dave_lai")
        expect_s3_class(plot(), "ggplot")
        unlink(.get_out_file(repo, "dave_lai", "SiteB"))
        session$setInputs(refresh = 3)
        expect_length(output_choices(), 0L)
        expect_error(plot(), "No fresh outputs", class = "shiny.silent.error")
        session$setInputs(sites = "SiteA")
        expect_identical(output_choices(), "dave_lai")
        expect_s3_class(plot(), "ggplot")
        session$setInputs(sites = "MissingSite")
        expect_error(plot(), "Select one or more shared sites", class = "shiny.silent.error")
        write_dave_output(repo, frame[0L, ], "dave_lai", "SiteB")
        session$setInputs(sites = c("SiteA", "SiteB"), refresh = 4)
        expect_error(data(), "no data rows", class = "shiny.silent.error")
        session$setInputs(sources = character(), sites = character())
        expect_length(site_choices(), 0L)
    })
})

test_that("sources can be added in the app and unreadable selections show useful errors", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    repo <- local_dave_repo()
    write_dave_output(repo, data.frame(Lon = 150, Lat = -33, Year = 2024,
                                      Day = 0:1, MRS = 1:2))
    shiny::testServer(.dave_shiny_server(.dave_sources(character()), 10, 0), {
        session$setInputs(source_paths = repo, add_sources = 1)
        expect_identical(unname(sources()), repo)
        session$setInputs(sources = repo, sites = "TestSite", outputs = "dave_lai",
            columns = "MRS", facet = "site", colour = "column", column_mode = "selected", yaxis = "auto")
        expect_s3_class(plot(), "ggplot")
        writeLines("malformed data", .get_out_file(repo, "dave_lai", "TestSite"))
        session$setInputs(refresh = 1)
        expect_error(data(), "Cannot calculate date", class = "shiny.silent.error")
    })
})

test_that("automatic and manual checks reuse unchanged data and reload changed files", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    repo <- local_dave_repo()
    frame <- data.frame(Lon = 150, Lat = -33, Year = 2024, Day = 0:1,
                        MRS = 1:2, total = 3:4)
    write_dave_output(repo, frame)
    reads <- 0L
    original <- read_dave
    local_mocked_bindings(read_dave = function(...) {
        reads <<- reads + 1L
        original(...)
    })
    shiny::testServer(.dave_shiny_server(.dave_sources(repo), 10, 1000), {
        session$setInputs(sources = repo, sites = "TestSite", outputs = "dave_lai",
            facet = "site", colour = "column", column_mode = "common", yaxis = "auto")
        expect_setequal(plot_options()$columns, c("MRS", "total"))
        expect_equal(nrow(plot()$data), 4L)
        expect_equal(reads, 1L)
        session$elapse(1000)
        session$setInputs(refresh = 1)
        expect_equal(nrow(plot()$data), 4L)
        expect_equal(reads, 1L)
        frame$MRS <- c(100, 200)
        write_dave_output(repo, frame)
        session$elapse(1000)
        expect_equal(data()[variable == "MRS"]$value, c(100, 200))
        expect_equal(reads, 2L)
        session$setInputs(column_mode = "all")
        expect_setequal(plot_options()$columns, c("MRS", "total"))
        expect_equal(reads, 2L)
    })
})
