test_that("factor selection supports public names, abbreviations and multiple facets", {
    expect_identical(.sanitise_factor("column"), "variable")
    expect_identical(.sanitise_factor("col"), "variable")
    expect_identical(.sanitise_factor(c("site", "column", "layer"), TRUE),
                      c("site", "variable", "layer"))
    for (factor in c("site", "output", "patch", "stand", "pft", "indiv", "layer")) {
        expect_identical(.sanitise_factor(factor), factor)
    }
    expect_error(.sanitise_factor("unknown"), "should be one of")
    expect_error(.sanitise_factor("p"), "should be one of")
    expect_error(.sanitise_factor(c("site", "output")), "length 1")
})

test_that("common variable selection intersects outputs and retains first-seen order", {
    data <- data.table::rbindlist(list(
        plot_data(variable = c("total", "MRS", "only_first")),
        plot_data(output = "dave_cmass", variable = c("MRS", "total", "only_second"))))
    expect_identical(.get_common_vars(data), c("total", "MRS"))
    data$variable <- factor(data$variable)
    expect_identical(as.character(.get_common_vars(data)), c("total", "MRS"))
    expect_identical(.get_common_vars(plot_data(variable = c("a", "b"))), c("a", "b"))
})

test_that("disjoint outputs give an error listing the available columns", {
    data <- data.table::rbindlist(list(plot_data(variable = "only_lai"),
        plot_data(output = "dave_cmass", variable = "only_cmass")))
    expect_error(.get_common_vars(data),
        paste0("no columns are present in all outputs. Per-output columns: ",
               "dave_lai: only_lai; dave_cmass: only_cmass"), fixed = TRUE)
    expect_error(.plot(data, NULL, TRUE, "output", "site"),
                 "Cannot decide which column to plot", fixed = TRUE)
})

test_that("explicit columns filter data and can select non-common variables", {
    data <- data.table::rbindlist(list(plot_data(variable = c("shared", "first")),
        plot_data(output = "dave_alai", variable = c("shared", "second"))))
    plot <- .plot(data, c("first", "second"), FALSE, "output", "variable")
    expect_setequal(plot$data$variable, c("first", "second"))
    expect_equal(nrow(plot$data), 4L)
    expect_error(.plot(data, c("missing_a", "missing_b"), FALSE, "output", "site"),
        "The following columns are not present in the data: missing_a, missing_b",
        fixed = TRUE)
    common <- .plot(data, NULL, TRUE, "output", "site")
    expect_identical(unique(common$data$variable), "shared")
})

test_that("plot data, geometry, axes and facet scale settings are correct", {
    data <- plot_data(variable = c("MRS", "total"))
    plot <- .plot(data, NULL, FALSE, c("site", "output"), "variable")
    expect_s3_class(plot, "ggplot")
    expect_s3_class(plot$layers[[1]]$geom, "GeomLine")
    expect_identical(plot$labels$x, "Date")
    expect_null(plot$labels$y)
    expect_identical(plot$labels$colour, "Column")
    expect_true(plot$facet$params$free$y)
    expect_false(plot$facet$params$free$x)
    expect_named(plot$facet$params$facets, c("site", "output"))
    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$data[[1]]), nrow(data))
    expect_equal(sort(built$data[[1]]$y), sort(data$value))
    expect_equal(sort(built$data[[1]]$x), sort(as.numeric(data$Date)))
    expect_false(.plot(data, NULL, TRUE, "site", "variable")$facet$params$free$y)
})

test_that("every available model and source dimension keeps time series separate", {
    alternatives <- list(repo = "another-run", site = "AnotherSite",
        output = "dave_alai", variable = "total", Lon = 151, Lat = -34,
        stand = 1L, patch = 1L, pft = "OtherPFT", indiv = 2L, layer = 1L)
    for (dimension in names(alternatives)) {
        first <- plot_data()
        second <- data.table::copy(first)
        second[[dimension]] <- alternatives[[dimension]]
        second$value <- c(10, 20)
        data <- data.table::rbindlist(list(first, second))
        plot <- .plot(data, NULL, TRUE, "site", "site")
        groups <- plot$data$.plot_series
        expect_equal(length(unique(groups)), 2L, info = dimension)
        expect_equal(as.integer(table(groups)), c(2L, 2L), info = dimension)
        built <- ggplot2::ggplot_build(plot)$data[[1]]
        # ggplot uses panel IDs as well as groups; neither series can be joined.
        expect_equal(nrow(unique(built[c("PANEL", "group")])), 2L, info = dimension)
    }
})

test_that("optional identity columns are not required for basic plotting", {
    data <- plot_data()
    for (column in c("stand", "patch", "pft", "indiv", "layer")) {
        data[[column]] <- NULL
    }
    plot <- .plot(data, NULL, FALSE, "site", "variable")
    expect_length(unique(plot$data$.plot_series), 1L)
    expect_no_error(ggplot2::ggplot_build(plot))
})

test_that("facets and legends use output and variable metadata", {
    plot <- .plot(plot_data(), NULL, TRUE, c("output", "variable"), "variable")
    expect_identical(facet_text(plot, output = "dave_lai", variable = "MRS"),
                      c("LAI", "MRS (m2/m2)"))
    expect_identical(facet_text(plot, output = "dave_lai"), "LAI (m2/m2)")
    expect_identical(facet_text(plot, variable = "MRS"), "MRS (m2/m2)")
    expect_identical(facet_text(plot, output = "dave_lai", variable = "unknown"),
                      c("LAI", "unknown"))
    scale <- plot$scales$get_scales("colour")
    expect_identical(scale$labels(c("MRS", "unknown")), c("MRS (m2/m2)", "unknown"))
    plot <- .plot(plot_data(), NULL, TRUE, "variable", "output")
    expect_identical(plot$scales$get_scales("colour")$labels("dave_lai"),
                      "LAI (m2/m2)")
    expect_identical(plot$labels$colour, "Output")
})

test_that("joint output and variable facets retain per-output units", {
    data <- data.table::rbindlist(list(plot_data(), plot_data(output = "dave_cmass")))
    expect_no_warning(plot <- .plot(data, NULL, FALSE, c("output", "variable"), "site"))
    expect_identical(facet_text(plot, output = c("dave_lai", "dave_cmass"),
                               variable = c("MRS", "MRS")),
                      c("LAI", "C Mass", "MRS (m2/m2)", "MRS (kgC/m2)"))
    expect_equal(nrow(ggplot2::ggplot_build(plot)$layout$layout), 2L)
})

test_that("dimension facets and legends include readable prefixes", {
    titles <- c(patch = "Patch", stand = "Stand", indiv = "Individual", pft = "PFT",
                site = "Site", layer = "Layer")
    for (dimension in names(titles)) {
        plot <- .plot(plot_data(), NULL, TRUE, dimension, dimension)
        expect_identical(plot$labels$colour, unname(titles[[dimension]]))
        value <- as.character(plot$data[[dimension]][1])
        expected <- .plot_dimension_labels(value, dimension)
        expect_identical(do.call(facet_text, c(list(plot),
                                               setNames(list(value), dimension))),
                          expected)
        expect_identical(plot$scales$get_scales("colour")$labels(value), expected)
    }
})

test_that("soil depths label both facets and colours", {
    data <- plot_data(output = "dave_soil_state", variable = "wmass", layer = 0:1)
    data$depth_top <- ifelse(data$layer == 0L, 0, 100)
    data$depth_bottom <- ifelse(data$layer == 0L, 100, 300)
    plot <- .plot(data, NULL, FALSE, "layer", "layer")
    expected <- c("Layer 1 (0-10 cm)", "Layer 2 (10-30 cm)")
    expect_identical(facet_text(plot, layer = c("0", "1")), expected)
    expect_identical(plot$scales$get_scales("colour")$labels(c("0", "1")), expected)
    expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("geometry columns remain available when plotting only soil properties data", {
    data <- plot_data(output = "dave_soil_properties",
                      variable = c("thickness", "depth_bot", "ph"))
    data$value <- ifelse(data$variable == "ph", 7, 100)
    plot <- .plot(data, "ph", FALSE, "layer", "layer")
    expect_identical(unique(plot$data$variable), "ph")
    expect_identical(facet_text(plot, layer = "0"), "Layer 1 (0-10 cm)")
    expect_identical(plot$scales$get_scales("colour")$labels("0"),
                      "Layer 1 (0-10 cm)")
})

test_that("irrelevant soil geometry does not warn when layers are not labelled", {
    data <- plot_data(output = "dave_soil_state", variable = "wmass",
                      depth_top = -1, depth_bottom = 100)
    expect_no_warning(.plot(data, NULL, FALSE, "site", "variable"))
})

test_that("public plotting reads real files and applies default and custom arguments", {
    repo <- local_dave_repo(c("SiteA", "SiteB"))
    for (site in c("SiteA", "SiteB")) {
        for (output in c("dave_lai", "dave_alai")) {
            write_dave_output(repo, data.frame(Lon = 150, Lat = -33, Year = 2024L,
                Day = c(0L, 1L), patch = 0L, MRS = c(2, 3), total = c(4, 5)),
                output, site)
        }
    }
    single <- plot_dave(repo, "file_dave_lai.out", "SiteA")
    expect_s3_class(single, "ggplot")
    expect_true(single$facet$params$free$y)
    expect_named(single$facet$params$facets, c("site", "output"))
    expect_identical(single$labels$colour, "Column")
    expect_equal(nrow(single$data), 4L)
    multiple <- plot_dave(repo, c("dave_lai", "dave_alai"), c("SiteA", "SiteB"))
    expect_false(multiple$facet$params$free$y)
    expect_equal(nrow(multiple$data), 16L)
    custom <- plot_dave(repo, "dave_lai", c("SiteA", "SiteB"), columns = "MRS",
                        common_yaxis = TRUE, facet = c("site", "column"),
                        colour = "patch")
    expect_identical(unique(as.character(custom$data$variable)), "MRS")
    expect_named(custom$facet$params$facets, c("site", "variable"))
    expect_identical(custom$labels$colour, "Patch")
    expect_false(custom$facet$params$free$y)
    expect_no_error(ggplot2::ggplotGrob(custom))
    expect_error(plot_dave(repo, "dave_lai", "SiteA", colour = "unknown"),
                 "should be one of")
    expect_error(plot_dave(repo, "dave_lai", "SiteA", facet = "unknown"),
                 "should be one of")
    expect_error(plot_dave(repo, "dave_lai", "SiteA", columns = "missing"),
                 "not present in the data")
})
