test_that("ggplotly preserves separate series, facets, metadata and date axes", {
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    data <- plot_data(variable = c("MRS", "total"), repo = c("Baseline", "Treatment"),
                      patch = 0:1)
    static <- .plot(data, NULL, FALSE, c("repo", "site", "output"), "variable")
    saved_data <- data.table::copy(static$data)
    widget <- plotly::plotly_build(.dave_interactive_plot(static))
    traces <- widget$x$data

    expect_s3_class(widget, "plotly")
    # Plotly combines equally coloured series with NA separators. Verify those
    # separators keep patches distinct instead of assuming one trace per line.
    segments <- unlist(lapply(traces, function(trace) {
        runs <- rle(!is.na(trace$x))
        runs$lengths[runs$values]
    }))
    expect_length(segments, length(unique(static$data$.plot_series)))
    expect_true(all(segments == 2L))
    expect_false(any(vapply(traces, function(trace) isTRUE(trace$connectgaps), logical(1))))
    expect_equal(sort(unlist(lapply(traces, function(trace) trace$y))),
                  sort(static$data$value))
    expect_setequal(vapply(traces, function(trace) trace$name, character(1)),
                    c("MRS (m2/m2)", "total (m2/m2)"))
    expect_equal(sum(vapply(traces, function(trace) isTRUE(trace$showlegend), logical(1))), 2L)
    expect_setequal(vapply(traces, function(trace) trace$legendgroup, character(1)),
                    c("MRS", "total"))
    expect_setequal(vapply(traces, function(trace) trace$yaxis, character(1)), c("y", "y2"))
    annotations <- paste(vapply(widget$x$layout$annotations, function(label) label$text,
                                character(1)), collapse = "\n")
    expect_match(annotations, "Baseline<br />TestSite<br />LAI (m2/m2)", fixed = TRUE)
    expect_match(annotations, "Treatment<br />TestSite<br />LAI (m2/m2)", fixed = TRUE)
    expect_identical(widget$x$layout$xaxis$type, "date")
    expect_identical(widget$x$layout$xaxis$tickmode, "auto")
    expect_false(identical(widget$x$layout$yaxis$range, widget$x$layout$yaxis2$range))

    text <- paste(unlist(lapply(traces, function(trace) trace$text)), collapse = "\n")
    expect_match(text, "Source: Treatment", fixed = TRUE)
    expect_match(text, "Column: MRS (m2/m2)", fixed = TRUE)
    expect_match(text, "Output: dave_lai", fixed = TRUE)
    expect_match(text, "Patch: 1", fixed = TRUE)
    expect_false(grepl(".plot_series", text, fixed = TRUE))
    expect_false(grepl("as.factor", text, fixed = TRUE))
    expect_equal(static$data, saved_data)
    expect_null(static$mapping$text)
    expect_true(widget$x$config$scrollZoom)

    shared <- .plot(data, NULL, TRUE, c("repo", "site", "output"), "variable")
    shared_widget <- plotly::plotly_build(.dave_interactive_plot(shared))
    axes <- unique(vapply(shared_widget$x$data, function(trace) trace$yaxis, character(1)))
    ranges <- lapply(axes, function(axis) {
        shared_widget$x$layout[[sub("^y", "yaxis", axis)]]$range
    })
    expect_length(unique(ranges), 1L)
})

test_that("interactive soil facets and legends retain depth labels", {
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    data <- plot_data(output = "dave_soil_state", variable = "wmass", layer = 0:1)
    data$depth_top <- ifelse(data$layer == 0L, 0, 100)
    data$depth_bottom <- ifelse(data$layer == 0L, 100, 350)
    static <- .plot(data, "wmass", FALSE, "layer", "layer")
    widget <- plotly::plotly_build(.dave_interactive_plot(static))
    labels <- c("Layer 1 (0-10 cm)", "Layer 2 (10-35 cm)")
    expect_setequal(vapply(widget$x$data, function(trace) trace$name, character(1)), labels)
    annotations <- vapply(widget$x$layout$annotations, function(label) label$text, character(1))
    expect_true(all(labels %in% annotations))
    expect_identical(widget$x$layout$legend$title$text, "Layer")
    expect_match(widget$x$data[[1]]$text[[1]], "Column: wmass (mm)", fixed = TRUE)
})

test_that("joint facets keep per-output units for the same column", {
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    data <- data.table::rbindlist(list(plot_data(), plot_data(output = "dave_cmass")))
    static <- .plot(data, NULL, FALSE, c("output", "variable"), "site")
    widget <- plotly::plotly_build(.dave_interactive_plot(static))
    annotations <- vapply(widget$x$layout$annotations, function(label) label$text, character(1))
    expect_true("LAI<br />MRS (m2/m2)" %in% annotations)
    expect_true("C Mass<br />MRS (kgC/m2)" %in% annotations)
    text <- paste(unlist(lapply(widget$x$data, function(trace) trace$text)), collapse = "\n")
    expect_match(text, "Column: MRS (m2/m2)", fixed = TRUE)
    expect_match(text, "Column: MRS (kgC/m2)", fixed = TRUE)
})

test_that("hover text escapes raw names and missing values retain line gaps", {
    skip_if_not_installed("plotly", "4.12.0")
    withr::local_pdf(tempfile(fileext = ".pdf"))
    data <- plot_data(output = "custom", variable = "leaf<1", repo = "/runs/<baseline>&x",
        Date = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03"), tz = "UTC"))
    data$value <- c(1, NA, 3)
    static <- .dave_capture(.plot(data, NULL, FALSE, "site", "variable"))$value
    converted <- .dave_capture(.dave_interactive_plot(static))
    expect_null(converted$error)
    expect_match(converted$warnings, "No metadata found for outputs: custom")
    widget <- plotly::plotly_build(converted$value)
    trace <- widget$x$data[[1]]
    expect_equal(as.numeric(trace$y), c(1, NA, 3))
    expect_false(isTRUE(trace$connectgaps))
    expect_match(trace$text[[1]], "Source: /runs/&lt;baseline&gt;&amp;x", fixed = TRUE)
    expect_match(trace$text[[1]], "Column: leaf&lt;1", fixed = TRUE)
    expect_identical(trace$name, "leaf&lt;1")
})
