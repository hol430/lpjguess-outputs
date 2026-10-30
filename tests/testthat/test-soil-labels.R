test_that("soil geometry labelling applies only to supported layered outputs", {
    expect_null(.get_soil_layer_labels(plot_data()))
    data <- plot_data(output = "dave_soil_state")
    data$layer <- NULL
    expect_null(.get_soil_layer_labels(data))
    expect_null(.get_soil_layer_labels(data.table::rbindlist(list(
        plot_data(output = "dave_soil_state"), plot_data(output = "dave_lai")))))
})

test_that("old soil files use one-based layer numbers and omit missing layers", {
    data <- plot_data(output = "dave_soil_state", variable = "wmass",
                      layer = c(0L, 2L, NA_integer_))
    expect_identical(.get_soil_layer_labels(data), c(`0` = "Layer 1", `2` = "Layer 3"))
    data$layer <- as.character(data$layer)
    expect_identical(.get_soil_layer_labels(data), c(`0` = "Layer 1", `2` = "Layer 3"))
    data$depth_top <- 0
    # A partially available pair of bounds still counts as the older format.
    expect_identical(.get_soil_layer_labels(data), c(`0` = "Layer 1", `2` = "Layer 3"))
})

test_that("consistent state depths produce centimetre ranges despite duplicate rows", {
    data <- plot_data(output = "dave_soil_state", variable = c("wmass", "imass"),
                      layer = c(0L, 1L), patch = c(0L, 1L))
    data$depth_top <- ifelse(data$layer == 0L, 0, 100)
    data$depth_bottom <- ifelse(data$layer == 0L, 100, 350)
    expect_identical(.get_soil_layer_labels(data),
                      c(`0` = "Layer 1 (0-10 cm)", `1` = "Layer 2 (10-35 cm)"))
})

test_that("soil properties derive tops from bottom minus thickness", {
    data <- plot_data(output = "dave_soil_properties",
                      variable = c("thickness", "depth_bot", "ph"), layer = 0:1)
    data$value <- ifelse(data$variable == "ph", 7,
        ifelse(data$layer == 0L, 100, ifelse(data$variable == "thickness", 250, 350)))
    expect_identical(.get_soil_layer_labels(data),
                      c(`0` = "Layer 1 (0-10 cm)", `1` = "Layer 2 (10-35 cm)"))
})

test_that("state and properties geometry agree across output formats", {
    state <- plot_data(output = "dave_soil_state", variable = "wmass")
    state$depth_top <- 100
    state$depth_bottom <- 300
    properties <- plot_data(output = "dave_soil_properties",
                            variable = c("thickness", "depth_bot"))
    properties$value <- ifelse(properties$variable == "thickness", 200, 300)
    data <- data.table::rbindlist(list(state, properties), fill = TRUE)
    expect_identical(.get_soil_layer_labels(data), c(`0` = "Layer 1 (10-30 cm)"))
    data$value[data$output == "dave_soil_properties" &
               data$variable == "thickness"] <- 100
    expect_warning(labels <- .get_soil_layer_labels(data),
                   "Soil layer depths vary or are unavailable for layers: 0",
                   fixed = TRUE)
    expect_identical(labels, c(`0` = "Layer 1"))
})

test_that("varying state geometry falls back only for affected layers", {
    data <- plot_data(output = "dave_soil_state", variable = "wmass", layer = 0:1)
    data$depth_top <- ifelse(data$layer == 0L, 0, 100)
    data$depth_bottom <- ifelse(data$layer == 0L, 100, c(300, 400))
    expect_warning(labels <- .get_soil_layer_labels(data),
                   "layers: 1; showing layer numbers", fixed = TRUE)
    expect_identical(labels, c(`0` = "Layer 1 (0-10 cm)", `1` = "Layer 2"))
})

test_that("invalid or missing state bounds warn and use layer numbers", {
    bounds <- list(c(-1, 100), c(100, 100), c(100, 0), c(NA_real_, 100),
                   c(0, NA_real_), c(0, Inf), c(-Inf, 100))
    for (bound in bounds) {
        data <- plot_data(output = "dave_soil_state", variable = "wmass")
        data$depth_top <- bound[1]
        data$depth_bottom <- bound[2]
        expect_warning(labels <- .get_soil_layer_labels(data),
                       "depths vary or are unavailable")
        expect_identical(labels, c(`0` = "Layer 1"))
    }
})

test_that("incomplete and varying properties geometry warn and fall back", {
    for (variable in c("thickness", "depth_bot")) {
        data <- plot_data(output = "dave_soil_properties", variable = variable)
        data$value <- 100
        expect_warning(labels <- .get_soil_layer_labels(data),
                       "depths vary or are unavailable")
        expect_identical(labels, c(`0` = "Layer 1"))
    }
    data <- plot_data(output = "dave_soil_properties",
                      variable = c("thickness", "depth_bot"))
    data$value <- c(100, 200, 300, 300)
    expect_warning(labels <- .get_soil_layer_labels(data),
                   "depths vary or are unavailable")
    expect_identical(labels, c(`0` = "Layer 1"))
})

test_that("properties with no geometry columns retain plain layer numbers", {
    data <- plot_data(output = "dave_soil_properties", variable = "ph")
    expect_identical(.get_soil_layer_labels(data), c(`0` = "Layer 1"))
})

test_that("non-numeric soil identifiers retain their original names", {
    data <- plot_data(output = "dave_soil_state", variable = "wmass",
                      layer = "organic", depth_top = 0, depth_bottom = 100)
    expect_identical(.get_soil_layer_labels(data), c(organic = "Layer organic"))
})
