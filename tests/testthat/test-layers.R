test_that("aggregation and temporal enums accept supported values", {
    aggregation <- c(gridcell = "Gridcell", stand = "Stand", patch = "Patch",
                     individual = "Individual")
    temporal <- c(subdaily = "Subdaily", daily = "Daily", monthly = "Monthly",
                  annual = "Annual")
    for (value in names(aggregation)) {
        expect_identical(validate_aggregation_level(value), value)
        expect_identical(get_aggregation_label(value), aggregation[[value]])
    }
    for (value in names(temporal)) {
        expect_identical(validate_temporal_resolution(value), value)
        expect_identical(get_temporal_resolution_label(value), temporal[[value]])
    }
})

test_that("enum validation rejects non-scalars, non-strings and unknown values", {
    invalid <- list(NULL, character(), c("patch", "stand"), 1, TRUE,
                    list("patch"), factor("patch"))
    for (value in invalid) {
        expect_error(validate_aggregation_level(value),
                     "aggregation_level must be a single character value")
        expect_error(validate_temporal_resolution(value),
                     "temporal_resolution must be a single character value")
    }
    for (value in c("", "Patch", "weekly", NA_character_)) {
        expect_error(get_aggregation_label(value), "Invalid aggregation_level")
        expect_error(get_temporal_resolution_label(value),
                     "Invalid temporal_resolution")
    }
})

test_that("metadata layers reflect the aggregation hierarchy", {
    dimensions <- list(gridcell = character(), stand = "stand",
        patch = c("stand", "patch"),
        individual = c("stand", "patch", "indiv", "pft"))
    for (aggregation in names(dimensions)) {
        for (temporal in c("daily", "subdaily", "monthly", "annual")) {
            layers <- get_metadata_layers(aggregation, temporal)
            expect_true(all(c("Lon", "Lat", "Year") %in% layers))
            if (temporal %in% c("daily", "subdaily")) {
                expect_contains(layers, "Day")
            }
            expect_identical(intersect(layers, c("stand", "patch", "indiv",
                                                 "pft")),
                              dimensions[[aggregation]])
            expect_identical(anyDuplicated(layers), 0L)
        }
    }
    expect_error(get_metadata_layers("invalid", "daily"),
                 "Invalid aggregation_level")
    expect_error(get_metadata_layers("patch", "invalid"),
                 "Invalid temporal_resolution")
})

test_that("static layer specifications normalise supported input formats", {
    expected <- c(wmass = "mm", temp = "C")
    expect_identical(.normalise_layer_units(expected), expected)
    expect_identical(.normalise_layer_units(as.list(expected)), expected)
    expect_identical(.normalise_layer_units(data.frame(
        layer = factor(c("wmass", "temp")), unit = factor(c("mm", "C")))),
        expected)
    expect_identical(.normalise_layer_units(c("wmass", "imass"), "mm"),
                      c(wmass = "mm", imass = "mm"))
    expect_identical(.normalise_layer_units(setNames(c("a", "b"), c("", "")),
                                            "unit"), c(a = "unit", b = "unit"))
    expect_identical(.normalise_layer_units(list("a", "b")), c(a = "", b = ""))
    expect_identical(.normalise_layer_units(character()),
                      setNames(character(), character()))
})

test_that("static layer specifications reject malformed definitions", {
    expect_error(.normalise_layer_units(data.frame(layer = "a")),
                 "must contain columns 'layer' and 'unit'", fixed = TRUE)
    expect_error(.normalise_layer_units(c(a = "mm", "C")),
                 "must have either all names or no names", fixed = TRUE)
    for (value in list(1, TRUE, matrix(1), list(a = 1))) {
        expect_error(.normalise_layer_units(value), "layer_units must be")
    }
    for (units in list(NULL, 1, character(), c("mm", "C"))) {
        expect_error(.normalise_layer_units("a", units),
                     "default_units must be a single character value")
    }
})

test_that("static layers expose declared units and custom names", {
    layers <- create_static_layers(c(wmass = "mm", temp = "C"), "patch", "daily",
                                    get_name_func = function(x) paste("Soil", x))
    expect_s4_class(layers, "StaticLayers")
    expect_s4_class(layers, "LayerDefinitions")
    expect_identical(get_units(layers, "wmass"), "mm")
    expect_identical(get_units(layers, "temp"), "C")
    expect_identical(get_name(layers, "temp"), "Soil temp")
    expect_error(get_units(layers, "unknown"), "not a static data layer")
    expect_identical(get_name(create_static_layers("wmass", "patch", "daily"),
                              "wmass"), "wmass")
})

test_that("dynamic layers accept arbitrary data columns with shared units", {
    layers <- create_dynamic_layers("individual", "subdaily", "mm",
                                     function(x) paste("PFT", x))
    expect_s4_class(layers, "DynamicLayers")
    expect_s4_class(layers, "LayerDefinitions")
    expect_true(is_data_layer(layers, "MRS"))
    expect_identical(get_units(layers, "MRS"), "mm")
    expect_identical(get_units(layers, "total"), "mm")
    expect_identical(get_name(layers, "MRS"), "PFT MRS")
    defaults <- create_dynamic_layers("gridcell", "annual", "")
    expect_identical(get_name(defaults, "MRS"), "MRS")
    expect_identical(get_units(defaults, "MRS"), "")
})

test_that("metadata columns are excluded by both layer classes", {
    classes <- list(create_static_layers("wmass", "individual", "daily"),
                    create_dynamic_layers("individual", "daily", "mm"))
    for (layers in classes) {
        for (column in c("Lon", "Lat", "Year", "Day", "stand", "patch",
                         "indiv", "pft")) {
            expect_false(is_data_layer(layers, column), info = column)
            expect_error(get_units(layers, column), "Layer '")
            expect_error(get_name(layers, column), "Layer '")
        }
        for (columns in list(character(), c("wmass", "temp"))) {
            expect_error(is_data_layer(layers, columns),
                         "layer must be a single character value")
            expect_error(get_units(layers, columns),
                         "layer must be a single character value")
            expect_error(get_name(layers, columns),
                         "layer must be a single character value")
        }
    }
})

test_that("layer constructors validate context and dynamic units", {
    expect_error(create_static_layers("a", "bad", "daily"),
                 "Invalid aggregation_level")
    expect_error(create_static_layers("a", "patch", "bad"),
                 "Invalid temporal_resolution")
    expect_error(create_dynamic_layers("bad", "daily", "mm"),
                 "Invalid aggregation_level")
    expect_error(create_dynamic_layers("patch", "bad", "mm"),
                 "Invalid temporal_resolution")
    expect_error(create_dynamic_layers("patch", "monthly", "mm"),
                 "Monthly resolution is not supported for dynamic layers")
    for (units in list(NULL, 1, character(), c("mm", "C"))) {
        expect_error(create_dynamic_layers("patch", "daily", units),
                     "default_units must be a single character value")
    }
})

test_that("static layer expressions preserve names and caller scope", {
    env <- list2env(list(unit = "mm", layers = c(temp = "C")))
    expect_identical(.evaluate_layers_expr(quote(c(wmass = unit, , temp = "C", )),
                                           env), c(wmass = "mm", temp = "C"))
    expect_identical(.evaluate_layers_expr(quote(layers), env), c(temp = "C"))
    expect_null(.evaluate_layers_expr(quote(c()), env))
    expect_error(.evaluate_layers_expr(quote(missing_layers), env),
                 "missing_layers")
})
