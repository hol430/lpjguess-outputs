test_that("the built-in catalogue contains valid and self-consistent metadata", {
    ids <- get_output_metadata_ids()
    expect_gt(length(ids), 0L)
    expect_identical(anyDuplicated(ids), 0L)
    expect_true(all(startsWith(ids, "file_")))
    metadata <- lapply(ids, get_output_metadata)
    expect_identical(vapply(metadata, function(x) x@id, character(1)), ids)
    expect_true(all(vapply(metadata, methods::validObject, logical(1))))
    expect_true(all(vapply(metadata, function(x) nzchar(x@name), logical(1))))
    expect_true(all(vapply(metadata, function(x) nzchar(x@description), logical(1))))
    expect_true(all(vapply(metadata, function(x)
        x@aggregation_level == x@layers@aggregation_level &&
        x@temporal_resolution == x@layers@temporal_resolution, logical(1))))
    for (item in metadata) {
        expect_false(is_data_layer(item@layers, "Year"), info = item@id)
        if (methods::is(item@layers, "StaticLayers")) {
            expect_true(all(nzchar(names(item@layers@units_by_layer))),
                        info = item@id)
            expect_identical(anyDuplicated(names(item@layers@units_by_layer)), 0L,
                              info = item@id)
        }
    }
})

test_that("representative built-in quantities retain their scientific units", {
    cases <- data.frame(
        id = c("dave_lai", "lai", "dave_alai", "dave_gpp", "dave_agpp",
               "dave_stand_frac", "dave_met_subdaily_temp", "mnpp",
               "dave_soil_state", "dave_soil_properties", "firert", "mald"),
        aggregation = c("patch", "gridcell", "patch", "patch", "patch", "stand",
                        "patch", "gridcell", "patch", "patch", "gridcell", "gridcell"),
        temporal = c("daily", "annual", "annual", "daily", "annual", "annual",
                     "subdaily", "monthly", "daily", "daily", "annual", "monthly"),
        layer = c("MRS", "MRS", "MRS", "gpp", "MRS", "fraction", "MRS", "Jan",
                  "psi_mpa", "bulk_density", "FireRT", "MAXALD"),
        units = c("m2/m2", "m2/m2", "m2/m2", "gC/m2/day", "kgC/m2/year", "",
                  "°C", "kgC/m2/month", "MPa", "g/cm3", "years", "m"),
        stringsAsFactors = FALSE)
    for (i in seq_len(nrow(cases))) {
        metadata <- get_output_metadata(paste0("file_", cases$id[i]))
        expect_s4_class(metadata, "OutputFileMetadata")
        expect_identical(metadata@aggregation_level, cases$aggregation[i],
                          info = cases$id[i])
        expect_identical(metadata@temporal_resolution, cases$temporal[i],
                          info = cases$id[i])
        expect_identical(get_display_units(paste0(cases$id[i], ".out"),
                                           cases$layer[i]), cases$units[i],
                          info = cases$id[i])
    }
    expect_identical(get_display_units("dave_soil_state.out", "temp"), "°C")
    expect_identical(get_display_units("dave_soil_state.out", "wmass"), "mm")
    expect_identical(get_display_units("dave_soil_properties.out", "depth_bot"),
                      "mm")
    expect_identical(get_display_units("firert.out", "BurntFr"), "0-1")
})

test_that("all standard monthly quantities expose months and a total", {
    ids <- get_output_metadata_ids()
    monthly <- Filter(function(x) x@temporal_resolution == "monthly",
                      lapply(ids, get_output_metadata))
    expect_gt(length(monthly), 0L)
    for (metadata in monthly) {
        expect_s4_class(metadata@layers, "StaticLayers")
        expect_true(all(c(month.abb, "Total") %in%
                        names(metadata@layers@units_by_layer)), info = metadata@id)
    }
})

test_that("soil indices accept canonical nonnegative integers only", {
    for (index in c(0L, 1L, 12L)) {
        expect_identical(.soil_layer_index(as.character(index)), index)
        expect_identical(.soil_layer_number(as.character(index)),
                          paste("Layer", index + 1L))
    }
    for (value in c("-1", "1.5", "01", "1.0", "1e2", "abc", "", "Inf",
                    "2147483648", NA_character_)) {
        expect_identical(.soil_layer_index(value), NA_integer_)
        expect_identical(.soil_layer_number(value), paste("Layer", value))
    }
    expect_identical(.soil_layer_index(0L), 0L)
})

test_that("soil labels convert millimetres to centimetres and use one-based names", {
    expect_identical(.soil_layer_label("0", 0, 100), "Layer 1 (0-10 cm)")
    expect_identical(.soil_layer_label("2", 125, 275), "Layer 3 (12.5-27.5 cm)")
    expect_identical(.soil_layer_label("organic", 0, 100), "Layer organic")
    expect_identical(get_display_label("dave_soil_psi_layer", "soil_psi0"),
                      "Layer 1")
    expect_identical(get_display_label("dave_soil_psi_layer", "soil_psi12"),
                      "Layer 13")
    expect_identical(get_display_label("dave_soil_psi_layer", "soil_psiorganic"),
                      "Layer organic")
})
