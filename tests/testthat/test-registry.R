test_that("metadata constructors retain fields and default to dynamic layers", {
    metadata <- example_metadata()
    expect_s4_class(metadata, "OutputFileMetadata")
    expect_true(methods::validObject(metadata))
    expect_identical(metadata@id, "file_test")
    expect_identical(metadata@name, "Test quantity")
    expect_identical(metadata@description, "Test output")
    expect_identical(metadata@units, "mm")
    expect_identical(metadata@aggregation_level, "patch")
    expect_identical(metadata@temporal_resolution, "daily")
    expect_s4_class(metadata@layers, "DynamicLayers")
    expect_identical(get_units(metadata@layers, "MRS"), "mm")
})

test_that("metadata constructors accept static specifications and S4 layers", {
    for (layers in list(c(a = "mm"), list(a = "mm"),
                        data.frame(layer = "a", unit = "mm"))) {
        metadata <- example_metadata(layers = layers)
        expect_s4_class(metadata@layers, "StaticLayers")
        expect_identical(get_units(metadata@layers, "a"), "mm")
    }
    layers <- create_static_layers("a", "patch", "daily", "mm")
    expect_identical(example_metadata(layers = layers)@layers, layers)
    expect_identical(get_units(example_metadata(layers = c("a", "b"))@layers,
                               "a"), "mm")
})

test_that("metadata constructors validate all scalar fields", {
    for (field in c("id", "name", "description", "units")) {
        for (value in list(NULL, character(), c("a", "b"), 1, TRUE)) {
            args <- setNames(list(value), field)
            expect_error(do.call(example_metadata, args), paste0(field, " must"))
        }
    }
    expect_error(example_metadata(id = ""), "id must be a non-empty")
    expect_error(example_metadata(aggregation_level = "bad"),
                 "Invalid aggregation_level")
    expect_error(example_metadata(temporal_resolution = "bad"),
                 "Invalid temporal_resolution")
    expect_error(example_metadata(layers = 1),
                 "layers must inherit from LayerDefinitions")
})

test_that("PFT and monthly constructors apply their documented defaults", {
    pft <- create_pft_output_metadata("file_pft", "PFT", "Plant types", "kgC/m2")
    expect_identical(pft@aggregation_level, "patch")
    expect_identical(pft@temporal_resolution, "daily")
    expect_s4_class(pft@layers, "DynamicLayers")
    monthly <- create_monthly_output_metadata("file_monthly", "Monthly", "Months",
                                               "mm", layers = c("Jan", "Feb"))
    expect_identical(monthly@aggregation_level, "gridcell")
    expect_identical(monthly@temporal_resolution, "monthly")
    expect_identical(get_units(monthly@layers, "Jan"), "mm")
    expect_error(create_monthly_output_metadata("file_m", "M", "Monthly"),
                 "Monthly resolution is not supported for dynamic layers")
})

test_that("registration returns invisibly and lookup is exact", {
    local_output_registry()
    metadata <- example_metadata()
    result <- withVisible(register_output_metadata(metadata))
    expect_false(result$visible)
    expect_identical(result$value, metadata)
    expect_identical(get_output_metadata("file_test"), metadata)
    expect_setequal(get_output_metadata_ids(), "file_test")
    expect_null(get_output_metadata("test"))
    expect_null(get_output_metadata("file_missing"))
    expect_error(register_output_metadata(list(id = "file_test")),
                 "metadata must be an OutputFileMetadata object")
})

test_that("duplicate registration warns and replaces the previous object", {
    local_output_registry()
    register_output_metadata(example_metadata())
    replacement <- example_metadata(units = "m")
    expect_warning(register_output_metadata(replacement),
                   "Overwriting output metadata with ID: file_test", fixed = TRUE)
    expect_identical(get_output_metadata("file_test"), replacement)
    expect_length(get_output_metadata_ids(), 1L)
})

test_that("the registry can be cleared repeatedly and reused", {
    local_output_registry()
    register_output_metadata(example_metadata())
    result <- withVisible(clear_output_registry())
    expect_false(result$visible)
    expect_null(result$value)
    expect_length(get_output_metadata_ids(), 0L)
    expect_null(get_output_metadata("file_test"))
    expect_no_error(clear_output_registry())
    register_output_metadata(example_metadata())
    expect_setequal(get_output_metadata_ids(), "file_test")
})

test_that("filename lookup handles paths and strips a single extension", {
    local_output_registry()
    metadata <- example_metadata()
    register_output_metadata(metadata)
    for (filename in c("test.out", "test", "test.csv",
                       file.path("some", "directory", "test.out"))) {
        expect_identical(guess_metadata_from_filename(filename), metadata)
    }
    expect_null(guess_metadata_from_filename("test.out.backup"))
    expect_null(guess_metadata_from_filename("file_test.out"))
    expect_null(guess_metadata_from_filename("missing.out"))
})

test_that("display names describe temporal and aggregation context", {
    local_output_registry()
    register_output_metadata(example_metadata())
    expect_identical(get_display_name("test.out"), "Daily Patch-level Test output")
    expect_null(get_display_name("unknown.out"))
    register_output_metadata(example_metadata(id = "file_annual",
        aggregation_level = "individual", temporal_resolution = "annual"))
    expect_identical(get_display_name("annual.out"),
                      "Annual Individual-level Test output")
})

test_that("display units use column units with a safe output-level fallback", {
    local_output_registry()
    register_output_metadata(example_metadata(layers = c(wmass = "mm", temp = "C"),
                                               units = "default"))
    expect_identical(get_display_units("test.out", "wmass"), "mm")
    expect_identical(get_display_units("test.out", "temp"), "C")
    for (layer in list(NULL, "", 1, character(), c("a", "b"), "missing", "Lon")) {
        expect_identical(get_display_units("test.out", layer), "default")
    }
    expect_null(get_display_units("unknown.out", "temp"))
})

test_that("display labels use callbacks and warn for unknown outputs", {
    local_output_registry()
    layers <- create_dynamic_layers("patch", "daily", "mm",
                                     function(x) paste("Name of", x))
    register_output_metadata(example_metadata(layers = layers))
    expect_identical(get_display_label("test", "a"), "Name of a")
    expect_warning(label <- get_display_label("unknown", "raw"),
                   "Failed to resolve metadata for var: unknown", fixed = TRUE)
    expect_identical(label, "raw")
})

test_that("quantity resolution returns useful fields and a layer name callback", {
    local_output_registry()
    metadata <- example_metadata(layers = c(wmass = "mm", temp = "C"))
    register_output_metadata(metadata)
    result <- resolve_quantity_metadata("test", "temp")
    expect_named(result, c("id", "name", "units", "display_name", "metadata",
                           "get_layer_name"))
    expect_identical(result$id, "test")
    expect_identical(result$name, "Test quantity")
    expect_identical(result$units, "C")
    expect_identical(result$display_name, "Daily Patch-level Test output")
    expect_identical(result$metadata, metadata)
    expect_identical(result$get_layer_name("temp"), "temp")
    register_output_metadata(example_metadata(id = "file_unnamed", name = ""))
    expect_null(resolve_quantity_metadata("unnamed")$name)
    for (id in list(NULL, character(), c("a", "b"), 1, "", "unknown")) {
        expect_null(resolve_quantity_metadata(id))
    }
})

test_that("registration helpers create the correct kinds of layers", {
    local_output_registry()
    result <- withVisible(add_output_dynamic("file_dynamic", "D", "Dynamic", "mm",
        "stand", "daily", function(x) paste("Data", x)))
    expect_false(result$visible)
    expect_identical(get_name(get_output_metadata("file_dynamic")@layers, "a"),
                      "Data a")
    add_pft_output("file_pft", "P", "PFT", "kgC/m2", "patch", "annual")
    expect_s4_class(get_output_metadata("file_pft")@layers, "DynamicLayers")
    expect_identical(get_output_metadata("file_pft")@temporal_resolution, "annual")
    unit <- "C"
    add_output_static("file_static", "S", "Static", "default",
                      c(temp = unit, , wmass = "mm", ), "patch", "daily")
    expect_identical(get_units(get_output_metadata("file_static")@layers, "temp"),
                      "C")
    definitions <- c("a", "b")
    add_output_static("file_static_names", "S", "Names", "mm", definitions,
                      "patch", "daily")
    expect_identical(get_units(get_output_metadata("file_static_names")@layers,
                               "a"), "mm")
    add_monthly_output("file_monthly", "M", "Monthly", "mm")
    monthly <- get_output_metadata("file_monthly")
    expect_s4_class(monthly@layers, "StaticLayers")
    expect_identical(monthly@temporal_resolution, "monthly")
    expect_identical(names(monthly@layers@units_by_layer), c(month.abb, "Total"))
    expect_identical(unname(monthly@layers@units_by_layer), rep("mm", 13L))
    expect_setequal(get_output_metadata_ids(), c("file_dynamic", "file_pft",
        "file_static", "file_static_names", "file_monthly"))
})
