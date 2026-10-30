test_that("unit labels omit empty, missing and dimensionless markers", {
    for (units in c("", "-", NA_character_)) {
        expect_identical(.plot_label_with_units("Water", units), "Water")
    }
    expect_identical(.plot_label_with_units("Water", "mm"), "Water (mm)")
    expect_identical(.plot_label_with_units("Fraction", "0-1"), "Fraction (0-1)")
})

test_that("duplicate labels gain raw identifiers without changing unique labels", {
    expect_identical(.disambiguate_plot_labels(c(a = "Same", b = "Other",
                                                c = "Same")),
                      c(a = "Same [a]", b = "Other", c = "Same [c]"))
    expect_identical(.disambiguate_plot_labels(c(a = "One")), c(a = "One"))
    expect_identical(.disambiguate_plot_labels(setNames(character(), character())),
                      setNames(character(), character()))
})

test_that("label lookup supports factors and preserves unknown raw values", {
    labels <- c(a = "Alpha", b = "Beta")
    expect_identical(.lookup_plot_labels(factor(c("b", "missing", "a")), labels),
                      c("Beta", "missing", "Alpha"))
    expect_identical(.lookup_plot_labels(c("a", NA_character_), labels),
                      c("Alpha", NA_character_))
    expect_identical(.lookup_plot_labels(character(), labels), character())
})

test_that("dimension labels identify model dimensions and retain other values", {
    dimensions <- c(layer = "Layer", patch = "Patch", stand = "Stand",
                    indiv = "Individual")
    for (dimension in names(dimensions)) {
        expect_identical(.plot_dimension_labels(c(0L, 2L, NA_integer_), dimension),
                          c(paste(dimensions[[dimension]], c(0, 2)), NA_character_))
    }
    expect_identical(.plot_dimension_labels(factor(c("MRS", "total")), "pft"),
                      c("MRS", "total"))
    expect_identical(.plot_dimension_labels("TestSite", "site"), "TestSite")
})

test_that("known outputs produce units on output, variable and pair labels", {
    labels <- .get_plot_label_maps(plot_data(), "output", "variable")
    expect_identical(labels$output, c(dave_lai = "LAI (m2/m2)"))
    expect_identical(labels$output_name, c(dave_lai = "LAI"))
    expect_identical(labels$variable, c(MRS = "MRS (m2/m2)"))
    expect_identical(labels$pairs$label, "MRS (m2/m2)")
    expect_identical(labels$pairs$key, "dave_lai\rMRS")
})

test_that("unknown output metadata warns and falls back to raw names", {
    expect_warning(labels <- .get_plot_label_maps(plot_data(output = "unknown"),
                                                  "output", "variable"),
                   "No metadata found for outputs: unknown; using raw names",
                   fixed = TRUE)
    expect_identical(labels$output, c(unknown = "unknown"))
    expect_identical(labels$variable, c(MRS = "MRS"))
    expect_identical(labels$pairs$units, "")
})

test_that("empty metadata names fall back to the output ID", {
    local_output_registry()
    register_output_metadata(example_metadata(name = ""))
    labels <- .get_plot_label_maps(plot_data(output = "test"), "output", "site")
    expect_identical(labels$output, c(test = "test (mm)"))
    expect_identical(labels$output_name, c(test = "test"))
})

test_that("mixed output units warn when one output label must cover all columns", {
    data <- plot_data(output = "dave_soil_state", variable = c("wmass", "temp"))
    expect_warning(labels <- .get_plot_label_maps(data, "output", "site"),
                   "Multiple units for plotted columns in outputs: dave_soil_state",
                   fixed = TRUE)
    expect_identical(labels$output, c(dave_soil_state = "Soil"))
    expect_identical(labels$variable, c(wmass = "wmass (mm)", temp = "temp (°C)"))
    expect_warning(.get_plot_label_maps(data, "site", "output"),
                   "Multiple units for plotted columns in outputs")
    expect_no_warning(.get_plot_label_maps(data, c("output", "variable"), "site"))
})

test_that("shared variables with different units warn and omit shared units", {
    data <- data.table::rbindlist(list(plot_data(),
        plot_data(output = "dave_cmass")))
    expect_warning(labels <- .get_plot_label_maps(data, "output", "variable"),
                   "Multiple units across outputs for columns: MRS", fixed = TRUE)
    expect_identical(labels$variable, c(MRS = "MRS"))
    expect_identical(labels$output,
                      c(dave_lai = "LAI (m2/m2)", dave_cmass = "C Mass (kgC/m2)"))
    expect_identical(labels$pairs$label, c("MRS (m2/m2)", "MRS (kgC/m2)"))
    expect_no_warning(.get_plot_label_maps(data, c("output", "variable"), "site"))
    expect_warning(.get_plot_label_maps(data, "variable", "site"),
                   "Multiple units across outputs for columns: MRS", fixed = TRUE)
})

test_that("conflicting variable names warn only when a shared name is needed", {
    local_output_registry()
    for (id in c("first", "second")) {
        add_output_dynamic(paste0("file_", id), id, id, "mm", "patch", "daily",
                           local({ name <- id; function(x) paste(name, x) }))
    }
    data <- data.table::rbindlist(list(plot_data(output = "first"),
                                      plot_data(output = "second")))
    expect_warning(labels <- .get_plot_label_maps(data, "variable", "site"),
                   "Conflicting column names across outputs for: MRS", fixed = TRUE)
    expect_identical(labels$variable, c(MRS = "MRS (mm)"))
    expect_identical(labels$pairs$label, c("first MRS (mm)", "second MRS (mm)"))
    expect_no_warning(.get_plot_label_maps(data, c("output", "variable"), "site"))
})

test_that("duplicate output display names remain distinguishable", {
    data <- data.table::rbindlist(list(plot_data(), plot_data(output = "dave_alai")))
    labels <- .get_plot_label_maps(data, "output", "variable")
    expect_identical(labels$output, c(dave_lai = "LAI (m2/m2) [dave_lai]",
                                      dave_alai = "LAI (m2/m2) [dave_alai]"))
    expect_identical(labels$output_name, c(dave_lai = "LAI [dave_lai]",
                                           dave_alai = "LAI [dave_alai]"))
})

test_that("duplicate column display names are disambiguated within each output", {
    local_output_registry()
    add_output_dynamic("file_test", "Test", "Test", "mm", "patch", "daily",
                       function(x) "Water")
    data <- plot_data(output = "test", variable = c("a", "b"))
    labels <- .get_plot_label_maps(data, c("output", "variable"), "variable")
    expect_identical(labels$variable, c(a = "Water (mm) [a]", b = "Water (mm) [b]"))
    expect_identical(labels$pairs$label, c("Water (mm) [a]", "Water (mm) [b]"))
})
