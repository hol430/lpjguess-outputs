# Keep fixtures independent of real simulation runs and the local time zone.
local_dave_repo <- function(sites = "TestSite", .local_envir = parent.frame()) {
    withr::local_envvar(c(TZ = "UTC"), .local_envir = .local_envir)
    repo <- withr::local_tempdir(pattern = "dave-test-",
                                .local_envir = .local_envir)
    ozflux <- file.path(repo, "benchmarks", "ozflux")
    dir.create(ozflux, recursive = TRUE)
    writeLines("! Temporary test repository", file.path(ozflux, "settings.ins"))
    for (site in sites) {
        dir.create(file.path(ozflux, site, "out"), recursive = TRUE)
    }
    normalizePath(repo)
}

write_dave_output <- function(repo, data, output = "dave_lai",
                              site = "TestSite") {
    path <- file.path(repo, "benchmarks", "ozflux", site, "out",
                      paste0(output, ".out"))
    data.table::fwrite(data, path, sep = " ", na = "NA", quote = FALSE)
    invisible(path)
}

# Restore the registry even if a test fails; do not use the function under test
# to set up or tear down registry tests.
local_output_registry <- function(empty = TRUE, .local_envir = parent.frame()) {
    registry <- lpjguessOutput:::.output_registry
    saved <- as.list(registry, all.names = TRUE)
    withr::defer({
        rm(list = ls(registry, all.names = TRUE), envir = registry)
        list2env(saved, envir = registry)
    }, envir = .local_envir)
    if (empty) {
        rm(list = ls(registry, all.names = TRUE), envir = registry)
    }
    invisible(registry)
}

example_metadata <- function(...) {
    args <- utils::modifyList(list(
        id = "file_test", name = "Test quantity", description = "Test output",
        units = "mm", aggregation_level = "patch", temporal_resolution = "daily"
    ), list(...), keep.null = TRUE)
    do.call(lpjguessOutput:::create_output_metadata, args)
}

plot_data <- function(output = "dave_lai", variable = "MRS", ...) {
    dimensions <- utils::modifyList(list(
        Date = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC"),
        repo = "run", site = "TestSite", output = output, variable = variable,
        Lon = 150, Lat = -33, stand = 0L, patch = 0L, pft = "MRS",
        indiv = 1L, layer = 0L
    ), list(...))
    data <- data.table::as.data.table(do.call(expand.grid,
        c(dimensions, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))))
    data$value <- seq_len(nrow(data))
    data
}

facet_text <- function(plot, ...) {
    labels <- data.frame(..., stringsAsFactors = FALSE)
    unname(unlist(plot$facet$params$labeller(labels), use.names = FALSE))
}
