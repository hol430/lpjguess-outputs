local_runner_output <- function(jobs = data.frame(
    instruction = c("SiteA", "SiteA", "SiteB"),
    simulation = c("baseline", "treatment", "baseline")),
    factors = NULL, .local_envir = parent.frame()) {
    testthat::skip_if_not_installed("lpjguessRunner")
    withr::local_envvar(c(TZ = "UTC"), .local_envir = .local_envir)
    root <- withr::local_tempdir(pattern = "runner-test-", .local_envir = .local_envir)
    directories <- if (nrow(jobs)) paste0("job-", seq_len(nrow(jobs))) else character()
    quote_toml <- function(value) encodeString(value, quote = '"')
    writeLines(paste0("simulations = [", paste(quote_toml(directories), collapse = ", "), "]"),
               file.path(root, "index.toml"))
    for (i in seq_len(nrow(jobs))) {
        directory <- file.path(root, directories[[i]])
        dir.create(file.path(directory, "results"), recursive = TRUE)
        base <- if ("base_ins" %in% names(jobs)) jobs$base_ins[[i]] else
            file.path(root, "original-inputs", paste0(jobs$instruction[[i]], ".ins"))
        changes <- if (!is.null(factors)) factors[[i]] else list(
            lpjguessRunner::top_level_parameter("nindiv_max", if (jobs$simulation[[i]] == "baseline") 0 else 1),
            lpjguessRunner::block_parameter("pft", "MRS", "sla", if (jobs$simulation[[i]] == "baseline") 26 else 39))
        lines <- c(paste0("key = ", quote_toml(jobs$simulation[[i]])),
            paste0("name = ", quote_toml(jobs$simulation[[i]])),
            paste0("base_ins = ", quote_toml(base)), 'ins_file = "generated.ins"',
            'generated_at_utc = 2024-01-01T00:00:00Z', 'pfts = ["MRS"]')
        for (change in changes) {
            lines <- c(lines, "[[factors]]")
            for (key in intersect(c("block_type", "block_name", "name", "value"), names(change))) {
                lines <- c(lines, paste0(key, " = ", quote_toml(change[[key]])))
            }
        }
        writeLines(lines, file.path(directory, "manifest.toml"))
        writeLines(c('outputdirectory "results"', 'file_dave_lai "custom-lai.out"',
                     'file_dave_soil_state "soil.out"', 'file_mnpp "monthly.out"'),
                   file.path(directory, "generated.ins"))
        frame <- data.frame(Lon = 150, Lat = -33, Year = 2024L,
                            Day = 0:1, stand = 0L, patch = 0L, total = i + 0:1)
        data.table::fwrite(frame, file.path(directory, "results", "custom-lai.out"), sep = " ")
    }
    normalizePath(root)
}

write_runner_output <- function(root, data, job = 1L, filename = "custom-lai.out") {
    runs <- lpjguessRunner::list_simulations(root)
    path <- file.path(runs$path[[job]], "results", filename)
    data.table::fwrite(data, path, sep = " ", quote = FALSE)
    invisible(path)
}
