.require_runner <- function() {
    if (!requireNamespace("lpjguessRunner", quietly = TRUE)) {
        stop("Install lpjguessRunner to work with runner outputs. See ?read_runner.",
             call. = FALSE)
    }
}

.runner_empty_runs <- function() {
    data.frame(simulation = character(), instruction_file = character(),
        path = character(), base_ins = character(), ins_file = character(),
        key = character(), factors = I(list()), run_id = character())
}

.runner_runs <- function(x, simulations = NULL, instruction_files = NULL,
                         parameters = NULL) {
    .require_runner()
    runs <- if (is.data.frame(x)) as.data.frame(x) else
        as.data.frame(lpjguessRunner::list_simulations(x))
    if (!nrow(runs)) return(.runner_empty_runs())
    required <- c("simulation", "instruction_file", "path", "base_ins", "ins_file", "key", "factors")
    if (!all(required %in% names(runs))) {
        stop("Expected a simulation table from lpjguessRunner::list_simulations().",
             call. = FALSE)
    }
    runs$run_id <- normalizePath(runs$path, mustWork = FALSE)
    runs <- runs[!duplicated(runs$run_id), , drop = FALSE]
    # A basename is a label, not an instruction-file identity.
    instructions <- unique(runs[c("instruction_file", "base_ins")])
    duplicate <- duplicated(instructions$instruction_file) |
        duplicated(instructions$instruction_file, fromLast = TRUE)
    ambiguous <- instructions$instruction_file[duplicate]
    labels <- runs$instruction_file
    runs$instruction_file[labels %in% ambiguous] <- paste0(
        labels[labels %in% ambiguous], " [", runs$base_ins[labels %in% ambiguous], "]")
    attr(runs, "runner_parameter_names") <- unique(c(attr(x, "runner_parameter_names"),
        .runner_factor_table(runs)$parameter))
    if (!is.null(simulations)) {
        .runner_check_choices(simulations, unique(runs$simulation), "simulations")
        runs <- runs[runs$simulation %in% simulations, , drop = FALSE]
    }
    if (!is.null(instruction_files)) {
        .runner_check_choices(instruction_files,
            unique(c(runs$instruction_file, runs$base_ins)), "instruction_files")
        runs <- runs[runs$instruction_file %in% instruction_files |
                         runs$base_ins %in% instruction_files, , drop = FALSE]
    }
    if (!is.null(parameters)) {
        if (!is.list(parameters) || is.null(names(parameters)) ||
            anyNA(names(parameters)) || any(!nzchar(names(parameters))) ||
            anyDuplicated(names(parameters))) {
            stop("parameters must be a named list of scoped parameter names and allowed values.",
                 call. = FALSE)
        }
        values <- .runner_parameter_values(runs)
        .runner_check_choices(names(parameters), sub("^param:", "", names(values)[-1L]), "parameters")
        keep <- rep(TRUE, nrow(runs))
        for (parameter in names(parameters)) {
            keep <- keep & values[[paste0("param:", parameter)]] %in%
                as.character(parameters[[parameter]])
        }
        runs <- runs[keep, , drop = FALSE]
    }
    rownames(runs) <- NULL
    runs
}

.runner_check_choices <- function(values, choices, argument) {
    if (!is.character(values) || anyNA(values) || any(!nzchar(values))) {
        stop(argument, " must contain non-empty character values.", call. = FALSE)
    }
    unknown <- setdiff(values, choices)
    if (length(unknown)) {
        stop("Unknown ", argument, ": ", paste(unknown, collapse = ", "),
             ". Available: ", paste(choices, collapse = ", "), call. = FALSE)
    }
}

.runner_factor_table <- function(runs) {
    empty <- data.table::data.table(run_id = character(), simulation = character(),
        instruction_file = character(), parameter = character(), type = character(),
        block_type = character(), block_name = character(), name = character(), value = character())
    rows <- list(empty)
    for (i in seq_len(nrow(runs))) {
        for (factor in runs$factors[[i]]) {
            if (is.null(factor$name) || !nzchar(factor$name) || identical(factor$type, "dummy")) next
            block <- identical(factor$type, "block")
            parameter <- if (block) paste(factor$block_type, factor$block_name, factor$name, sep = ":") else factor$name
            rows[[length(rows) + 1L]] <- data.table::data.table(
                run_id = runs$run_id[[i]], simulation = runs$simulation[[i]],
                instruction_file = runs$instruction_file[[i]], parameter = parameter,
                type = factor$type, block_type = if (block) factor$block_type else NA_character_,
                block_name = if (block) factor$block_name else NA_character_,
                name = factor$name, value = as.character(factor$value))
        }
    }
    result <- data.table::rbindlist(rows)
    result[!duplicated(result[, c("run_id", "parameter"), with = FALSE], fromLast = TRUE)]
}

.runner_parameter_values <- function(runs) {
    factors <- .runner_factor_table(runs)
    values <- data.frame(run_id = runs$run_id, stringsAsFactors = FALSE)
    for (parameter in unique(c(attr(runs, "runner_parameter_names"), factors$parameter))) {
        indices <- which(factors$parameter == parameter)
        rows <- factors[indices]
        values[[paste0("param:", parameter)]] <- rows$value[match(runs$run_id, rows$run_id)]
    }
    values
}

#' Inspect parameter overrides in runner simulations.
#'
#' @param x Runner output directory, settings object, `lpjguess_result`, or a
#'   (possibly filtered) table from `lpjguessRunner::list_simulations()`.
#' @return A `data.table` with one row per job and overridden parameter. The
#'   `parameter` column contains top-level names such as `npatch` or scoped names
#'   such as `pft:MRS:sla`. Values are preserved as strings. These are overrides,
#'   not a complete inventory of effective instruction-file settings.
#' @seealso [read_runner()], [plot_runner()], [runner_shiny()]
#' @export
runner_parameters <- function(x) {
    .runner_factor_table(.runner_runs(x))
}

.runner_check_threshold <- function(stale_threshold_secs) {
    if (!is.numeric(stale_threshold_secs) || length(stale_threshold_secs) != 1L ||
        is.na(stale_threshold_secs) || stale_threshold_secs < 0) {
        stop("stale_threshold_secs must be a non-negative number (or Inf to disable freshness filtering).",
             call. = FALSE)
    }
}

.runner_empty_inventory <- function() {
    data.frame(run_id = character(), simulation = character(), instruction_file = character(),
        base_ins = character(), ins_file = character(), output_type = character(),
        file = character(), path = character(), simulation_key = character(),
        output = character(), size = numeric(), mtime = numeric(), status = character())
}

.runner_inventory <- function(runs, stale_threshold_secs) {
    .runner_check_threshold(stale_threshold_secs)
    rows <- list(.runner_empty_inventory())
    for (i in seq_len(nrow(runs))) {
        # Discover all outputs before selecting quantities: freshness is relative
        # to this job's newest .out, never to a different job's finish time.
        files <- as.data.frame(lpjguessRunner::list_outputs(runs[i, , drop = FALSE]))
        if (!nrow(files)) next
        info <- file.info(files$path)
        keep <- !is.na(info$isdir) & !info$isdir & !is.na(info$mtime)
        files <- files[keep, , drop = FALSE]
        info <- info[keep, , drop = FALSE]
        if (!nrow(files)) next
        files$run_id <- runs$run_id[[i]]
        files$instruction_file <- runs$instruction_file[[i]]
        files$output <- ifelse(is.na(files$output_type) | !nzchar(files$output_type),
                               .sanitise_output(files$file), .sanitise_output(files$output_type))
        files$size <- info$size
        files$mtime <- as.numeric(info$mtime)
        age <- max(files$mtime) - files$mtime
        files$status <- ifelse(age <= stale_threshold_secs, "Available", "Stale")
        files$status[info$size == 0] <- "Empty"
        files$status[file.access(files$path, 4L) != 0L] <- "Unreadable"
        rows[[length(rows) + 1L]] <- files[names(rows[[1L]])]
    }
    do.call(rbind, rows)
}

.runner_common_outputs <- function(files, runs) {
    if (!nrow(runs)) return(character())
    sort(Reduce(intersect, lapply(runs$run_id, function(id) {
        files$output[files$run_id == id & files$status == "Available"]
    })))
}

.runner_select_files <- function(files, runs, outputs, missing) {
    if (!is.character(outputs) || !length(outputs) || anyNA(outputs) || any(!nzchar(outputs))) {
        stop("outputs must be a non-empty character vector.", call. = FALSE)
    }
    selected <- integer()
    problems <- character()
    for (i in seq_len(nrow(runs))) {
        for (output in outputs) {
            stem <- .sanitise_output(basename(output))
            matches <- which(files$run_id == runs$run_id[[i]] &
                (files$output == stem | .sanitise_output(files$file) == stem | files$path == output))
            if (length(matches) > 1L) {
                stop("Output '", output, "' matches multiple files for ", runs$run_id[[i]],
                     "; select an unambiguous filename.", call. = FALSE)
            }
            status <- if (length(matches)) files$status[matches] else "Missing"
            if (status == "Available") {
                selected <- c(selected, matches)
            } else {
                problems <- c(problems, paste0(runs$instruction_file[[i]], "/",
                    runs$simulation[[i]], ": ", output, " (", status, ")"))
            }
        }
    }
    if (length(problems) && missing != "ignore") {
        message <- paste0("Unavailable runner outputs:\n  ", paste(problems, collapse = "\n  "))
        if (missing == "error") stop(message, call. = FALSE)
        warning(message, call. = FALSE)
    }
    selected <- unique(selected)
    if (!length(selected)) stop("No readable, non-empty, fresh runner outputs selected.", call. = FALSE)
    files[selected, , drop = FALSE]
}

.runner_long_output <- function(dt) {
    # Standard monthly files store a year of observations across twelve columns.
    # Annual summaries such as Total and MAXALD remain separate year-end series.
    months <- month.abb
    if ("Year" %in% names(dt) && !("Day" %in% names(dt)) && all(months %in% names(dt))) {
        ids <- intersect(.identity_cols, names(dt))
        monthly <- data.table::melt(dt, id.vars = ids, measure.vars = months,
            variable.name = ".month", value.name = "value")
        monthly$Date <- as.POSIXct(sprintf("%04d-%02d-01", monthly$Year,
            match(as.character(monthly$.month), months)), format = "%Y-%m-%d")
        monthly$Year <- monthly$.month <- NULL
        monthly$variable <- "monthly"
        extra <- setdiff(names(dt), c(ids, months))
        if (!length(extra)) return(monthly)
        annual <- .long_output(dt[, c(ids, extra), with = FALSE])
        return(data.table::rbindlist(list(monthly, annual), use.names = TRUE, fill = TRUE))
    }
    .long_output(dt)
}

.runner_read_files <- function(files, runs) {
    parameter_values <- .runner_parameter_values(runs)
    tables <- lapply(seq_len(nrow(files)), function(i) {
        file <- files[i, , drop = FALSE]
        class(file) <- c("lpjguess_outputs", "data.frame")
        raw <- lpjguessRunner::read_output(file, file$file, combine = FALSE, id_cols = FALSE)[[1L]]
        if (!nrow(raw)) stop("Selected output file contains no data rows: ", file$path, call. = FALSE)
        dt <- .runner_long_output(data.table::as.data.table(raw))
        for (column in c("run_id", "simulation", "instruction_file", "base_ins", "ins_file", "simulation_key", "output")) {
            dt[[column]] <- rep(file[[column]], nrow(dt))
        }
        dt$output_file <- rep(file$file, nrow(dt))
        dt$output_path <- rep(file$path, nrow(dt))
        row <- match(file$run_id, parameter_values$run_id)
        for (column in setdiff(names(parameter_values), "run_id")) {
            dt[[column]] <- rep(parameter_values[[column]][row], nrow(dt))
        }
        dt
    })
    data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)
}

#' Read outputs from runner-managed simulations.
#'
#' Uses the optional package \pkg{lpjguessRunner} to discover catalogued jobs
#' and read their output files. Jobs are assumed to have completed successfully.
#'
#' @inheritParams runner_parameters
#' @param outputs Output IDs, filenames, or stems, for example `file_dave_lai`,
#'   `dave_lai.out`, or `dave_lai`. Configured filenames are resolved through the runner.
#' @param simulations Optional simulation names. A name selects every matching
#'   instruction-file job in the supplied catalog.
#' @param instruction_files Optional instruction-file labels or full `base_ins`
#'   paths. Duplicate labels are disambiguated with their base paths.
#' @param parameters Optional named list of allowed parameter override values,
#'   e.g. `list("pft:MRS:sla" = c(26, 39))`. Use `NA_character_` to select jobs
#'   where a known parameter was not overridden.
#' @param stale_threshold_secs Non-negative freshness window in seconds,
#'   default 10. Compare each file to the newest `.out` in its own job's output
#'   directory. Set to `Inf` to disable freshness filtering. This is a timestamp
#'   heuristic and does not establish successful completion.
#' @param missing How to handle missing, stale, empty, or unreadable requested
#'   outputs: `"error"` (default), `"warn"`, or `"ignore"`. The latter two read
#'   available files; an empty selection always errors.
#' @return A long-form `data.table` with `Date`, `variable`, `value`, model
#'   dimensions, job identifiers, output paths, and string-valued parameter
#'   columns named `param:<parameter>`. Missing overrides are `NA`, not inferred
#'   base values. No observation rows are duplicated for parameter metadata.
#' @details Annual, daily, and subdaily output layouts use the same conventions
#'   as [read_dave()], with zero-based `Day` values. Standard `Jan`--`Dec` monthly
#'   columns become a `monthly` variable dated at month starts; other columns
#'   such as `Total` remain separate annual variables dated at year end.
#'   Normalize each file before combining, so heterogeneous schemas do not
#'   generate measurements for columns absent from a file.
#' @seealso [plot_runner()], [runner_shiny()], [runner_parameters()]
#' @examples
#' \dontrun{
#' runs <- lpjguessRunner::list_simulations("/path/to/runner-output")
#' lai <- read_runner(runs, "dave_lai")
#' runner_parameters(runs)
#' }
#' @export
read_runner <- function(x, outputs, simulations = NULL, instruction_files = NULL,
                        parameters = NULL, stale_threshold_secs = 10,
                        missing = c("error", "warn", "ignore")) {
    missing <- match.arg(missing)
    .runner_check_threshold(stale_threshold_secs)
    runs <- .runner_runs(x, simulations, instruction_files, parameters)
    if (!nrow(runs)) stop("No runner simulations selected.", call. = FALSE)
    inventory <- .runner_inventory(runs, stale_threshold_secs)
    files <- .runner_select_files(inventory, runs, outputs, missing)
    .runner_read_files(files, runs)
}

.runner_dimensions <- function(dt) {
    choices <- c("column", "instruction_file", "simulation", "output", "run_id",
                 "patch", "stand", "pft", "indiv", "layer")
    mapped <- choices
    mapped[mapped == "column"] <- "variable"
    c(choices[mapped %in% names(dt)], names(dt)[startsWith(names(dt), "param:")])
}

.runner_dimension <- function(value, dt, multiple = FALSE) {
    result <- match.arg(value, .runner_dimensions(dt), several.ok = multiple)
    result[result == "column"] <- "variable"
    result
}

.runner_column_sets <- function(dt) {
    per_file <- dt[, list(columns = list(unique(as.character(get("variable"))))),
                   by = c("run_id", "output")]
    list(common = Reduce(intersect, per_file$columns),
         all = sort(unique(as.character(dt$variable))))
}

.runner_plot <- function(dt, columns, common_yaxis, facet, colour) {
    if (is.null(columns)) {
        columns <- .runner_column_sets(dt)$common
        if (!length(columns)) stop("No data columns are shared by every selected runner output; provide columns explicitly.", call. = FALSE)
    }
    .plot(dt, columns, common_yaxis, .runner_dimension(facet, dt, TRUE),
          .runner_dimension(colour, dt))
}

#' Plot outputs from runner-managed simulations.
#'
#' @inheritParams read_runner
#' @inheritParams plot_dave
#' @param facet One or more available model or runner dimensions: `column`,
#'   `instruction_file`, `simulation`, `output`, `run_id`, `patch`, `stand`,
#'   `pft`, `indiv`, `layer`, or a `param:<parameter>` column returned by
#'   [read_runner()].
#' @param colour One available dimension, using the same names as `facet`.
#' @return A ggplot2 object. Series from different jobs always remain separate,
#'   even when those jobs share the same colour and facet values.
#' @seealso [read_runner()], [runner_parameters()], [runner_shiny()]
#' @examples
#' \dontrun{
#' plot_runner("/path/to/runner-output", "dave_lai", columns = "total")
#' plot_runner("/path/to/runner-output", "dave_lai", columns = "total",
#'             colour = "param:pft:MRS:sla")
#' }
#' @export
plot_runner <- function(x, outputs, simulations = NULL, instruction_files = NULL,
                        parameters = NULL, columns = NULL,
                        common_yaxis = length(outputs) > 1L,
                        facet = c("instruction_file", "output", "column"),
                        colour = "simulation", stale_threshold_secs = 10,
                        missing = c("error", "warn", "ignore")) {
    dt <- read_runner(x, outputs, simulations, instruction_files, parameters,
                      stale_threshold_secs, match.arg(missing))
    .runner_plot(dt, columns, common_yaxis, facet, colour)
}
