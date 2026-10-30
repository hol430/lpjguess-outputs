.sanitise_repo <- function(repo) {
    repo <- normalizePath(repo)
    if (!dir.exists(repo)) {
        stop("Repository does not exist: ", repo)
    }
    if (!file.exists(file.path(repo, "benchmarks", "ozflux", "settings.ins"))) {
        stop("Repository is missing settings.ins: ", repo)
    }
    return(repo)
}

.sanitise_output <- function(output) {
    output <- sub("^file_", "", output)
    output <- sub("\\.out$", "", output)
    return(output)
}

.get_out_file <- function(repo, output, site) {
    # TODO: parse instruction file (or expose parser from lpjguessRunner).
    out_dir <- file.path(repo, "benchmarks", "ozflux", site, "out")
    if (!dir.exists(out_dir)) {
        stop("Site is missing output directory: ", site)
    }

    out_file <- file.path(out_dir, paste0(output, ".out"))
    if (!file.exists(out_file)) {
        stop("Output file does not exist: ", out_file)
    }

    return(out_file)
}

.identity_cols <- c("Lon", "Lat", "Year", "Day", "timestep", "Date", "stand",
                    "patch", "pft", "indiv", "layer", "depth_top",
                    "depth_bottom")

.calc_date <- function(dt) {
    cols <- names(dt)
    # Multiple layers, patches, or individuals can share one timestamp.
    if ("Year" %in% cols && "Day" %in% cols && "timestep" %in% cols) {
        dates <- unique(dt[, .(Year, Day, timestep)])
        dates[, Date := as.POSIXct(paste(Year, Day, timestep),
                                  format = "%Y %j %H")]
        dt[, Date := dates[dt, on = .(Year, Day, timestep), Date]]
        dt$Year <- NULL
        dt$Day <- NULL
        dt$timestep <- NULL
    } else if ("Year" %in% cols && "Day" %in% cols) {
        dates <- unique(dt[, .(Year, Day)])
        dates[, Date := as.POSIXct(paste(Year, Day + 1),
                                  format = "%Y %j")]
        dt[, Date := dates[dt, on = .(Year, Day), Date]]
        dt$Year <- NULL
        dt$Day <- NULL
    } else if ("Year" %in% cols) {
        dates <- unique(dt[, .(Year)])
        dates[, Date := as.POSIXct(paste0(Year, "-12-31"),
                                  format = "%Y-%m-%d")]
        dt[, Date := dates[dt, on = "Year", Date]]
        dt$Year <- NULL
    } else {
        stop("Cannot calculate date from columns: ",
             paste(cols, collapse = ", "))
    }
    cols <- names(dt)
    data.table::setcolorder(dt, c("Lon", "Lat", "Date",
                                  setdiff(cols, c("Lon", "Lat", "Date"))))
    return(dt)
}

.read_dave_single <- function(repo, output, site) {
    out_file <- .get_out_file(repo, output, site)
    dt <- data.table::fread(out_file)

    dt <- .calc_date(dt)

    # Pivot to long format.
    data_cols <- setdiff(names(dt), .identity_cols)
    id_cols <- intersect(.identity_cols, names(dt))

    dt <- data.table::melt(dt, id.vars = id_cols,
                           measure.vars = data_cols,
                           variable.name = "variable", value.name = "value")

    dt$repo <- repo
    dt$output <- output
    dt$site <- site

    return(dt)
}

#' Read OzFlux site outputs from a DAVE repository.
#'
#' @param repos Path(s) to top-level LPJ-GUESS DAVE repositories.
#' @param outputs Output file stem(s), such as `"dave_lai"`.
#' @param sites OzFlux site directory name(s), such as `"CumberlandPlain"`.
#'
#' @return A long-form `data.table` containing the requested outputs, with
#'   `variable` and `value` columns plus the available site, time, and
#'   dimension columns.
#' @seealso [plot_dave()], `vignette("plotting-dave")`
#' @export
read_dave <- function(repos, outputs, sites) {
    repos <- vapply(repos, .sanitise_repo, character(1))
    outputs <- vapply(outputs, .sanitise_output, character(1))
    data <- data.table::rbindlist(lapply(repos, function(repo) {
        data.table::rbindlist(lapply(outputs, function(output) {
            data.table::rbindlist(lapply(sites, function(site) {
                .read_dave_single(repo, output, site)
            }), use.names = TRUE, fill = TRUE)
        }), use.names = TRUE, fill = TRUE)
    }), use.names = TRUE, fill = TRUE)
    return(data)
}
