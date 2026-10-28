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

.identity_cols <- c("Lon", "Lat", "Year", "Day", "timestep", "stand", "patch",
                    "pft", "indiv")

.calc_date <- function(dt) {
    cols <- names(dt)
    if ("Year" %in% cols && "Day" %in% cols && "timestep" %in% cols) {
        dt[, Date := as.POSIXct(paste(Year, Day, timestep),
                                format = "%Y %j %H")]
        dt$Year <- NULL
        dt$Day <- NULL
        dt$timestep <- NULL
    } else if ("Year" %in% cols && "Day" %in% cols) {
        dt[, Date := as.POSIXct(paste(Year, Day + 1),
                                format = "%Y %j")]
        dt$Year <- NULL
        dt$Day <- NULL
    } else if ("Year" %in% cols) {
        dt[, Date := as.POSIXct(paste0(Year, "-12-31"),
                                format = "%Y-%m-%d")]
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

    # Pivot to long format.
    data_cols <- setdiff(names(dt), .identity_cols)
    id_cols <- intersect(.identity_cols, names(dt))

    dt <- data.table::melt(dt, id.vars = id_cols,
                           measure.vars = data_cols,
                           variable.name = "variable", value.name = "value")

    dt <- .calc_date(dt)

    dt$repo <- repo
    dt$output <- output
    dt$site <- site

    return(dt)
}

#' Read LPJ-GUESS outputs from a DAVE repository.
#'
#' @param repos Path(s) to one or more DAVE repositories.
#' @param outputs Output name(s) to read.
#' @param sites Site name(s) to read.
#'
#' @return a data.table containing the requested outputs.
#' @export
read_dave <- function(repos, outputs, sites) {
    repos <- vapply(repos, .sanitise_repo, character(1))
    outputs <- vapply(outputs, .sanitise_output, character(1))
    data <- data.table::rbindlist(lapply(repos, function(repo) {
        data.table::rbindlist(lapply(outputs, function(output) {
            data.table::rbindlist(lapply(sites, function(site) {
                .read_dave_single(repo, output, site)
            }))
        }))
    }))
    return(data)
}
