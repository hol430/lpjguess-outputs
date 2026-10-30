test_that("repository validation accepts and normalises a DAVE repository", {
    repo <- local_dave_repo()
    expect_identical(.sanitise_repo(file.path(repo, ".")), repo)
})

test_that("repository validation reports missing paths and settings", {
    repo <- local_dave_repo()
    missing <- file.path(repo, "missing")
    expect_error(suppressWarnings(.sanitise_repo(missing)),
                 "Repository does not exist", fixed = TRUE)
    expect_error(.sanitise_repo(file.path(repo, "benchmarks")),
                 "Repository is missing settings.ins", fixed = TRUE)
    expect_error(.sanitise_repo(file.path(repo, "benchmarks", "ozflux",
                                         "settings.ins")),
                 "Repository does not exist", fixed = TRUE)
})

test_that("output names accept stems, instruction IDs and out extensions", {
    expect_identical(.sanitise_output(c("dave_lai", "file_dave_lai",
        "dave_lai.out", "file_dave_lai.out")), rep("dave_lai", 4L))
    expect_identical(.sanitise_output(c("profile_lai", "lai.out.backup",
        "laiXout", "lai.OUT")),
        c("profile_lai", "lai.out.backup", "laiXout", "lai.OUT"))
})

test_that("output paths distinguish missing site directories and files", {
    repo <- local_dave_repo()
    expect_error(.get_out_file(repo, "dave_lai", "MissingSite"),
                 "Site is missing output directory: MissingSite", fixed = TRUE)
    expect_error(.get_out_file(repo, "dave_lai", "TestSite"),
                 "Output file does not exist:", fixed = TRUE)
    path <- write_dave_output(repo, data.frame(Lon = 150, Lat = -33,
                                              Year = 2024, MRS = 2))
    expect_identical(.get_out_file(repo, "dave_lai", "TestSite"), path)
})

test_that("annual dates are year end and retain row order and data", {
    withr::local_envvar(c(TZ = "UTC"))
    dt <- data.table::data.table(value = c(3, 1, 2), Year = c(2024L, 2023L, 2024L),
                                 Lat = -33, Lon = 150, patch = c(1L, 0L, 0L))
    result <- .calc_date(dt)
    expect_s3_class(result$Date, "POSIXct")
    expect_equal(result$Date, as.POSIXct(c("2024-12-31", "2023-12-31",
                                          "2024-12-31"), tz = ""))
    expect_identical(names(result), c("Lon", "Lat", "Date", "value", "patch"))
    expect_identical(result$value, c(3, 1, 2))
    expect_identical(result$patch, c(1L, 0L, 0L))
})

test_that("daily dates use zero-based days including leap days and year end", {
    withr::local_envvar(c(TZ = "UTC"))
    dt <- data.table::data.table(Lon = 150, Lat = -33,
        Year = c(2024L, 2024L, 2024L, 2023L, 2024L),
        Day = c(365L, 0L, 59L, 364L, 59L), layer = c(0L, 0L, 0L, 0L, 1L),
        value = seq_len(5L))
    result <- .calc_date(dt)
    expect_equal(result$Date, as.POSIXct(c("2024-12-31", "2024-01-01",
        "2024-02-29", "2023-12-31", "2024-02-29"), tz = ""))
    expect_false(any(c("Year", "Day") %in% names(result)))
    expect_identical(result$layer, c(0L, 0L, 0L, 0L, 1L))
    expect_identical(result$value, seq_len(5L))
})

test_that("subdaily dates use one-based days and hour timesteps", {
    withr::local_envvar(c(TZ = "UTC"))
    dt <- data.table::data.table(Lon = 150, Lat = -33, Year = 2024L,
        Day = c(60L, 1L, 1L, 1L), timestep = c(12L, 23L, 0L, 23L),
        indiv = c(1L, 1L, 1L, 2L), value = seq_len(4L))
    result <- .calc_date(dt)
    expect_equal(result$Date, as.POSIXct(c("2024-02-29 12:00:00",
        "2024-01-01 23:00:00", "2024-01-01 00:00:00",
        "2024-01-01 23:00:00"), tz = ""))
    expect_false(any(c("Year", "Day", "timestep") %in% names(result)))
    expect_identical(result$indiv, c(1L, 1L, 1L, 2L))
})

test_that("date calculation explains unsupported time columns", {
    dt <- data.table::data.table(Lon = 150, Lat = -33, Day = 0L, value = 2)
    expect_error(.calc_date(dt),
                 "Cannot calculate date from columns: Lon, Lat, Day, value",
                 fixed = TRUE)
})

test_that("reading preserves every identity dimension and melts only data", {
    repo <- local_dave_repo()
    source <- data.frame(Lon = 150, Lat = -33, Year = 2024L,
        Day = c(0L, 0L), stand = 0L, patch = c(0L, 1L), pft = "MRS",
        indiv = c(1L, 2L), layer = c(0L, 1L), depth_top = c(0, 100),
        depth_bottom = c(100, 300), wmass = c(0, NA_real_), psi_mpa = c(-1, -2))
    write_dave_output(repo, source, "dave_soil_state")
    result <- read_dave(repo, "file_dave_soil_state.out", "TestSite")
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 4L)
    expect_setequal(as.character(result$variable), c("wmass", "psi_mpa"))
    expect_equal(result[as.character(result$variable) == "wmass"]$value,
                 c(0, NA_real_))
    expect_equal(result[as.character(result$variable) == "psi_mpa"]$value,
                 c(-1, -2))
    for (column in c("Lon", "Lat", "stand", "patch", "pft", "indiv",
                     "layer", "depth_top", "depth_bottom")) {
        expect_equal(result[[column]], rep(source[[column]], 2L), info = column)
    }
    expect_identical(result$repo, rep(repo, 4L))
    expect_identical(result$output, rep("dave_soil_state", 4L))
    expect_identical(result$site, rep("TestSite", 4L))
    expect_equal(result$Date, rep(as.POSIXct("2024-01-01", tz = ""), 4L))
    expect_false(any(c("Year", "Day") %in% names(result)))
})

test_that("reading combines every repository, output and site requested", {
    sites <- c("SiteA", "SiteB")
    repos <- c(local_dave_repo(sites), local_dave_repo(sites))
    outputs <- c("dave_lai", "dave_cmass")
    expected <- expand.grid(repo = repos, output = outputs, site = sites,
                            stringsAsFactors = FALSE)
    expected$value <- seq_len(nrow(expected))
    for (i in seq_len(nrow(expected))) {
        write_dave_output(expected$repo[i], data.frame(Lon = 150, Lat = -33,
            Year = 2024L, MRS = expected$value[i]), expected$output[i],
            expected$site[i])
    }
    result <- read_dave(repos, outputs, sites)
    actual <- as.data.frame(result[, c("repo", "output", "site", "value"),
                                   with = FALSE])
    ordering <- function(x) order(x$repo, x$output, x$site)
    expect_equal(actual[ordering(actual), ], expected[ordering(expected), ],
                  ignore_attr = TRUE)
})

test_that("heterogeneous output dimensions are filled without losing rows", {
    repo <- local_dave_repo()
    write_dave_output(repo, data.frame(Lon = 150, Lat = -33, Year = 2024L,
        Day = 0L, layer = 0L, depth_top = 0, depth_bottom = 100, wmass = 42),
        "dave_soil_state")
    write_dave_output(repo, data.frame(Lon = 150, Lat = -33, Year = 2024L,
                                      MRS = 3))
    result <- read_dave(repo, c("dave_soil_state", "dave_lai"), "TestSite")
    expect_equal(nrow(result), 2L)
    expect_equal(result[output == "dave_soil_state"]$layer, 0L)
    expect_true(is.na(result[output == "dave_lai"]$layer))
    expect_true(is.na(result[output == "dave_lai"]$depth_bottom))
    expect_equal(result$value, c(42, 3))
})

test_that("subdaily files are read without melting timestamps", {
    repo <- local_dave_repo()
    write_dave_output(repo, data.frame(Lon = 150, Lat = -33, Year = 2024L,
        Day = 1L, timestep = c(0L, 12L), MRS = c(2, 4)))
    result <- read_dave(repo, "dave_lai", "TestSite")
    expect_equal(result$Date, as.POSIXct(c("2024-01-01 00:00:00",
                                          "2024-01-01 12:00:00"), tz = ""))
    expect_identical(as.character(result$variable), rep("MRS", 2L))
    expect_equal(result$value, c(2, 4))
})

test_that("reader propagates invalid files and missing date columns", {
    repo <- local_dave_repo()
    expect_error(read_dave(repo, "missing", "TestSite"),
                 "Output file does not exist", fixed = TRUE)
    expect_error(read_dave(repo, "dave_lai", "missing"),
                 "Site is missing output directory", fixed = TRUE)
    write_dave_output(repo, data.frame(Lon = 150, Lat = -33, MRS = 1))
    expect_error(read_dave(repo, "dave_lai", "TestSite"),
                 "Cannot calculate date from columns", fixed = TRUE)
})

test_that("empty selections return empty data tables", {
    repo <- local_dave_repo()
    selections <- list(list(character(), "dave_lai", "TestSite"),
                       list(repo, character(), "TestSite"),
                       list(repo, "dave_lai", character()))
    for (selection in selections) {
        result <- do.call(read_dave, selection)
        expect_s3_class(result, "data.table")
        expect_equal(nrow(result), 0L)
    }
})
