# Run from the repository root. The hook and CI share this entry point.
description <- read.dcf("DESCRIPTION")
field <- "Config/roxygen2/version"
if (!(field %in% colnames(description)) || !nzchar(description[1L, field])) {
    stop("Set Config/roxygen2/version in DESCRIPTION before generating docs.",
         call. = FALSE)
}
required <- description[1L, field]
if (!requireNamespace("roxygen2", quietly = TRUE) ||
    as.character(utils::packageVersion("roxygen2")) != required) {
    stop("Documentation requires roxygen2 ", required, ". Install it with:\n",
         "  pak::pkg_install(\"roxygen2@", required, "\")", call. = FALSE)
}

roxygen2::roxygenise()

if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
    git_output <- function(args) {
        output <- system2("git", args, stdout = TRUE)
        status <- attr(output, "status")
        if (!is.null(status) && status != 0L) {
            stop("Could not inspect generated documentation with Git.",
                 call. = FALSE)
        }
        output
    }
    changed <- git_output(c("diff", "--name-only", "--", "DESCRIPTION",
                            "NAMESPACE", "man/"))
    untracked <- git_output(c("ls-files", "--others", "--exclude-standard",
                              "--", "NAMESPACE", "man/"))
    paths <- unique(c(changed, untracked))
    if (length(paths) > 0L) {
        stop("Generated documentation needs to be staged or committed:\n  ",
             paste(paths, collapse = "\n  "),
             "\nReview the changes, run git add DESCRIPTION NAMESPACE man, ",
             "then retry the commit.", call. = FALSE)
    }
}
