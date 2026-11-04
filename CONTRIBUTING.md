## Running the tests

From the repository root, install the test dependencies if needed:

```r
install.packages(c("testthat", "withr", "pkgload", "data.table", "ggplot2",
                   "shiny", "plotly"))
```

Run the complete suite against the local source:

```r
testthat::test_local()
```

`tests/testthat.R` also runs the suite against the installed package during
`R CMD check`.

## Documentation and the commit hook

Commit `NAMESPACE` and `man/` alongside the R code. Documentation generation
uses the roxygen2 version in `Config/roxygen2/version` in `DESCRIPTION`. To
install that version, run this from the repository root (install `pak` first if
needed):

```r
version <- read.dcf("DESCRIPTION", fields = "Config/roxygen2/version")[[1L]]
pak::pkg_install(paste0("roxygen2@", version))
```

Enable the versioned Git hook once in each clone:

```sh
git config --local core.hooksPath .githooks
```

For commits affecting package sources or documentation, the hook regenerates
`NAMESPACE`, `man/`, and any generated `DESCRIPTION` fields. If the results
differ from what is staged, it stops the commit. Review and stage the generated
changes, then retry:

```sh
git add DESCRIPTION NAMESPACE man
git commit
```

The hook leaves staging to you. It stops before generation if package sources
have unstaged changes, including partially staged R files and untracked R
sources. Stage or stash those edits before retrying. Commits affecting only
unrelated files skip documentation generation.

You can also regenerate or check documentation manually:

```sh
Rscript .githooks/roxygenise.R
Rscript .githooks/roxygenise.R --check
```

The check compares generated files with the staging area and also detects new,
untracked documentation. CI runs the same check against the checkout and fails
if generated changes were left out of the commit. When upgrading roxygen2,
update `Config/roxygen2/version`, install that version, and regenerate the
files; CI reads the required version from `DESCRIPTION` automatically.

## Continuous integration and releases

The [R package workflow](.github/workflows/ci.yml) runs on every pull request,
pushes to `master`, and version tags such as `1.2.3`. It installs the package
and CI dependencies, verifies the committed documentation with `roxygen2`,
builds the source package including its HTML vignette, and runs `R CMD check`
with the `testthat` suite.

To release a version, update `Version` in `DESCRIPTION` and add the
corresponding release section to `NEWS.md`, headed
`# lpjguessOutput 1.2.3 (YYYY-MM-DD)`. Commit the changes, then push a matching
tag:

```sh
git tag 1.2.3
git push origin 1.2.3
```

Tags must use the `X.Y.Z` format without a `v` prefix, and must exactly match
the package version. After the checks pass, the release job creates a GitHub
release with the matching section from the tagged `NEWS.md` and attaches the
validated `lpjguessOutput_X.Y.Z.tar.gz` source package. Rerunning the workflow
updates the asset and release notes if the release already exists. A missing or
empty release section fails the release job before publishing. Publishing uses
the workflow's `GITHUB_TOKEN`; no additional secret is required.
