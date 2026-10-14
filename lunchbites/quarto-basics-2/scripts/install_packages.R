# Explicit setup only; never called by a render hook.
packages <- c("knitr", "rmarkdown", "dplyr", "ggplot2", "plotly", "DT")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Package unavailable: ", package)
  cat(package, as.character(packageVersion(package)), "\n")
}
