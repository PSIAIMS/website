# Show the current library paths where R looks for packages
.libPaths()

# List all installed packages (data frame of package metadata)
installed.packages()


###############################################
# Changing and resetting library paths
###############################################

# Save the current library search paths so we can restore them later
old_libpaths <- .libPaths()

# Temporarily set the library path to a temp folder (not recommended long-term)
.libPaths(tempdir())

# Change library path to the current working directory
.libPaths(getwd())

# Restore the original library paths
.libPaths(old_libpaths)

# Prepend a custom folder to the library paths
.libPaths(c("C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/Test Folder",
            old_libpaths))


###############################################
# Installing packages into different locations
###############################################

# Install DoseFinding into the *first* library in .libPaths()
install.packages("DoseFinding")

# Install 'stringr' into a specific library folder, using a custom CRAN mirror,
# and without installing dependencies
install.packages(
  "stringr",
  lib = "C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/Test Folder2",
  repos = "https://cran.ma.imperial.ac.uk/",
  dependencies = FALSE
)

# Overwrite library paths to look only in Program Files
# (R likely has no write access here)
.libPaths("C:/Program Files")

# Install a package from a source tarball (older version from CRAN Archive)
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/DoseFinding/DoseFinding_1.3-1.tar.gz",
  repos = NULL, type = "source"
)

# Remove a previously installed package
remove.packages("DoseFinding")


##################################################
# Demonstrating function masking and namespaces
##################################################

words <- c("First","Second","Third")

# Calling str_length() *before* loading stringr — will fail unless stringr loaded
str_length(words)

library(stringr)
# Now str_length() works because stringr is attached
str_length(words)

# Detach the package completely
detach("package:stringr", unload = TRUE)

# Using the function explicitly via namespace, without attaching the package
stringr::str_length(words)


########################################################
# Demonstrating masking between dplyr and stats
########################################################

install.packages("dplyr")

library(dplyr)

View(iris)

# dplyr::filter() filters rows
filter(iris, Sepal.Length > 7)

# Detach stats (usually not needed – just for demonstration)
detach("package:stats", unload = TRUE)

# Re-load stats
library(stats)

# Now filter() may refer to stats::filter (time-series filter!), not dplyr
filter(iris, Sepal.Length > 7)   # likely error or unexpected result

# Help for filter will show the stats version
?filter

# Use the fully qualified dplyr function
dplyr::filter(iris, Sepal.Length > 7)


##################################################################
# Checking versions and session info
##################################################################

# Get the installed version of ggplot2
packageVersion("ggplot2")

# Display detailed information about R, OS, loaded packages
sessionInfo()


################################################################
# Mini-exercise: read a CSV file with readr
################################################################

# Point the library path to a custom folder
.libPaths("C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/Test Folder3")

# Install readr into that folder
install.packages("readr")

library(readr)

?read_csv

# Read a CSV file using readr
MAI <- read_csv(
  file = "C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/MAI.csv"
)


#################################################################
# Installing from GitHub with 'remotes'
#################################################################

install.packages("remotes")  # install once

# Install a specific tagged GitHub release
remotes::install_github(
  "insightsengineering/teal.reporter",
  ref = "v0.5.0"
)


#################################################################
# Installing from GitHub with 'pak'
#################################################################

install.packages("pak")  # install once

# Install the same tagged release, using pak's syntax
pak::pkg_install("github::insightsengineering/teal.reporter@v0.5.0")


##################################################################
# Minimal renv demo — no scanning of thousands of files
##################################################################

# Set the library path to a clean folder for the renv demo
.libPaths("C:/Users/Martin.Brown1/Desktop/OldCBTI/WorkBTI/RDemo/Test Folder3")

install.packages("renv")

# Initialize renv WITHOUT scanning the project for dependencies
renv::init(bare = TRUE)

# Install a package inside the renv environment
install.packages("stringr")   # or any package you like

# Snapshot the exact state of the renv library to renv.lock
renv::snapshot()

# Show dependencies that renv thinks are in the project
renv::dependencies()
