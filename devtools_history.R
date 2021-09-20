# Ignore file "devtools_history.R"
usethis::use_build_ignore("devtools_history.R")

# Add the utils package in the DESCRIPTION file: Imports
usethis::use_package("utils")

# Ignore file "run_analysis.R"
usethis::use_build_ignore("run_analysis.R")

# Add the popbio package in the DESCRIPTION file: Imports
usethis::use_package("popbio")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("magrittr")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("stats")

# Ignore file "junk.R"
usethis::use_build_ignore("junk.R")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("SHELF")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("graphics")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("RColorBrewer")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("grDevices")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("dichromat")

# Ignore file "run_shiny.R"
usethis::use_build_ignore("run_shiny.R")


## Put it on Git
library(usethis)
usethis::use_git()


# Ignore file "run_shiny.R"
usethis::use_build_ignore("draws_histog.R")


# Tell Git to ignore the file "junk.R"
library(gitignore)
library(usethis)
usethis::use_git_ignore(ignores = "junk.R", directory = ".")


# Add package in the DESCRIPTION file: Imports
usethis::use_package("dplyr")
usethis::use_package("ggplot2")


# Add package in the DESCRIPTION file: Imports
usethis::use_package("shiny")

# Add package in the DESCRIPTION file: Imports
usethis::use_package("scales")
