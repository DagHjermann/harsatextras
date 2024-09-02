#
# Notes and code for creating/maintaining this package
# - note that when the package has been set up, this file (_create_package.r) was added to .Rbuildignore
# The basis for functions and example files is the 'harsat_targets' project
# Guidance for making package: "R Packages" e-book, 2nd edition, by Hadley (https://r-pkgs.org/)

#
# Initializing package ----
#

# How I started this package:
# - "R Packages" e-book chapter 1.4 to 1.5, https://r-pkgs.org/whole-game.html
#     - in RStudio (on jupyterhub), create_package and use_git()
#     - initiates the local git setup (on your hardisk, or rather, jupyterhub's hard disk)

# from the level above_
library(devtools)
create_package()
# new project opens

library(devtools) # because we are in a new project
use_git()

#
# Make github repo, connect to it ----
#

# - Happy Git e-book
#     - chapter 17.3.2 and 17.3.2.1 (https://happygitwithr.com/existing-github-last)
#     - this step is also in R Packages 1.17; as mentioned there you may also use use_github() but that requires
#     that your github personal access token (PAT) can be discovered


# R packages chapter 7.1.1:
  # Preserve the origin story of package data
  # We highly recommend taking the time to include the code used to do this in the source version of your package.
  # We suggest that you keep this code in one or more .R files below data-raw/. You don’t want it in the bundled version of
  # your package, so this folder should be listed in .Rbuildignore.

  #  R book: 'I recommend you save scripts that generate package data in data-raw: use use_data_raw() to set it up.'

usethis::use_data_raw()
# creates folder and adds it to .Rbuildignore


#
# Add example data ----
# From the folder 'example_data' in 'harsat_targets' project
#

# 1. Csv files - will be put in package folder 'inst/extdata/', see "R Packages" chapter 7.3)
# Just made folder 'inst/extdata/' and uploaded csv files to that folder
#   from my harddisk (drag-and-drop from harsat_targets/example_data)

# 2. Rdata (rds/rda) files - made it a bit closer to the correct way
# - uploaded RDS files to the 'data-raw' folder from my harddisk
# - made new file 'rds_files.R'
# - wrote two lines of code:

norway_timeseries_all <- readRDS("data-raw/norway_timeseries_all.rds")
use_data(norway_timeseries_all)

# R output:
# ✔ Adding 'R' to Depends field in DESCRIPTION
# ✔ Creating 'data/'
# ✔ Setting LazyData to 'true' in 'DESCRIPTION'
# ✔ Saving 'norway_timeseries_all' to 'data/norway_timeseries_all.rda'
# • Document your data (see 'https://r-pkgs.org/data.html')

#
# Help file for example data ----
#

# Made 'data.R' file and wrote description for 'norway_timeseries_all'
# (using 'https://github.com/hadley/babynames/blob/master/R/data.R'
# as a template).

#
# build the actual help file
#
document()

#
# First build ----
#

# check help file ("R Packages" chapter 1.8)
load_all()
?norway_timeseries_all

#
# First check ----
#
#  ("R Packages" chapter 1.9)

check()
# one warning for lacking licence
# one warning for non-ASCII characters in file
use_mit_license()

#
# Add first function ----
#

# "R Packages" chapter 1.7
use_r("split_timeseries_object")

# Document it
# - open file, put cursor in function
# - Code > Insert roxygen skeleton

#
# Finalize 'split_timeseries_object'
# - add the two RData (rda) files needed to test run 'split_timeseries_object'
# - finalize description for 'split_timeseries_object' , including example

# read_data_tar, preparations ----
# add
# - info file (as exported data object)
# - station dictionary (as csv file)

# add path helper function 'harsatextras_example' (to use for e.g. the station dictionary)
use_r("harsatextras_example")
# copied function (and changed [package name]) from R Packages chapter 7.3.2.
# added help text in Roygen format
document()
load_all()
?harsatextras_example
# testing examples in help file

# add read_data_tar ----
use_r("read_data_tar")
# Code > Insert ROxygen skeleton
# ... fill out text for help file
document()
load_all()
?read_data_tar

#
# add more functions ----
#
# For eqch time series, use the work flow
# use_r -> Code > Insert ROxygen skeleton -> document() -> load_all() -> check()

use_r("tidy_data2")
use_r("run_assessment_tar")
use_r("get_assessment_data")
use_r("combine_assessment_data")
# for the latter, remmeber to wrap examples in \dontrun{}
use_r("ggplot_assessment")
# for the latter, remember
#   requireNamespace("ggplot2") (and note: name in quotes)
use_r("open_assessment_app")
use_r("get_trend_text")

#
# Better Readme
#
usethis::use_readme_rmd()
devtools::build_readme()


