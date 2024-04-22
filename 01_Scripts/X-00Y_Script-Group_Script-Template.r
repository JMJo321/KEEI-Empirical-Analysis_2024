# < Description > *
# > Script Group Indicator Number and Name
# : X-00, XYZ
#
# > Script Number(s)
# : X-00Y
#
# > Purpose of the script(s)
# : (...)

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "Empirical-Analysis"


# ------- Set working directory -------
PATH_PROJ <- paste(
  paste(
    "/Users/jmjo/Library/CloudStorage/Dropbox/00_JMJo/KEEI",
    "KEEI-Empirical-Analysis_2024",
    sep = "/"
  ),
  PROJ.NAME,
  sep = "/"
)
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# (...)
# ------------------------------------------------------------------------------
# ------- (...) -------
# (...)