# < Description > *
# > Script Group Indicator Number and Name
# : A, Data Building
#
# > Script Number(s)
# : A-05A
#
# > Purpose of the script(s)
# : Merge dataset to create a DT for regression analysis.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(readr)
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "IGHGE"


# ------- Set working directory -------
PATH_PROJ <- paste(
  "/Users/jmjo/Library/CloudStorage/Dropbox/00_JMJo/KEEI/2024/기본과제",
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
# 1. Path(s) for loading file(s) and/or script(s)
# 2.1. For "Mining and Manufacturing Survey (MMS)" dataset
FILE_TO.LOAD_MMS <- "Mining-and-Manufacturing-Survey.RData"
PATH_TO.LOAD_MMS <-
  paste(PATH_DATA_INTERMEDIATE_MMS, FILE_TO.LOAD_MMS, sep = "/")

# 2.2. For "Economic Census (EC)" dataset
FILE_TO.LOAD_EC <- "Economic-Census.RData"
PATH_TO.LOAD_EC <-
  paste(PATH_DATA_INTERMEDIATE_EC, FILE_TO.LOAD_EC, sep = "/")

# 2.3. For "Korean Standard Industrial Classification (KSIC)" dataset
FILE_TO.LOAD_KSIC <- "Korean-Standard-Industrial-Classification_10th.RData"
PATH_TO.LOAD_KSIC <-
  paste(PATH_DATA_INTERMEDIATE_KSIC, FILE_TO.LOAD_KSIC, sep = "/")

# 2.4. For "Korean Classification of Administrative Districts (KCAD)" dataset
FILE_TO.LOAD_KCAD <-
  "Korean-Classification-of-Administrative-Districts_2024-04-01.RData"
PATH_TO.LOAD_KCAD <-
  paste(PATH_DATA_INTERMEDIATE_KCAD, FILE_TO.LOAD_KCAD, sep = "/")


# 2. Path(s) for saving file(s) and/or script(s)
# 2.1. In .RData format
FILE_TO.SAVE_REG_RDATA <- "DT-for-Regression-Analysis.RData"
PATH_TO.SAVE_REG_RDATA <-
  paste(PATH_DATA_ANALYSIS, FILE_TO.SAVE_REG_RDATA, sep = "/")

# 2.2. In .dta format
FILE_TO.SAVE_REG_DTA <- "DT-for-Regression-Analysis.dta"
PATH_TO.SAVE_REG_DTA <-
  paste(PATH_DATA_ANALYSIS, FILE_TO.SAVE_REG_DTA, sep = "/")


# ------- Define parameter(s) -------
# 1. A list that includes lables
LIST_LABELS <- list(
  ksic = "산업분류코드 (10차 기준), 세분류",
  kcad = "한국행정구역분류코드, 시/도 (대분류)"
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT, by merging dataset, for regression analysis
# ------------------------------------------------------------------------------
# ------- Merge DTs: MMS and EC datasets -------
# 1. Load DT(s) that will be merged
# 1.1. Load DT(s)
load(PATH_TO.LOAD_MMS)
load(PATH_TO.LOAD_EC)

# 1.2. Check the key(s) of the DT(s)
key(subdt_mms)
key(subdt_ec)


# 2. Create a temporary DT by merging the two DTs
tmp_dt <- rbind(subdt_mms, subdt_ec, fill = TRUE)


# ------- Incorporate KSIC and KCAD info. into the DT created -------
# 1. Regarding KSIC
# 1.1. Load DT(s)
load(PATH_TO.LOAD_KSIC)

# 1.2. Check the key(s) of the DT(s)
stopifnot(dt_ksic[, .N, by = .(code)][N > 1] == 0)


# 2. Incorporate KSIC info. into the DT created
ksic_levels <- dt_ksic[level_en == "Class"]$code
ksic_labels <- dt_ksic[level_en == "Class"]$desc_ko
tmp_dt[, ksic := factor(ksic, levels = ksic_levels, labels = ksic_labels)]


# 2. Regarding KCAD
# 2.1. Load DT(s)
load(PATH_TO.LOAD_KCAD)

# 2.2. Check the key(s) of the DT(s)
key(dt_kcad)


# 2. Incorporate KCAD info. into the DT created
kcad_levels <- dt_kcad[, .N, by = .(code_level1, desc_level1)]$code_level1
kcad_labels <- dt_kcad[, .N, by = .(code_level1, desc_level1)]$desc_level1
tmp_dt[
  ,
  kcad := factor(kcad_level1, levels = kcad_levels, labels = kcad_labels)
]


# ------- Create a DT for regression analysis -------
# 1. Create a DT for regression anaysis by extracting data fields required
cols_extract <-
  names(tmp_dt)[str_detect(names(tmp_dt), "level[0-9]$", negate = TRUE)]
dt_reg <- tmp_dt[, ..cols_extract]


# 2. Modify the DT created
# 2.1. Reorder columns
cols_reorder <- c("id", "year", "ksic", "kcad")
setcolorder(dt_reg, cols_reorder)


# ------------------------------------------------------------------------------
# Export the DT created
# ------------------------------------------------------------------------------
# ------- Prepare for exporting the DT created above  -------
# 1. Set key(s)
stopifnot(dt_reg[, .N, by = . (id)][N > 1] == 0)
setkey(dt_reg, "id")


# 2. Label data fields
lapply(
  names(LIST_LABELS),
  label_data.fields,
  dt_in.str = "dt_reg", list_labels = LIST_LABELS
)


# ------- Export the DT created -------
# 1. Export in .Rdata format
save("dt_reg", file = PATH_TO.SAVE_REG_RDATA)


# 2. Export in .dta format
haven::write_dta(dt_reg, PATH_TO.SAVE_REG_DTA, version = 15)
