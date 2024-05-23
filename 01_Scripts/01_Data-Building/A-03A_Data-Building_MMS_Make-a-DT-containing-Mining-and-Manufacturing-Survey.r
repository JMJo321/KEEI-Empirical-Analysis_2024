# < Description > *
# > Script Group Indicator Number and Name
# : A, Data Building
#
# > Script Number(s)
# : A-03A
#
# > Purpose of the script(s)
# : Ingest "Mining and Manufacturing Survey(광업-제조업조사)" dataset for
#   2016-19 and 2021, which are downloaded from MDIS.
#
# > Note
# : Dataset for the public.


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
# (Not Applicable)


# 2. Path(s) for saving file(s) and/or script(s)
# 2.1. In .RData format
FILE_TO.SAVE_MMS_RDATA <- "Mining-and-Manufacturing-Survey.RData"
PATH_TO.SAVE_MMS_RDATA <-
  paste(PATH_DATA_INTERMEDIATE_MMS, FILE_TO.SAVE_MMS_RDATA, sep = "/")
# 2.2. In .dta format
FILE_TO.SAVE_MMS_DTA <- "Mining-and-Manufacturing-Survey.dta"
PATH_TO.SAVE_MMS_DTA <-
  paste(PATH_DATA_INTERMEDIATE_MMS, FILE_TO.SAVE_MMS_DTA, sep = "/")


# ------- Define parameter(s) -------
# 1. Columns(s) that will extracted from the original dataset
COLS_EXTRACT <- c(
  "id",
  "조사기준연도", "행정구역시도코드",
  "산업대분류코드", "주사업_산업중분류코드", "주사업_산업소분류코드",
    "주사업_산업세분류코드", "주사업_산업세세분류코드",
  "생산금액", "부가가치금액", "사업영업비_총금액",
    "원재료비", "연료비", "전력비",
    "용수비", "조사_외주가공비", "수선비"
)
# Note:
# "경제총조사" 데이터에 "주요생산비" 변수가 존재하기 않기 때문에, 해당 변수를
# 제외.


# 2. New names for the columns extracted from the original dataset
COLS_NEW.NAMES <- c(
  "id",
  "year", "kcad_level1",
  "ksic_level1", "ksic_level2", "ksic_level3",
    "ksic_level4", "ksic_level5",
  "output_gross", "value_added", "expense_business",
    "expense_rawmaterial", "expense_fuel", "expense_electricity",
    "expense_water", "fee_outsourcing", "fee_mending"
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT by ingesting .csv files
# ------------------------------------------------------------------------------
# ------- Creating a DT by ingesting .csv files  -------
# 1. Creating a DT by ingesting .csv files
# 1.1. Make a list that contains the paths of .csv files that will be ingested
array_files <- list.files(PATH_DATA_RAW_USE_MMS)
array_paths <- paste(
  PATH_DATA_RAW_USE_MMS,
  array_files[str_detect(array_files, "csv$")],
  sep = "/"
)

# 1.2. Create a DT by ingesting .csv files
dt_mms <-
  lapply(array_paths, helper_ingest) %>%
  rbindlist(., fill = TRUE)


# ------------------------------------------------------------------------------
# Make a DT by subsetting the DT created above
# ------------------------------------------------------------------------------
# ------- Make a DT by subsetting the DT created above  -------
# 1. Make a DT by subetting the DT created above
# 1.1. Creat a DT by dropping observations covered with "*"
subdt_mms <- dt_mms[str_detect(창설연도, "\\*", negate = TRUE), ..COLS_EXTRACT]

# 1.2. Rename data fields
names(subdt_mms) <- COLS_NEW.NAMES


# ------- Modify the DT created -------
# 1. Add data field(s)
# 1.1. Add a data field showing KSIC(10th) codes
subdt_mms[, ksic := paste0(ksic_level2, ksic_level3, ksic_level4)]
# Note:
# "경제총조사"의 경우, 표준산업분류가 Level-4 (세분류)까지만 제공됨.


# 2. Conver data field's types
# 2.1. Character to numeric
cols_to.numeric <- COLS_NEW.NAMES[
  str_detect(COLS_NEW.NAMES, "year|output|value|cost|expense|fee")
]
for(col in cols_to.numeric) {
  subdt_mms[, (col) := (get(col) %>% as.numeric)]
}


# 3. Drop unnecessary data field(s)
# (Not Applicable)


# 4. Others
cols_reorder <- c("id", "year", "ksic")
setcolorder(subdt_mms, cols_reorder)


# ------------------------------------------------------------------------------
# Export the DT created
# ------------------------------------------------------------------------------
# ------- Prepare for exporting the DT created above  -------
# 1. Set key(s)
stopifnot(subdt_mms[, .N, by = . (id)][N > 1] == 0)
setkey(subdt_mms, "id")


# 2. Label data fields
# 2.1. Create an array containing labels
cols_labels <- COLS_EXTRACT
cols_labels[1] <- "ID Number"
cols_labels <- c(cols_labels, "산업분류코드 (10차 기준)")
names(cols_labels) <- c(COLS_NEW.NAMES, "ksic")

# 2.2. Label data fields
lapply(
  names(cols_labels),
  label_data.fields,
  dt_in.str = "subdt_mms", list_labels = cols_labels
)


# ------- Export the DT created -------
# 1. Export in .Rdata format
save("subdt_mms", file = PATH_TO.SAVE_MMS_RDATA)


# 2. Export in .dta format
haven::write_dta(subdt_mms, PATH_TO.SAVE_MMS_DTA, version = 15)
