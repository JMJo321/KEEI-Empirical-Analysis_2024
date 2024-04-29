# < Description > *
# > Script Group Indicator Number and Name
# : A, Data Building
#
# > Script Number(s)
# : A-02A
#
# > Purpose of the script(s)
# : Ingest an Excel file, which contains
#   KCAD (Korean Classification of Administrative Districts, 한국행정구역분류)
#   codes.
#
# > Source
# : https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#
#   (2024.04.01 기준)


# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(openxlsx2)
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
FILE_TO.LOAD_KCAD <- "한국행정구역분류_2024.4.1.기준_20240329034019.xlsx"
PATH_TO.LOAD_KCAD <-
  paste(PATH_DATA_RAW_USE_KCAD, FILE_TO.LOAD_KCAD, sep = "/")


# 2. Path(s) for saving file(s) and/or script(s)
# 2.1. In .RData format
FILE_TO.SAVE_KCAD_RDATA <-
  "Korean-Classification-of-Administrative-Districts_2024-04-01.RData"
PATH_TO.SAVE_KCAD_RDATA <-
  paste(PATH_DATA_INTERMEDIATE_KCAD, FILE_TO.SAVE_KCAD_RDATA, sep = "/")
# 2.2. In .dta format
FILE_TO.SAVE_KCAD_DTA <-
  "Korean-Classification-of-Administrative-Districts_2024-04-01.dta"
PATH_TO.SAVE_KCAD_DTA <-
  paste(PATH_DATA_INTERMEDIATE_KCAD, FILE_TO.SAVE_KCAD_DTA, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)
LIST_KCAD_LABEL <- list(
  code = "분류 코드",
  code_level1 = "대분류 코드",
  desc_level1 = "시/도",
  code_level2 = "중분류 코드",
  desc_level2 = "시/군/구",
  code_level3 = "소분류 코드",
  desc_level3 = "읍/면/동",
  desc_level3_en = "읍/면/동 영문",
  desc_level3_ch = "읍/면/동 한자",
  note = "비고"
)

# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Ingest a .xlsx file, and then save it as .dta file
# ------------------------------------------------------------------------------
# ------- Ingest an Excel file -------
dt_kcad <-
  wb_read(
    file = PATH_TO.LOAD_KCAD, sheet = "2. 항목표(기준시점)",
    skip_empty_rows = TRUE, skip_empty_cols = TRUE,
    start_row = 3, col_names = TRUE
  ) %>%
    setDT(.)


# ------- Modify the DT created -------
# 1. Rename data fields
cols_name <- c(
  "code_level1", "desc_level1",
  "code_level2", "desc_level2",
  "code_level3", "desc_level3", "desc_level3_en", "desc_level3_ch",
  "note"
)
names(dt_kcad) <- cols_name


# 2. Change column's data type
dt_kcad[, code_level1 := as.character(code_level1)]


# 3. Adjust value(s)
dt_kcad[, code_level2 := str_replace(code_level2, "^[0-9]{2}", "")]
dt_kcad[, code_level3 := str_replace(code_level3, "^[0-9]{5}", "")]


# 4. Add data field(s)
dt_kcad[
  ,
  code := (
    paste0(code_level1, code_level2, code_level3) %>%
      str_replace_all(., "NA", "")
  )
]


# 5. Reorder column(s)
cols_reorder <- "code"
setcolorder(dt_kcad, cols_reorder)


# 6. Label data field(s)
lapply(
  names(dt_kcad),
  label_data.fields,
  dt_in.str = "dt_kcad", list_labels = LIST_KCAD_LABEL
)
str(dt_kcad)


# 7. Set key(s)
stopifnot(dt_kcad[, .N, by = .(code)][N > 1] == 0)
setkey(dt_kcad, "code")


# ------- Export the DT -------
# 1. Export in .Rdata format
save("dt_kcad", file = PATH_TO.SAVE_KCAD_RDATA)

# 2. Export in .dta format
haven::write_dta(dt_kcad, PATH_TO.SAVE_KCAD_DTA, version = 15)
