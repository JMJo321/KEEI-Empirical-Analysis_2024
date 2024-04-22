# < Description > *
# > Script Group Indicator Number and Name
# : A-01, Data Building
#
# > Script Number(s)
# : A-01B
#
# > Purpose of the script(s)
# : Ingest the by-KSIC-code 5-Year history data, and then save it as .dta
#   format.
#
# > Source
#   1) 통계표 ID:
#      DT_2KI2022_S
#   2) 통계표명:
#      [광업,제조업] 산업세세분류별 출하액, 생산액, 부가가치 및 주요생산비
#      (10인 이상)
#   3) 통계표URL:
#      https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_2KI2022_S&conn_path=I3


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
PROJ.NAME <- "KEEI_Empirical-Analysis"


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
# 1. Path(s) for loading file(s) and/or script(s)
# 1.1. 5-Year History Data by KSIC-10
# 1.1.1. Fro 2015
FILE_TO.LOAD_5Y.HISTORY_2015 <- "광업-제조업-산업세세분류별-실적_2015.xlsx"
PATH_TO.LOAD_5Y.HISTORY_2015 <-
  paste(PATH_DATA_RAW_USE_5Y.HISTORY, FILE_TO.LOAD_5Y.HISTORY_2015, sep = "/")
# 1.1.1. Fro 2020
FILE_TO.LOAD_5Y.HISTORY_2020 <- "광업-제조업-산업세세분류별-실적_2020.xlsx"
PATH_TO.LOAD_5Y.HISTORY_2020 <-
  paste(PATH_DATA_RAW_USE_5Y.HISTORY, FILE_TO.LOAD_5Y.HISTORY_2020, sep = "/")

# 1.2. KSIC-10
FILE_TO.LOAD_KSIC <- "Korean-Standard-Industrial-Classification_10th.RData"
PATH_TO.LOAD_KSIC <-
  paste(PATH_DATA_INTERMEDIATE_KSIC, FILE_TO.LOAD_KSIC, sep = "/")


# 2. Path(s) for saving file(s) and/or script(s)
# 2.1. For .dta format file
FILE_TO.SAVE_5Y.HISTORY_DTA <- "5-Year-History-Data_By-KSIC-10.dta"
PATH_TO.SAVE_5Y.HISTORY_DTA <- paste(
  PATH_DATA_INTERMEDIATE_5Y.HISTORY, FILE_TO.SAVE_5Y.HISTORY_DTA,
  sep = "/"
)

# 2.2. For .RData format file
FILE_TO.SAVE_5Y.HISTORY_RDATA <- "5-Year-History-Data_By-KSIC-10.RData"
PATH_TO.SAVE_5Y.HISTORY_RDATA <- paste(
  PATH_DATA_INTERMEDIATE_5Y.HISTORY, FILE_TO.SAVE_5Y.HISTORY_RDATA,
  sep = "/"
)


# ------- Define parameter(s) -------
# 1. List incl. data fields' labels
LIST_LABELS <- list(
  "code_ksic" = "KSIC-10 코드",
  "year" = "연도",
  "t10" = "출하액 합계 (백만원)",
  "t101" = "제품출하액 (백만원)",
  "t102" = "부산물-폐품판매액 (백만원)",
  "t103" = "임가공수입액 (백만원)",
  "t104" = "수리수입액 (백만원)",
  "t20" = "생산액 (백만원)",
  "t30" = "부가가치 (백만원)",
  "t40" = "주요생산비 합계 (백만원)",
  "t401" = "원재료비 (백만원)",
  "t402" = "연료비 (백만원)",
  "t403" = "전력비 (백만원)",
  "t404" = "용수비 (백만원)",
  "t405" = "외주가공비 (백만원)",
  "t406" = "수선비 (백만원)",
  "t50" = "영업비용 (백만원)"
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Ingest .xlsx files, and then save it as .dta and .RData files
# ------------------------------------------------------------------------------
# ------- Ingest Excel files -------
# 1. Ingest .xlsx files
# 1.1. For 2015
tmp_dt_2015 <-
  wb_read(
    file = PATH_TO.LOAD_5Y.HISTORY_2015, sheet = "데이터",
    start_row = 1, col_names = TRUE
  ) %>%
    setDT(.)

# 1.2. For 2020
tmp_dt_2020 <-
  wb_read(
    file = PATH_TO.LOAD_5Y.HISTORY_2020, sheet = "데이터",
    start_row = 1, col_names = TRUE
  ) %>%
    setDT(.)


# 2. Modify the DTs ingested
# 2.1. Rename data fields
cols_name <- names(tmp_dt_2020)
cols_name <-
  paste0("tmp_", str_extract(cols_name, "^T[0-9]+")) %>%
    str_to_lower(.)
cols_name[1:2] <- c("code_ksic", "year")
names(tmp_dt_2015) <- cols_name
names(tmp_dt_2020) <- cols_name


# ------- Create a DT by binding the DTs ingested -------
# 1. Create a DT by binding the DTs ingested
# dt_5y.history <- rbind(tmp_dt_2015[!(1:2)], tmp_dt_2020[!(1:2)])
dt_5y.history <- rbind(tmp_dt_2015, tmp_dt_2020)


# 2. Modify the DT created
# 2.1. Add data field(s)
# 2.1.1. Add a data field showing KSIC-10 codes
dt_5y.history[
  ,
  code_ksic := (
    str_replace_all(code_ksic, "\u3000", "") %>%  # Replace "　" with "".
      str_extract(., "^[0-9]+")
  )
]

# 2.2. Change data fields' data types
# 2.2.1. For `year`, from character to integer
dt_5y.history[
  ,
  year := (
    str_extract(year, " [0-9]+$") %>%
      str_replace(., " ", "") %>%
      as.integer(.)
  )
]
# 2.2.2. For `year`, from character to integer
dt_5y.history[str_detect(tmp_t10, "[0-9]+", negate = TRUE)]
# Note:
# The row with `code_ksic == 33201` has "-". 
cols_for.loop <- cols_name[str_detect(cols_name, "t[0-9]+")]
for (col in cols_for.loop) {
  dt_5y.history[get(col) == "-", (col) := NA]
}
for (col in cols_for.loop) {
  dt_5y.history[
    !is.na(get(col)),
    (str_replace(col, "tmp_", "")) := as.integer(get(col))
  ]
}

# 2.3. Drop unnecessary data field(s)
cols_select <-
  names(dt_5y.history)[str_detect(names(dt_5y.history), "^tmp_", negate = TRUE)]
dt_5y.history <- dt_5y.history[
  code_ksic != "0" & !is.na(code_ksic),
  .SD, .SDcols = cols_select
]


# ------- Add data field(s) that includes KSIC code-related information -------
# 1. Add data field(s)
# 1.1. Import a .RData
load(file = PATH_TO.LOAD_KSIC)

# 1.2. Add data field(s)
dt_5y.history_w.ksic <- merge(
  x = dt_5y.history,
  y = dt_ksic,
  by.x = "code_ksic",
  by.y = "code",
  all.x = TRUE
)

# 1.3. Set key(s)
keys <- c("code_ksic", "year")
setkeyv(dt_5y.history_w.ksic, cols = keys)
# 1.3.1. Conduct a simple test
stopifnot(dt_5y.history_w.ksic[, .N, by = keys][N > 1] == 0)


# ------- Export the DT in .dta and .RData format -------
# 1. Do additional work before exporting the DT
# 1.1. Label variables
lapply(
  names(LIST_LABELS),
  label_data.fields,
  dt_in.str = "dt_5y.history_w.ksic", list_labels = LIST_LABELS
)

# 2. Export the DT
# 2.1. Export in .RData format
save(dt_5y.history_w.ksic, file = PATH_TO.SAVE_5Y.HISTORY_RDATA)

# 2.2. Export in .dta format
haven::write_dta(
  data = dt_5y.history_w.ksic, path = PATH_TO.SAVE_5Y.HISTORY_DTA,
  version = 15
)
