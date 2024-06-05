# < Description > *
# > Script Group Indicator Number and Name
# : A, Data Building
#
# > Script Number(s)
# : A-04A
#
# > Purpose of the script(s)
# : Ingest "Economic Census(경제총조사)" dataset for 2015 and 2020, which
#   are downloaded from MDIS.
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
FILE_TO.SAVE_EC_RDATA <- "Economic-Census.RData"
PATH_TO.SAVE_EC_RDATA <-
  paste(PATH_DATA_INTERMEDIATE_EC, FILE_TO.SAVE_EC_RDATA, sep = "/")
# 2.2. In .dta format
FILE_TO.SAVE_EC_DTA <- "Economic-Census.dta"
PATH_TO.SAVE_EC_DTA <-
  paste(PATH_DATA_INTERMEDIATE_EC, FILE_TO.SAVE_EC_DTA, sep = "/")


# ------- Define parameter(s) -------
# 1. Columns that will extracted from the original dataset
COLS_EXTRACT <- c(
  "id",
  "조사기준연도", "행정구역시도코드",
  "주사업_산업대분류코드", "주사업_산업중분류코드", "주사업_산업소분류코드",
    "주사업_산업세분류코드",
  "생산금액", "부가가치금액", "영업비_총합",
    "원재료비_총합", "연료비_총합", "전력비_총합",
    "용수비_총합", "외주가공비_총합", "수선비_총합"
)


# 2. New names for the columns extracted from the original dataset
COLS_NEW.NAMES <- c(
  "id",
  "year", "kcad_level1",
  "ksic_level1", "ksic_level2", "ksic_level3",
    "ksic_level4",
  "output_gross", "value_added", "expense_business",
    "expense_rawmaterial", "expense_fuel", "expense_electricity",
    "expense_water", "fee_outsourcing", "fee_mending"
)


# ------- Define function(s) -------
# (Not Applicable)



# ------------------------------------------------------------------------------
# Ingest 
# ------------------------------------------------------------------------------
# ------- Ingest  -------
# 1. 
array_files <- list.files(PATH_DATA_RAW_USE_EC)
path_files <- paste(
  PATH_DATA_RAW_USE_EC,
  array_files[str_detect(array_files, "csv$")],
  sep = "/"
)

# 2. 
dt_ec <-
  lapply(path_files, helper_ingest) %>%
  rbindlist(., fill = TRUE)
# Note:
# 1) 동일한 항목에 대해서, 2015년 자료에서는 1개의 변수로 정리가 된 것이 2020년
#    자료에서는 2개의 변수로 정리가 된 것이 존재
#    (예> 연료비 in 2015 vs. 조사_연료비 & 조사외_연료비 in 2020)
# 2) 2020년 자료에서, 광업-제조업 이외 타 산업을 겸업하는 사업체의 경우에는
#    "광업 및 제조업 부문"에 대한 값을 "조사_XYZ", 그리고
#    "광업 및 제조업 부문 이외"에 대한 값을 "조사외_XYZ"로 정리함.
# 3) 이후의 작업에서, 2020년 데이터에 대해서 "조사_XYZ"와 "조사외_XYZ"를 더한
#    값을 "XYZ"의 값으로 사용 (기업단위에서이 분석임을 고려).


# ------- Modify the DT created above -------
# 1. Add data field(s)
# 1.1. Add data field(s) showing the sum of "조사_XYZ" and "조사외_XYZ"
# 1.1.1. For "생산금액"
names(dt_ec)[str_detect(names(dt_ec), "생산금액")]
dt_ec[!is.na(생산금액), .N, by = .(조사기준연도)]  # 2015 and 2020
# Note:
# No additional calculation is required.
# 1.1.2. For "부가가치금액"
names(dt_ec)[str_detect(names(dt_ec), "부가가치금액")]
dt_ec[!is.na(부가가치금액), .N, by = .(조사기준연도)]  # 2015 and 2020
# Note:
# No additional calculation is required.
# 1.1.3. For "영업비"
names(dt_ec)[str_detect(names(dt_ec), "영업비")]
dt_ec[!is.na(사업영업비_총금액), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사_기타영업비용), .N, by = .(조사기준연도)]  # 2015 and 2020
dt_ec[!is.na(조사외_영업비용), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "사업영업비_총금액" + "조사외_영업비용" => "영업비용_총합"
dt_ec[
  ,
  `:=` (
    사업영업비_총금액 = as.numeric(사업영업비_총금액),
    조사외_영업비용 = as.numeric(조사외_영업비용)
  )
]
dt_ec[, tmp_is_na := is.na(사업영업비_총금액) & is.na(조사외_영업비용)]
dt_ec[
  tmp_is_na == FALSE,
  영업비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(사업영업비_총금액, 조사외_영업비용)],
    na.rm = TRUE
  )
]
# 1.1.4. For "원재료비"
names(dt_ec)[str_detect(names(dt_ec), "원재료비")]
dt_ec[!is.na(원재료비), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사외_원재료비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "원재료비" + "조사외_원재료비" => "원재료비_총합"
dt_ec[
  ,
  `:=` (
    원재료비 = as.numeric(원재료비),
    조사외_원재료비 = as.numeric(조사외_원재료비)
  )
]
dt_ec[, tmp_is_na := is.na(원재료비) & is.na(조사외_원재료비)]
dt_ec[
  tmp_is_na == FALSE,
  원재료비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(원재료비, 조사외_원재료비)],
    na.rm = TRUE
  )
]
# 1.1.5. For "연료비"
names(dt_ec)[str_detect(names(dt_ec), "연료비")]
dt_ec[!is.na(연료비), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사_연료비), .N, by = .(조사기준연도)]  # 2020 only
dt_ec[!is.na(조사외_연료비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "연료비" + "조사_연료비" + "조사외_연료비" => "연료비_총합"
dt_ec[
  ,
  `:=` (
    연료비 = as.numeric(연료비),
    조사_연료비 = as.numeric(조사_연료비),
    조사외_연료비 = as.numeric(조사외_연료비)
  )
]
dt_ec[, tmp_is_na := is.na(연료비) & is.na(조사_연료비) & is.na(조사외_연료비)]
dt_ec[
  tmp_is_na == FALSE,
  연료비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(연료비, 조사_연료비, 조사외_연료비)],
    na.rm = TRUE
  )
]
# 1.1.6. For "전력비"
names(dt_ec)[str_detect(names(dt_ec), "전력비")]
dt_ec[!is.na(전력비), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사_전력비), .N, by = .(조사기준연도)]  # 2020 only
dt_ec[!is.na(조사외_전력비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "전력비" + "조사_전력비" + "조사외_전력비" => "전력비_총합"
dt_ec[
  ,
  `:=` (
    전력비 = as.numeric(전력비),
    조사_전력비 = as.numeric(조사_전력비),
    조사외_전력비 = as.numeric(조사외_전력비)
  )
]
dt_ec[, tmp_is_na := is.na(전력비) & is.na(조사_전력비) & is.na(조사외_전력비)]
dt_ec[
  tmp_is_na == FALSE,
  전력비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(전력비, 조사_전력비, 조사외_전력비)],
    na.rm = TRUE
  )
]
# 1.1.7. For "용수비"
names(dt_ec)[str_detect(names(dt_ec), "용수비")]
dt_ec[!is.na(용수비), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사_용수비), .N, by = .(조사기준연도)]  # 2020 only
dt_ec[!is.na(조사외_용수비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "용수비" + "조사_용수비" + "조사외_용수비" => "용수비_총합"
dt_ec[
  ,
  `:=` (
    용수비 = as.numeric(용수비),
    조사_용수비 = as.numeric(조사_용수비),
    조사외_용수비 = as.numeric(조사외_용수비)
  )
]
dt_ec[, tmp_is_na := is.na(용수비) & is.na(조사_용수비) & is.na(조사외_용수비)]
dt_ec[
  tmp_is_na == FALSE,
  용수비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(용수비, 조사_용수비, 조사외_용수비)],
    na.rm = TRUE
  )
]
# 1.1.8. For "외주가공비"
names(dt_ec)[str_detect(names(dt_ec), "외주가공비")]
dt_ec[!is.na(조사_외주가공비), .N, by = .(조사기준연도)]  # 2015 & 2020
dt_ec[!is.na(조사외_외주가공비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "조사_외주가공비" + "조사외_외주가공비" => "외주가공비_총합"
dt_ec[
  ,
  `:=` (
    조사_외주가공비 = as.numeric(조사_외주가공비),
    조사외_외주가공비 = as.numeric(조사외_외주가공비)
  )
]
dt_ec[, tmp_is_na := is.na(조사_외주가공비) & is.na(조사외_외주가공비)]
dt_ec[
  tmp_is_na == FALSE,
  외주가공비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(조사_외주가공비, 조사외_외주가공비)],
    na.rm = TRUE
  )
]
# 1.1.9. For "수선비"
names(dt_ec)[str_detect(names(dt_ec), "수선비")]
dt_ec[!is.na(수선비), .N, by = .(조사기준연도)]  # 2015 only
dt_ec[!is.na(조사_수선비), .N, by = .(조사기준연도)]  # 2020 only
dt_ec[!is.na(조사외_수선비), .N, by = .(조사기준연도)]  # 2020 only
# Note:
# "수선비" + "조사_수선비" + "조사외_수선비" => "수선비_총합"
dt_ec[
  ,
  `:=` (
    수선비 = as.numeric(수선비),
    조사_수선비 = as.numeric(조사_수선비),
    조사외_수선비 = as.numeric(조사외_수선비)
  )
]
dt_ec[, tmp_is_na := is.na(수선비) & is.na(조사_수선비) & is.na(조사외_수선비)]
dt_ec[
  tmp_is_na == FALSE,
  수선비_총합 := rowSums(
    dt_ec[tmp_is_na == FALSE, .(수선비, 조사_수선비, 조사외_수선비)],
    na.rm = TRUE
  )
]


# ------------------------------------------------------------------------------
# Make a DT by subsetting the DT created above
# ------------------------------------------------------------------------------
# ------- Make a DT by subsetting the DT created above  -------
# 1. Make a DT by subetting the DT created above
# 1.1. Creat a DT by dropping observations covered with "*"
subdt_ec <- dt_ec[str_detect(창설연도, "\\*", negate = TRUE), ..COLS_EXTRACT]

# 1.2. Rename data fields
names(subdt_ec) <- COLS_NEW.NAMES


# ------- Modify the DT created -------
# 1. Add data field(s)
# 1.1. Add a data field showing KSIC(10th) codes
# 1.1.1. For 2015 only
subdt_ec[year == "2015", ksic_level3 := str_extract(ksic_level3, "[0-9]$")]
subdt_ec[year == "2015", ksic_level4 := str_extract(ksic_level4, "[0-9]$")]
# 1.1.2. For all
subdt_ec[, ksic := paste0(ksic_level2, ksic_level3, ksic_level4)]
stopifnot(
  (subdt_ec$ksic %>% lapply(., str_length) %>% unlist(.) != 4) %>%
    as.integer(.) %>%
    sum(.) == 0
)


# 2. Conver data field's types
# 2.1. Character to numeric
cols_to.numeric <- COLS_NEW.NAMES[
  str_detect(COLS_NEW.NAMES, "year|output|value|cost|expense|fee")
]
for(col in cols_to.numeric) {
  subdt_ec[, (col) := (get(col) %>% as.numeric)]
}


# 3. Drop unnecessary data field(s)
# (Not Applicable)


# 4. Others
cols_reorder <- c("id", "year", "ksic")
setcolorder(subdt_ec, cols_reorder)


# ------------------------------------------------------------------------------
# Export the DT created
# ------------------------------------------------------------------------------
# ------- Prepare for exporting the DT created above  -------
# 1. Set key(s)
stopifnot(subdt_ec[, .N, by = . (id)][N > 1] == 0)
setkey(subdt_ec, "id")


# 2. Label data fields
# 2.1. Create an array containing labels
cols_labels <- str_replace(COLS_EXTRACT, "_총합", "")
cols_labels[1] <- "ID Number"
cols_labels <- c(cols_labels, "산업분류코드 (10차 기준)")
names(cols_labels) <- c(COLS_NEW.NAMES, "ksic")

# 2.2. Label data fields
lapply(
  names(cols_labels),
  label_data.fields,
  dt_in.str = "subdt_ec", list_labels = cols_labels
)


# ------- Export the DT created -------
# 1. Export in .Rdata format
save("subdt_ec", file = PATH_TO.SAVE_EC_RDATA)


# 2. Export in .dta format
haven::write_dta(subdt_ec, PATH_TO.SAVE_EC_DTA, version = 15)
