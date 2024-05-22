# < Description > *
# > Script Group Indicator Number and Name
# : A-01, Data Building
#
# > Script Number(s)
# : A-01A
#
# > Purpose of the script(s)
# : Ingest 10th KSIC (Korean Standard Industrial Classification, KSIC) data
#   from an Excel file, and then save it as .dta format.
#
# > Source
# : https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp


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
FILE_TO.LOAD_KSIC <- "한국표준산업분류(10차)_영문.xlsx"
PATH_TO.LOAD_KSIC <-
  paste(PATH_DATA_RAW_USE_KSIC, FILE_TO.LOAD_KSIC, sep = "/")


# 2. Path(s) for saving file(s) and/or script(s)
# 2.1. In .RData format
FILE_TO.SAVE_KSIC_RDATA <-
  "Korean-Standard-Industrial-Classification_10th.RData"
PATH_TO.SAVE_KSIC_RDATA <-
  paste(PATH_DATA_INTERMEDIATE_KSIC, FILE_TO.SAVE_KSIC_RDATA, sep = "/")
# 2.2. In .dta format
FILE_TO.SAVE_KSIC_DTA <- "Korean-Standard-Industrial-Classification_10th.dta"
PATH_TO.SAVE_KSIC_DTA <-
  paste(PATH_DATA_INTERMEDIATE_KSIC, FILE_TO.SAVE_KSIC_DTA, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Ingest a .xlsx file, and then save it as .dta file
# ------------------------------------------------------------------------------
# ------- Ingest an Excel file -------
tmp_dt_ksic <-
  wb_read(
    file = PATH_TO.LOAD_KSIC, sheet = "영문",
    start_row = 3, col_names = FALSE
  ) %>%
    setDT(.)
names(tmp_dt_ksic) <- c("code", "desc_ko", "desc_en")
tmp_dt_ksic[, desc_en := str_to_title(desc_en)]
tmp_dt_ksic[, tmp_code := (str_extract(code, "^[0-9]{2}") %>% as.integer(.))]


# ------- Create a DT incl. section-level code -------
tmp_dt_section <- tmp_dt_ksic[str_detect(code, "^[0-9]{2}$"), .(code)]
tmp_dt_section[, tmp_code := as.integer(code)]
tmp_dt_section[
  tmp_code %in% 1:3,
  `:=` (
    code_section = "A",
    section_ko = "농업, 임업 및 어업",
    section_en = "Agriculture, Forestry, and Fishing"
  )
][
  tmp_code %in% 5:8,
  `:=` (
    code_section = "B",
    section_ko = "광업",
    section_en = "Mining and Quarrying"
  )
][
  tmp_code %in% 10:34,
  `:=` (
    code_section = "C",
    section_ko = "제조업",
    section_en = "Manufacturing"
  )
][
  tmp_code %in% 35,
  `:=` (
    code_section = "D",
    section_ko = "전기, 가스, 증기 및 공기조절 공급업",
    section_en = "Electricity, Gas, Steam, and Air Conditioning Supply"
  )
][
  tmp_code %in% 36:39,
  `:=` (
    code_section = "E",
    section_ko = "수도, 하수 및 폐기물 처리, 원료 재생업",
    section_en = "Water Supply, Sewer, Waste Management, Material Recovery"
  )
][
  tmp_code %in% 41:42,
  `:=` (
    code_section = "F",
    section_ko = "건설업",
    section_en = "Construction"
  )
][
  tmp_code %in% 45:47,
  `:=` (
    code_section = "G",
    section_ko = "도매 및 소매업",
    section_en = "Wholesale and Retail Trade"
  )
][
  tmp_code %in% 49:52,
  `:=` (
    code_section = "H",
    section_ko = "운수 및 창고업",
    section_en = "Transportation and Storage"
  )
][
  tmp_code %in% 55:56,
  `:=` (
    code_section = "I",
    section_ko = "숙박 및 음식점업",
    section_en = "Accommodation and Food Service Activities"
  )
][
  tmp_code %in% 58:63,
  `:=` (
    code_section = "J",
    section_ko = "정보통신업",
    section_en = "Information and Communication"
  )
][
  tmp_code %in% 64:66,
  `:=` (
    code_section = "K",
    section_ko = "금융 및 보험업",
    section_en = "Financial and Insurance Activities"
  )
][
  tmp_code %in% 68,
  `:=` (
    code_section = "L",
    section_ko = "부동산업",
    section_en = "Real Estate Activities"
  )
][
  tmp_code %in% 70:73,
  `:=` (
    code_section = "M",
    section_ko = "전문, 과학 및 기술 서비스업",
    section_en = "Professional, Scientific, and Technical Activities"
  )
][
  tmp_code %in% 74:76,
  `:=` (
    code_section = "N",
    section_ko = "사업시설 관리, 사업 지원 및 임대 서비스업",
    section_en = paste0(
      "Business Facilities Management and Business Support Services, ",
      "Rental and Leasing Activities"
    )
  )
][
  tmp_code %in% 84,
  `:=` (
    code_section = "O",
    section_ko = "공공행정, 국방 및 사회보장 행정",
    section_en = "Public Administration and Defence, Compulsory Social Security"
  )
][
  tmp_code %in% 85,
  `:=` (
    code_section = "P",
    section_ko = "교육 서비스업",
    section_en = "Education"
  )
][
  tmp_code %in% 86:87,
  `:=` (
    code_section = "Q",
    section_ko = "보건업 및 사회복지 서비스업",
    section_en = "Human Health and Social Work Activities"
  )
][
  tmp_code %in% 90:91,
  `:=` (
    code_section = "R",
    section_ko = "예술, 스포츠 및 여가관련 서비스업",
    section_en = "Arts, Sports, and Recreation related Activities"
  )
][
  tmp_code %in% 94:96,
  `:=` (
    code_section = "S",
    section_ko = "협회 및 단체, 수리 및 기타 개인 서비스업",
    section_en = "Membership Organizations, Repair and Other Personal Services"
  )
][
  tmp_code %in% 97:98,
  `:=` (
    code_section = "T",
    section_ko = "가구 내 고용활동 및 달리 분류되지 않은 자가소비 생산활동",
    section_en = paste0(
      "Activities of Households as Employers, Undifferentiated Goods- and,",
      "Services-producing Activities of Households for Own Use"
    )
  )
][
  tmp_code %in% 99,
  `:=` (
    code_section = "U",
    section_ko = "국제 및 외국기관",
    section_en = "Activities of Extraterritorial Organizations and Bodies"
  )
]
tmp_dt_section[, section_en := str_to_title(section_en)]


# ------- Create a DT containing KSIC codes -------
# 1. Create a DT by merging the DTs created above
dt_ksic <- merge(
  x = tmp_dt_ksic[!is.na(tmp_code)],
  y = tmp_dt_section[, .(tmp_code, code_section, section_ko, section_en)],
  by = "tmp_code",
  all.x = TRUE
)


# 2. Modify the DT
# 2.1. Drop unnecessary data field(s)
dt_ksic[, tmp_code := NULL]

# 2.2. Add data field(s)
# 2.2.1. Add data fields showing code levels
dt_ksic[
  str_detect(code, "^[0-9]{1}$"),
  `:=`(level_ko = "대분류", level_en = "Section")
]
dt_ksic[
  str_detect(code, "^[0-9]{2}$"),
  `:=`(level_ko = "중분류", level_en = "Division")
]
dt_ksic[
  str_detect(code, "^[0-9]{3}$"),
  `:=`(level_ko = "소분류", level_en = "Group")
]
dt_ksic[
  str_detect(code, "^[0-9]{4}$"),
  `:=`(level_ko = "세분류", level_en = "Class")
]
dt_ksic[
  str_detect(code, "^[0-9]{5}$"),
  `:=`(level_ko = "세세분류", level_en = "Sub-Class")
]

# 2.3. Recorder columns
cols_reorder <- c(
  "code_section", "section_ko", "section_en",
  "code", "level_ko", "level_en",  "desc_ko", "desc_en"
)
setcolorder(dt_ksic, cols_reorder)


# 2.4. Label data fields
# 2.4.1. Create object(s)
array_data.field <- names(dt_ksic)
list_labels <- vector("list", length(array_data.field))
names(list_labels) <- array_data.field
list_labels[["code_section"]] <- "대분류 코드"
list_labels[["section_ko"]] <- "대분류 한글명"
list_labels[["section_en"]] <- "대분류 영문명"
list_labels[["code"]] <- "KSIC 분류코드"
list_labels[["level_ko"]] <- "한글 분류수준"
list_labels[["level_en"]] <- "영문 분류수준"
list_labels[["desc_ko"]] <- "한글 분류명"
list_labels[["desc_en"]] <- "영문 분류명"
# 2.4.2. Label data fields
lapply(
  array_data.field,
  label_data.fields,
  dt_in.str = "dt_ksic", list_labels = list_labels
)


# ------- Export the DT -------
# 1. Export in .Rdata format
save("dt_ksic", file = PATH_TO.SAVE_KSIC_RDATA)

# 2. Export in .dta format
haven::write_dta(dt_ksic, PATH_TO.SAVE_KSIC_DTA, version = 15)
