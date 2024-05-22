# < Description > *
# > Script Group Indicator Number and Name
# : H, (Not Applicable)
#
# > Script Number(s)
# : H
#
# > Purpose of the script(s)
# : R header script for KEEI Project(s).

# ------------------------------------------------------------------------------
# Prepare for empirical analysis
# ------------------------------------------------------------------------------
# ------- Load required libraries -------
# (Not Applicable)


# ------- Clear the workspace -------
rm(list = setdiff(ls(), c("PATH_PROJ")))


# ------- Set working directory -------
# (In each R script)


# ------------------------------------------------------------------------------
# Define parameter(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# 1. Set folder name(s)
# 1.1. For scripts
PATH_SCRIPT <- "01_Scripts"
PATH_SCRIPT_BUILD <- paste(PATH_SCRIPT, "01_Data-Building", sep = "/")
PATH_SCRIPT_ANALYSIS <- paste(PATH_SCRIPT, "02_Analysis", sep = "/")

# 1.2. For data
# 1.2.1. For folders commonly used
PATH_DATA <- "02_Data"
PATH_DATA_RAW <- paste(PATH_DATA, "01_Raw-Data", sep = "/")
PATH_DATA_RAW_ORIGINAL <- paste(PATH_DATA_RAW, "01_Original", sep = "/")
PATH_DATA_RAW_USE <- paste(PATH_DATA_RAW, "02_Use", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_Intermediate-Data", sep = "/")
PATH_DATA_ANALYSIS <- paste(PATH_DATA, "03_For-Analysis", sep = "/")
# 1.2.2. For specific datasets
# 1.2.2.1. For KSIC (Korean Standard Industrial Classification) data
PATH_DATA_RAW_USE_KSIC <- paste(PATH_DATA_RAW_USE, "KSIC", sep = "/")
PATH_DATA_INTERMEDIATE_KSIC <- paste(PATH_DATA_INTERMEDIATE, "KSIC", sep = "/")
# 1.2.2.2. For 5-year history data by KSIC-10
PATH_DATA_RAW_USE_5Y.HISTORY <-
  paste(PATH_DATA_RAW_USE, "5-Year-History-Data", sep = "/")
PATH_DATA_INTERMEDIATE_5Y.HISTORY <-
  paste(PATH_DATA_INTERMEDIATE, "5-Year-History-Data", sep = "/")
# 1.2.2.3. For KCAD (Korean Classification of Administrative Districts)
PATH_DATA_RAW_USE_KCAD <- paste(PATH_DATA_RAW_USE, "KCAD", sep = "/")
PATH_DATA_INTERMEDIATE_KCAD <- paste(PATH_DATA_INTERMEDIATE, "KCAD", sep = "/")
# 1.2.2.4. For Mining and Manufacturing Survey
PATH_DATA_RAW_USE_MMS <-
  paste(PATH_DATA_RAW_USE, "Mining-and-Manufacturing-Survey", sep = "/")
PATH_DATA_INTERMEDIATE_MMS <-
  paste(PATH_DATA_INTERMEDIATE, "Mining-and-Manufacturing-Survey", sep = "/")
# 1.2.2.5. For Economic Census
PATH_DATA_RAW_USE_EC <-
  paste(PATH_DATA_RAW_USE, "Economic-Census", sep = "/")
PATH_DATA_INTERMEDIATE_EC <-
  paste(PATH_DATA_INTERMEDIATE, "Economic-Census", sep = "/")

# 1.3. For output
PATH_OUTPUT <- "03_Output"
PATH_OUTPUT_FIGURE <- paste(PATH_OUTPUT, "01_Figures", sep = "/")
PATH_OUTPUT_TABLE <- paste(PATH_OUTPUT, "02_Tables", sep = "/")


# 2. Generate the folders defined above
paths <- c(
  PATH_SCRIPT,
  PATH_SCRIPT_BUILD,
  PATH_SCRIPT_ANALYSIS,
  PATH_DATA,
  PATH_DATA_RAW,
  PATH_DATA_RAW_ORIGINAL,
  PATH_DATA_RAW_USE,
  PATH_DATA_RAW_USE_KSIC,
  PATH_DATA_RAW_USE_5Y.HISTORY,
  PATH_DATA_RAW_USE_KCAD,
  PATH_DATA_RAW_USE_MMS,
  PATH_DATA_RAW_USE_EC,
  PATH_DATA_INTERMEDIATE,
  PATH_DATA_INTERMEDIATE_KSIC,
  PATH_DATA_INTERMEDIATE_5Y.HISTORY,
  PATH_DATA_INTERMEDIATE_KCAD,
  PATH_DATA_INTERMEDIATE_MMS,
  PATH_DATA_INTERMEDIATE_EC,
  PATH_DATA_ANALYSIS,
  PATH_OUTPUT,
  PATH_OUTPUT_FIGURE,
  PATH_OUTPUT_TABLE
)
for (path in paths) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}


# ------- Define parameters commonly used -------
# 1. Parameter(s) utilized for creating ggplot object(s)
# 1.1. A list including common plot options
PLOT.OPTIONS <- list(
  ggplot2::theme_linedraw(),
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 12)),
    legend.text =
      ggplot2::element_text(margin = ggplot2::margin(r = 10, unit = "pt")),
    legend.position = "bottom",
    legend.margin = ggplot2::margin(-0.5, 0, 0, 0, unit = "cm"),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.box.margin = ggplot2::margin(0.0, 0, 0.1, 0, unit = "cm")
  )
)

# 1.2. Color palette(s)
COL.PAL_CUSTOM <-
  unikn::usecol(c("firebrick", "gold", "forestgreen", "steelblue"))


# ------------------------------------------------------------------------------
# Define function(s)
# ------------------------------------------------------------------------------
# ------- Function(s) related to data cleansing -------
# 1. To ingest MDIS dataset(s), which is(are) in .csv format
helper_ingest <- function(path) {
  tmp_dt <- 
    read_csv(
      path,
      locale = locale(encoding = "euc-kr"),
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ) %>%
    setDT(.)
  
  tmp_year <- tmp_dt[, .N, by = .(조사기준연도)]$조사기준연도
  tmp_n <- tmp_dt[, .N]
  tmp_dt[, id := paste(tmp_year, 1:tmp_n, sep = "-")]

  setcolorder(tmp_dt, "id")

  return(tmp_dt)
}


# ------- Function(s) related to regression analysis -------
# (Not Applicable)


# ------- Functions for miscellaneous tasks -------
# 1. To export a ggplot object in PNG format
export_figure.in.png <- function (
  ggplot.obj, filename_str,
  width_numeric = 20, height_numeric = 10, units_str = "cm", dpi_int = 320
) {
  ggplot2::ggsave(
    plot = ggplot.obj,
    filename = filename_str,
    dpi = dpi_int,
    width = width_numeric,
    height = height_numeric,
    units = units_str,
    device = "png",
    limitsize = FALSE
  )
}


# 2. To label a DT's columns
label_data.fields <- function(
  dt_in.str, data.field_to.label_in.str, list_labels
) {
  tmp_obj.name <- paste(dt_in.str, data.field_to.label_in.str, sep = "$")
  data.table::setattr(
    eval(parse(text = tmp_obj.name)),
    name = "label",
    value = list_labels[[data.field_to.label_in.str]]
  )
}
