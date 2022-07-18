# Example xlsx table
# Data Source: https://data.ca.gov/dataset/ground-water-water-quality-results/resource/9e09ac40-b694-4ddc-acd2-2e7f7d10e231
#             (Downloaded 2022-07-11)


library(tidyverse)
library(openxlsx)
library(lubridate)

# Load data ---------------


d <- read.csv("./data/gama_2022-07-01_ions.csv")

d <- d %>%
  mutate(date = ymd(gm_samp_collection_date),
         analyte = factor(gm_chemical_name, 
                          levels = c("Calcium", "Chloride", "Fluoride", "Magnesium", "Potassium", "Sodium"),
                          ordered = T),
         Units = gsub("mg/l", "mg/L", tolower(gm_result_units)),
         detected = !gm_result_modifier %in% c("ND", "<"), # identifying detects
         result = ifelse(detected == F, gm_reporting_limit, gm_result)) %>% # using reporting limit if ND
  filter(!is.na(result)) 

# Summary Data
d_sum <- d %>%
  group_by(gm_well_id, gm_chemical_name, Units) %>% 
  summarise(n = n(),
            detected = sum(detected),
            min_date = min(date),
            max_date = max(date),
            min = min(result),
            max = max(result),
            mean = mean(result)) %>%
  arrange(gm_well_id, gm_chemical_name) %>%
  select(`Well ID` = gm_well_id, `Chemical Name` = gm_chemical_name, Units,
         `Samples` = n, `First Date Sampled` = min_date, `Last Date Samples` = max_date, 
         `Minium Sample` = min, `Mean Sample` = mean, `Maximum Sample` = max) %>%
  filter(`Well ID` == "100832") %>% mutate(`Well ID` = "MW-1") %>%
  mutate(across(contains("Sample"), ~round(.x, 2)))

# openxlsx Formats
col_style <- createStyle(
  fontName = "Arial",
  fontSize = 12,
  fontColour = "black",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  fgFill = "#D9D9D9",
  wrapText = T)

d_style <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fontColour = "black",
  halign = "left",
  valign = "center",
  wrapText = T)

d_cen_style <- createStyle(
  fontName = "Arial",
  fontSize = 10,
  fontColour = "black",
  halign = "center",
  valign = "center",
  wrapText = T)

border_style_bottom_double <- createStyle(
  border = "bottom",
  borderStyle = "double")

border_style_bottom <- createStyle(
  border = "bottom",
  borderStyle = "thin")

border_style_top <- createStyle(
  border = "top",
  borderStyle = "thin")


border_style_right <- createStyle(
  border = "right",
  borderStyle = "thin")
border_style_left <- createStyle(
  border = "left",
  borderStyle = "thin")

# Format
wb <- createWorkbook()
sheet <- "Summary Stats"
addWorksheet(wb, sheet)
showGridLines(wb, sheet, showGridLines = F)

writeData(wb, sheet, x = d_sum, 
          startCol = 2, startRow = 2, borders = c("surrounding"), borderStyle = getOption("openxlsx.borderStyle", "thin"))

# Merge Cells
mergeCells(wb, sheet, rows = 2:(dim(d_sum)[1] + 2), cols = 2)

# Style
addStyle(wb, sheet, col_style, rows = 2, cols = 2:(dim(d_sum)[2] + 1), gridExpand = T, stack = T)
addStyle(wb, sheet, border_style_bottom_double, rows = 2, cols = 2:(dim(d_sum)[2] + 1), gridExpand = T, stack = T)
addStyle(wb, sheet, border_style_right, rows = 2:(dim(d_sum)[1] + 2), cols = 2:(dim(d_sum)[2] + 1), gridExpand = T, stack = T)

addStyle(wb, sheet, d_cen_style, rows = 3:(dim(d_sum)[1] + 2), cols = c(1, 4:(dim(d_sum)[2] + 1)), gridExpand = T, stack = T)
addStyle(wb, sheet, d_style, rows = 3:(dim(d_sum)[1] + 2), cols = 3, gridExpand = T, stack = T)
addStyle(wb, sheet, border_style_bottom, rows = 3:(dim(d_sum)[1] + 2), cols = 2:(dim(d_sum)[2] + 1), gridExpand = T, stack = T)
addStyle(wb, sheet, border_style_top, rows = 2, cols = 2:(dim(d_sum)[2] + 1), gridExpand = T, stack = T)
addStyle(wb, sheet, border_style_left, rows = 2:(dim(d_sum)[1] + 2), cols = 2, gridExpand = T, stack = T)

# Final Touches
# col_widths <- c()
# 
# setColWidths(wb, sheet, cols = 1:dim(d_sum)[1], widths = col_widths)

pageSetup(wb, sheet, orientation = "landscape", fitToWidth = T)

saveWorkbook(wb, "./R/Summary-Stats.xlsx", overwrite = T)
