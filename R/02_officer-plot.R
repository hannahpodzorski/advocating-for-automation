# Example Exporting Plot to PowerPoint
# Data Source: https://data.ca.gov/dataset/ground-water-water-quality-results/resource/9e09ac40-b694-4ddc-acd2-2e7f7d10e231
#             (Downloaded 2022-07-11)

library(tidyverse)
library(officer)
library(lubridate)

# Load data ---------------
d <- read.csv("./data/gama_2022-07-01_ions.csv")

d <- d %>%
  mutate(date = ymd(gm_samp_collection_date),
         analyte = factor(gm_chemical_name, 
                          levels = c("Calcium", "Chloride", "Fluoride", "Magnesium", "Potassium", "Sodium"),
                          ordered = T),
         Units = gsub("mg/l", "mg/L", tolower(gm_result_units)),
         detected = ifelse(!gm_result_modifier %in% c("ND", "<"), "Detect", "Non-Detect"), # identifying detects
         result = ifelse(detected == F, gm_reporting_limit, gm_result)) %>% # using reporting limit if ND
  filter(!is.na(result), gm_chemical_name == "Chloride", gm_well_id == "100832") %>%
  arrange(date)

# Theme for plot ----------------------------
theme =  (theme(text = element_text(family = "sans"))
          + theme_bw()
          + theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "vertical",
                  legend.text = element_text(color="black", size=10, hjust = 0.5), legend.spacing.y = unit(0, "char"))
          + theme(panel.background = element_blank(), panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black")) 
          + theme(axis.text.x = element_text(color="black", size=10, vjust=0.5,hjust = .5)) 
          + theme(axis.text.y = element_text(color="black", size=10, vjust=0.5)) 
          + theme(axis.title.x=element_text(color="black", size=11, face="bold")) 
          + theme(axis.title.y=element_text(color="black", size=11, vjust=2, face="bold")) 
          + theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold")) 
          + theme(plot.subtitle = element_text(size = 12, hjust = 0.5)) 
          + theme(strip.background =element_rect(fill="white")) 
          + theme(strip.text = element_text(face="bold"))
          + theme(plot.margin = unit(c(1,1,1,1),"cm")))

plot_col <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Timeseries Plot -----------------------------
ts_plot <- ggplot(data = d) + # set data
  geom_point(aes(x = date, y = result, color =  gm_chemical_name, shape = detected), size = 3) + 
  geom_path(aes(x = date, y = gm_result, color =  gm_chemical_name)) + 
  scale_shape_manual(values = c("Detect" = 19, "Non-Detect" = 21), guide_legend(order = 2)) + # Define shapes based on detected column 
  scale_color_manual(values = c("Chloride" = plot_col[1]), 
                     guide = guide_legend(order = 1)) +  # include in legend 1st
  labs(y = "Concentration (mg/L)", x = "Sample Date", 
       title = "Groundwater Concentration Trends – Chloride",
       subtitle = "MW-1") +
  scale_x_date(date_labels = "%Y", breaks = "2 years") + # format x-axis
  theme # add pre-defined theme

# Save to PowerPoint ---------------------------
ts_plot <- rvg::dml(ggobj = ts_plot) # ggplot will now be editable

pptx <- read_pptx() %>%
  add_slide() %>%
  ph_with(ts_plot, ph_location(left = 0.3, top = 0.1, width = 8.75, height = 6.9))

print(pptx, "./R/Fig-Example.pptx")  
