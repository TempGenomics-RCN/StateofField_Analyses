################################################### Script for Data Exploration  #######################################################

#explore trends in data for "State of Field" section
#starts from cleaned & aggregated lit review data (created by "assemble_data_SoF.R" script)

##########################################################################################################################################

######## Set-up ########

remove(list = ls())
getwd() #check working directory

#load libraries
library(data.table)
library(tidyverse)

#read in data
all_data <- fread("Output/all_tempgen_data.csv")

##########################################################################################################################################

######## General data exploration ########

#### subject trends ####
#count # subject_1 occurrences by publication year
#data.table structure data.table[filter, function, grouped by what]
#.N stores the # of rows in a subject (Ex: count # rows in each cat cross-section)
subject1_by_year <- all_data[, .N, by = .(subject_1, Publication_Year)]

#plot subject_by_year
sub_by_year_plot <- ggplot(data = subject1_by_year, aes(x = Publication_Year, y = N, group = subject_1)) + 
  geom_line(aes(color = subject_1), size = 2) + 
  geom_point(aes(color = subject_1), size = 2) + 
  theme_minimal()
sub_by_year_plot

#### system trends ####
system_by_subject1 <- all_data[, .N, by = .(subject_1, system)]

#plot system_by_subject1
s_by_s_plot <- ggplot(data = system_by_subject1, aes(x = subject_1, y = N, fill = system)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
s_by_s_plot

#### taxonomic group trends ####
#plot tax distribution
tax_plot <- ggplot(data = all_data[, .N, by = .(tax_group)], aes(x = reorder(tax_group, -N), y = N, fill = tax_group)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal()
tax_plot

#tax by subject
tax_by_subject1 <- all_data[, .N, by = .(subject_1, tax_group)]

t_by_s_plot <- ggplot(data = tax_by_subject1, aes(x = subject_1, y = N, fill = tax_group)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
t_by_s_plot


#### study design trends ####

#study_design by subject
sd_by_subject <- all_data[, .N, by = .(subject_1, study_design)]
sd_by_subject <- sd_by_subject[study_design == "", study_design := NA] #change rows with blank data_type to NA

sd_by_subject_plot <- ggplot(data = na.omit(sd_by_subject), aes(x = subject_1, y = N, fill = study_design)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
sd_by_subject_plot

#study_design by year
sd_by_year <- all_data[, .N, by = .(study_design, Publication_Year)]
sd_by_year <- sd_by_year[study_design == "", study_design := NA] #change rows with blank data_type to NA

#plot sd_by_year
sd_by_year_plot <- ggplot(data = na.omit(sd_by_year), aes(x = Publication_Year, y = N, group = study_design)) + 
  geom_line(aes(color = study_design), size = 2) + 
  geom_point(aes(color = study_design), size = 2) + 
  theme_minimal()
sd_by_year_plot

#### marker type trends ####

#marker by subject
marker_by_subject <- all_data[, .N, by = .(subject_1, data_type)]
marker_by_subject <- marker_by_subject[data_type == "", data_type := NA] #change rows with blank data_type to NA

marker_by_subject_plot <- ggplot(data = na.omit(marker_by_subject), aes(x = subject_1, y = N, fill = data_type)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
marker_by_subject_plot #change position to fill to get stacked percentages --- otherwise make it "stack"

#marker by year
marker_by_year <- all_data[, .N, by = .(data_type, Publication_Year)]
marker_by_year <- marker_by_year[data_type == "", data_type := NA] #change rows with blank data_type to NA

#plot marker_by_year
marker_by_year_plot <- ggplot(data = na.omit(marker_by_year), aes(x = Publication_Year, y = N, group = data_type)) + 
  geom_line(aes(color = data_type), size = 2) + 
  geom_point(aes(color = data_type), size = 2) + 
  theme_minimal()
marker_by_year_plot

#### generation time trends ####
all_data$gen_time <- as.numeric(all_data$gen_time)

gt_by_tax_plot <- ggplot(data = all_data, aes(x = tax_group, y = gen_time, color = tax_group)) + 
  geom_boxplot() + 
  theme_minimal()
gt_by_tax_plot

gt_by_subject_plot <- ggplot(data = all_data, aes(x = subject_1, y = gen_time, color = subject_1)) + 
  geom_boxplot() + 
  theme_minimal()
gt_by_subject_plot