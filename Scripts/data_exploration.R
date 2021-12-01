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

################################################################################################

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

#### get max length of study in # gens ####
#pivoting to get gen time estimates (length of study in # generations)
#pivot longer and put all time points (expect for first one) in own row for each study
all_data_long <- as.data.table(pivot_longer(data = all_data,
                                            cols = TP_2:TP_63,
                                            names_to = "time_point",
                                            values_to = "date"))

#calculate diff (in days) between TP 1 and each subsequent TP in a study (now in date column)
#have to use absolute value bc not everyone recorded sampling dates in same order
all_data_long[, day_diff := abs(as.Date(all_data_long$TP_1, format = "%Y.%m.%d") - 
                                  as.Date(all_data_long$date, format = "%Y.%m.%d"))]

#convert to # days btwn time points to # generations
all_data_long[, gen_diff := all_data_long$day_diff/all_data_long$gen_time]

#remove day_diff & date columns (will otherwise screw up conversion back to wide format)
all_data_long[, date := NULL]
all_data_long[, day_diff := NULL]

#convert back to wide format
#now, number of generations from TP_1 is in TP_2, etc. columns
all_data_wide <- as.data.table(pivot_wider(data = all_data_long,
                                           names_from = "time_point",
                                           values_from = "gen_diff"))

#grab maximum gen_diff and put in own column
all_data_wide$max_gen_diff <- pmax(all_data_wide$TP_2, all_data_wide$TP_3, all_data_wide$TP_4, 
                                   all_data_wide$TP_5, all_data_wide$TP_6, all_data_wide$TP_7,
                                   all_data_wide$TP_8, all_data_wide$TP_9, all_data_wide$TP_10, 
                                   all_data_wide$TP_11, all_data_wide$TP_12, all_data_wide$TP_13, 
                                   all_data_wide$TP_14, all_data_wide$TP_15, all_data_wide$TP_16,
                                   all_data_wide$TP_17, all_data_wide$TP_18, all_data_wide$TP_19,
                                   all_data_wide$TP_20, all_data_wide$TP_21, all_data_wide$TP_22, 
                                   all_data_wide$TP_23, all_data_wide$TP_24, all_data_wide$TP_25,
                                   all_data_wide$TP_26, all_data_wide$TP_27, all_data_wide$TP_28,
                                   all_data_wide$TP_29, all_data_wide$TP_30, all_data_wide$TP_31, 
                                   all_data_wide$TP_32, all_data_wide$TP_33, all_data_wide$TP_34,
                                   all_data_wide$TP_35, all_data_wide$TP_36, all_data_wide$TP_37,
                                   all_data_wide$TP_38, all_data_wide$TP_39, all_data_wide$TP_40, 
                                   all_data_wide$TP_41, all_data_wide$TP_42, all_data_wide$TP_43,
                                   all_data_wide$TP_44, all_data_wide$TP_45, all_data_wide$TP_46,
                                   all_data_wide$TP_47, all_data_wide$TP_48, all_data_wide$TP_49, 
                                   all_data_wide$TP_50, all_data_wide$TP_51, all_data_wide$TP_52,
                                   all_data_wide$TP_53, all_data_wide$TP_54, all_data_wide$TP_55,
                                   all_data_wide$TP_56, all_data_wide$TP_57, all_data_wide$TP_58,
                                   all_data_wide$TP_59, all_data_wide$TP_60, all_data_wide$TP_61,
                                   all_data_wide$TP_62, all_data_wide$TP_63, na.rm = TRUE)

#remove excess columns
cols.to.del <- c("TP_2", "TP_3", "TP_4", "TP_5", "TP_6", "TP_7", "TP_8", "TP_9", "TP_10", "TP_11",
                 "TP_12", "TP_13", "TP_14", "TP_15", "TP_16", "TP_17", "TP_18", "TP_19", "TP_20", "TP_21",
                 "TP_22", "TP_23", "TP_24", "TP_25", "TP_26", "TP_27", "TP_28", "TP_29", "TP_30", "TP_31",
                 "TP_32", "TP_33", "TP_34", "TP_35", "TP_36", "TP_37", "TP_38", "TP_39", "TP_40", "TP_41",
                 "TP_42", "TP_43", "TP_44", "TP_45", "TP_46", "TP_47", "TP_48", "TP_49", "TP_50", "TP_51",
                 "TP_52", "TP_53", "TP_54", "TP_55", "TP_56", "TP_57", "TP_58", "TP_59", "TP_60", "TP_61", 
                 "TP_62", "TP_63")
all_data_wide[, (cols.to.del) := NULL]
dim(all_data_wide) #check

#### max gen time plots ####
all_data_wide$max_gen_diff <- as.numeric(all_data_wide$max_gen_diff)

maxgt_by_tax_plot <- ggplot(data = all_data_wide, aes(x = tax_group, y = max_gen_diff, color = tax_group)) + 
  geom_boxplot() + 
  #ylim(c(0, 100)) + #cutting off some groups but there are some tax (insects) with large gen diff
  theme_minimal()
maxgt_by_tax_plot

maxgt_by_subject_plot <- ggplot(data = all_data_wide, aes(x = subject_1, y = max_gen_diff, color = subject_1)) + 
  geom_boxplot() +
  #ylim(c(0, 100)) +
  theme_minimal()
maxgt_by_subject_plot

maxgt_by_driver_plot <- ggplot(data = all_data_wide, aes(x = driver_process1, y = max_gen_diff, color = driver_process1)) + 
  geom_boxplot() +
  ylim(c(0, 100)) +
  theme_minimal()
maxgt_by_driver_plot

maxgt_by_sd_plot <- ggplot(data = all_data_wide, aes(x = study_design, y = max_gen_diff, color = study_design)) + 
  geom_boxplot() +
  #ylim(c(0, 100)) +
  theme_minimal()
maxgt_by_sd_plot

maxgt_by_system_plot <- ggplot(data = all_data_wide, aes(x = system, y = max_gen_diff, color = system)) + 
  geom_boxplot() +
  ylim(c(0, 100)) +
  theme_minimal()
maxgt_by_system_plot

################################################################################################

#### driver type trends ####
#needs to to be fixed --> right now only looking at subject_1 but really needs to count if in any of the driver columns
num_drivers1 <- all_data_wide[, .N, by = .(driver_process1)]
  setnames(num_drivers1, new = c("driver_process", "N"))
num_drivers2 <- all_data_wide[, .N, by = .(driver_process2)]
  setnames(num_drivers2, new = c("driver_process", "N"))
num_drivers3 <- all_data_wide[, .N, by = .(driver_process3)]
setnames(num_drivers3, new = c("driver_process", "N"))

num_drivers_merge <- merge(num_drivers1, num_drivers2, num_drivers3, by = "driver_process")

#plot driver distribution
driver_plot <- ggplot(data = na.omit(all_data_wide[, .N, by = .(driver_process1)]), aes(x = reorder(driver_process1, -N), y = N, fill = driver_process1)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal()
driver_plot

driver_by_subject <- all_data_wide[, .N, by = .(subject_1, driver_process1)]
driver_by_subject <- driver_by_subject[driver_process1 == "", driver_process1 := NA] #change rows with blank data_type to NA

driver_by_subject_plot <- ggplot(data = na.omit(driver_by_subject), aes(x = subject_1, y = N, fill = driver_process1)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
driver_by_subject_plot #change position to fill to get stacked percentages --- otherwise make it "stack"


driver_by_sd <- all_data_wide[, .N, by = .(study_design, driver_process1)]
driver_by_sd <- driver_by_sd[driver_process1 == "", driver_process1 := NA] #change rows with blank data_type to NA
driver_by_sd <- driver_by_sd[study_design == "", study_design := NA]

driver_by_sd_plot <- ggplot(data = na.omit(driver_by_sd), aes(x = study_design, y = N, fill = driver_process1)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal()
driver_by_sd_plot #change position to fill to get stacked percentages --- otherwise make it "stack"

driver_by_year <- all_data_wide[, .N, by = .(Publication_Year, driver_process1)]
driver_by_year <- driver_by_year[driver_process1 == "", driver_process1 := NA] #change rows with blank data_type to NA

driver_by_year_plot <- ggplot(data = na.omit(driver_by_year), aes(x = Publication_Year, y = N, group = driver_process1)) + 
  geom_line(aes(color = driver_process1), size = 2) + 
  geom_point(aes(color = driver_process1), size = 2) + 
  theme_minimal()
driver_by_year_plot

driver_by_system <- all_data_wide[, .N, by = .(system, driver_process1)]
driver_by_system <- driver_by_system[driver_process1 == "", driver_process1 := NA] #change rows with blank data_type to NA

driver_by_system_plot <- ggplot(data = na.omit(driver_by_system), aes(x = system, y = N, fill = driver_process1)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal()
driver_by_system_plot #change position to fill to get stacked percentages --- otherwise make it "stack"

driver_by_taxa <- all_data_wide[, .N, by = .(tax_group, driver_process1)]
driver_by_taxa <- driver_by_taxa[driver_process1 == "", driver_process1 := NA] #change rows with blank data_type to NA

driver_by_taxa_plot <- ggplot(data = na.omit(driver_by_taxa), aes(x = tax_group, y = N, fill = driver_process1)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal()
driver_by_taxa_plot #change position to fill to get stacked percentages --- otherwise make it "stack"


#### TO DO
#incorporate # of samples and sample size ranges into analyses
#look at total range of time (not just generations) as well --> AND average time/# generations between samples (does this differ by study design?)
#geographic trends -- count up countries and plot, see if there is an interaction with type/driver of change, taxa, system, etc.
#fix type of driver --> count every instance, not just first one (won't be percent bar graph bc will add up to over 100%, either just stack OR multiple bars)
#incorporate preservation methods/tissue type/lib prep trends through time