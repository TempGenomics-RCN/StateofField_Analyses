################################################### Script for Data Exploration  #######################################################

#explore trends in data for "State of Field" section
#starts from cleaned & aggregated lit review data (created by "assemble_data_SoF.R" script)
#plots 1000 x 1000

##########################################################################################################################################

######## Set-up ########

remove(list = ls())
getwd() #check working directory

#load libraries
library(here)
library(data.table)
library(tidyverse)
library(ggpattern)

#read in data
all_data <- fread(here("Output", "all_tempgen_data.csv"))

#create non-duplicated dataset
#some studies have multiple rows (bc looked at more than one species, marker type, etc)
#don't want to double count these
all_data_deduplicate <- all_data[!duplicated(all_data$Article_Number), ] #down from 298 to 229 rows

##########################################################################################################################################

######## Subject data exploration ########

#### subject by publication year ####
#using deduplicated since interested in subject of publication -- don't want to inflate with multiple studies in a publication

#count # subject occurrences by publication year
#data.table structure data.table[filter, function, grouped by what]
#.N stores the # of rows in a subject (Ex: count # rows in each cat cross-section)
subject1_by_year <- all_data_deduplicate[, .N, by = .(subject_1, Publication_Year)]
  colnames(subject1_by_year) <- c("subject", "Publication_Year", "N1")
subject2_by_year <- all_data_deduplicate[, .N, by = .(subject_2, Publication_Year)]
  subject2_by_year <- subset(subject2_by_year, subject2_by_year$subject_2 != "")
  colnames(subject2_by_year) <- c("subject", "Publication_Year", "N2")
subject3_by_year <- all_data_deduplicate[, .N, by = .(subject_3, Publication_Year)]
  subject3_by_year <- subset(subject3_by_year, subject3_by_year$subject_3 != "")
  colnames(subject3_by_year) <- c("subject", "Publication_Year", "N3")
subject4_by_year <- all_data_deduplicate[, .N, by = .(subject_4, Publication_Year)]
  subject4_by_year <- subset(subject4_by_year, subject4_by_year$subject_4 != "")
  colnames(subject4_by_year) <- c("subject", "Publication_Year", "N4")

#merge subject_by_year data.tables
subject_by_year_list <- list(subject1_by_year, subject2_by_year, subject3_by_year, subject4_by_year)
  subject_by_year <- subject_by_year_list %>% reduce(full_join, by = c("subject", "Publication_Year"), all = TRUE)

#sum across columns to get total N subject_by_year
subject_by_year$Ntot <- rowSums(subject_by_year[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)
  
#plot subject_by_year
#if ever want just first subject, change back to subject1_by_year data.table
sub_by_year_plot <- ggplot(data = subject_by_year, aes(x = Publication_Year, y = Ntot, group = subject)) + 
  geom_smooth(aes(color = subject), size = 4, se = FALSE) + 
  #geom_point(aes(color = subject), size = 2) + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
sub_by_year_plot

##########################################################################################################################################

######## System data exploration ########

#### system by subject ####
#using deduplicated since interested in system of publication -- any publication with multiple rows should be due to diff marker same system OR diff taxa same system

#count # system occurrences by subject
system_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, system)]
  colnames(system_by_subject1) <- c("subject", "system", "N1")
system_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, system)]
  system_by_subject2 <- subset(system_by_subject2, system_by_subject2$subject_2 != "")
  colnames(system_by_subject2) <- c("subject", "system", "N2")
system_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, system)]
  system_by_subject3 <- subset(system_by_subject3, system_by_subject3$subject_3 != "")
  colnames(system_by_subject3) <- c("subject", "system", "N3")
system_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, system)]
  system_by_subject4 <- subset(system_by_subject4, system_by_subject4$subject_4 != "")
  colnames(system_by_subject4) <- c("subject", "system", "N4")
  system_by_subject4 <- system_by_subject4[-4 ,] #wouldn't delete last row with missing data for some reason, doing manually

#merge system_by_subject data.tables
system_by_subject_list <- list(system_by_subject1, system_by_subject2, system_by_subject3, system_by_subject4)
  system_by_subject <- system_by_subject_list %>% reduce(full_join, by = c("system", "subject"), all = TRUE)
  
#sum across columns to get total N system_by_subject
system_by_subject$Ntot <- rowSums(system_by_subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)

#plot system_by_subject
#position = fill to get relative percentages, position = stack to get absolute counts
s_by_s_plot <- ggplot(data = system_by_subject, aes(x = subject, y = Ntot, fill = system)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
s_by_s_plot

##########################################################################################################################################

######## Taxa data exploration ########

#### tax distribution ####
#should this be deduplicate? probably. studies may look at one or more organisms in same taxa
#this is number of STUDIES/taxa

#plot tax distribution
tax_plot <- ggplot(data = all_data_deduplicate[, .N, by = .(tax_group)], aes(x = reorder(tax_group, -N), y = N, fill = tax_group)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal() + xlab("taxonomic group (class)") + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), axis.text.x = element_text(angle = 315), 
        legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
tax_plot

#### taxa by subject ####

#count # taxa occurrences by subject
tax_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, tax_group)]
  colnames(tax_by_subject1) <- c("subject", "tax_group", "N1")
tax_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, tax_group)]
  tax_by_subject2 <- subset(tax_by_subject2, tax_by_subject2$subject_2 != "")  
  colnames(tax_by_subject2) <- c("subject", "tax_group", "N2")
tax_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, tax_group)]
  tax_by_subject3 <- subset(tax_by_subject3, tax_by_subject3$subject_3 != "")  
  colnames(tax_by_subject3) <- c("subject", "tax_group", "N3")
tax_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, tax_group)]
  tax_by_subject4 <- subset(tax_by_subject4, tax_by_subject4$subject_4 != "")  
  colnames(tax_by_subject4) <- c("subject", "tax_group", "N4")
  tax_by_subject4 <- tax_by_subject4[-5 ,] #wouldn't delete 5th row with missing data for some reason, doing manually

#merge tax_by_subject data.tables
tax_by_subject_list <- list(tax_by_subject1, tax_by_subject2, tax_by_subject3, tax_by_subject4)
  tax_by_subject <- tax_by_subject_list %>% reduce(full_join, by = c("tax_group", "subject"), all = TRUE)

#sum across columns to get total N tax_by_subject
tax_by_subject$Ntot <- rowSums(tax_by_subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)

#plot tax_by_subject
t_by_s_plot <- ggplot(data = tax_by_subject, aes(x = subject, y = Ntot, fill = tax_group)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
t_by_s_plot

##### taxa by system ####
tax_by_system <- all_data_deduplicate[, .N, by = .(system, tax_group)]

#plot taxa by system
t_by_syst_plot <- ggplot(data = tax_by_system, aes(x = system, y = N, fill = tax_group)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
t_by_syst_plot

##########################################################################################################################################

######## Study design data exploration ########

##### study_design by subject ####
#using deduplicated since interested in study_design of publication -- any publication with multiple rows should be due to diff marker/taxa same study design

#count # study_design occurrences by subject
sd_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, study_design)]
  colnames(sd_by_subject1) <- c("subject", "study_design", "N1")
sd_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, study_design)]
  sd_by_subject2 <- subset(sd_by_subject2, sd_by_subject2$subject_2 != "")
  colnames(sd_by_subject2) <- c("subject", "study_design", "N2")
sd_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, study_design)]
  sd_by_subject3 <- subset(sd_by_subject3, sd_by_subject3$subject_3 != "")
  colnames(sd_by_subject3) <- c("subject", "study_design", "N3")
sd_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, study_design)]
  sd_by_subject4 <- subset(sd_by_subject4, sd_by_subject4$subject_4 != "")
  colnames(sd_by_subject4) <- c("subject", "study_design", "N4")
  sd_by_subject4 <- sd_by_subject4[-4 ,] #wouldn't delete last row with missing data for some reason, doing manually
  
#merge sd_by_subject data.tables
sd_by_subject_list <- list(sd_by_subject1, sd_by_subject2, sd_by_subject3, sd_by_subject4)
  sd_by_subject <- sd_by_subject_list %>% reduce(full_join, by = c("study_design", "subject"), all = TRUE)
  
#sum across columns to get total N sd_by_subject
sd_by_subject$Ntot <- rowSums(sd_by_subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)
  sd_by_subject <- subset(sd_by_subject, sd_by_subject$study_design != "")

#plot sd by system
sd_by_subject_plot <- ggplot(data = sd_by_subject, aes(x = subject, y = Ntot, fill = study_design)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
sd_by_subject_plot

#### study_design by taxa ####
sd_by_tax <- all_data_deduplicate[, .N, by = .(study_design, tax_group)]

#plot sd by taxa
sd_by_tax_plot <- ggplot(data = na.omit(sd_by_tax), aes(x = study_design, y = N, fill = tax_group)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
sd_by_tax_plot

#### study_design by year ####
sd_by_year <- all_data_deduplicate[, .N, by = .(study_design, Publication_Year)]
  sd_by_year <- sd_by_year[study_design == "", study_design := NA] #change rows with blank data_type to NA

#plot sd_by_year
sd_by_year_plot <- ggplot(data = na.omit(sd_by_year), aes(x = Publication_Year, y = N, group = study_design)) + 
  geom_smooth(aes(color = study_design), size = 4, se = FALSE) + 
  #geom_point(aes(color = study_design), size = 2) + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
sd_by_year_plot

##########################################################################################################################################

######## Marker type data exploration ########
#NOT "usual" deduplicated bc different studies use different markers

#removing rows if have same Article_Number (same publication) and same marker type
all_data_dedup_marker <- distinct(all_data, Article_Number, data_type, .keep_all = TRUE)

#### marker by subject ####

#count # marker occurrences by subject
marker_by_subject1 <- all_data_dedup_marker[, .N, by = .(subject_1, data_type)]
  colnames(marker_by_subject1) <- c("subject", "data_type", "N1")
marker_by_subject2 <- all_data_dedup_marker[, .N, by = .(subject_2, data_type)]
  marker_by_subject2 <- subset(marker_by_subject2, marker_by_subject2$subject_2 != "")
  colnames(marker_by_subject2) <- c("subject", "data_type", "N2")
marker_by_subject3 <- all_data_dedup_marker[, .N, by = .(subject_3, data_type)]
  marker_by_subject3 <- subset(marker_by_subject3, marker_by_subject3$subject_3 != "")
  colnames(marker_by_subject3) <- c("subject", "data_type", "N3")
marker_by_subject4 <- all_data_dedup_marker[, .N, by = .(subject_4, data_type)]
  marker_by_subject4 <- subset(marker_by_subject4, marker_by_subject4$subject_4 != "")
  colnames(marker_by_subject4) <- c("subject", "data_type", "N4")
  marker_by_subject4 <- marker_by_subject4[-5 ,] #wouldn't delete 5th row with missing data for some reason, doing manually

marker_by_subject <- marker_by_subject[data_type == "", data_type := NA] #change rows with blank data_type to NA

#merge marker_by_subject data.tables
marker_by_subject_list <- list(marker_by_subject1, marker_by_subject2, marker_by_subject3, marker_by_subject4)
  marker_by_subject <- marker_by_subject_list %>% reduce(full_join, by = c("subject", "data_type"), all = TRUE)

#sum across columns to get total N marker_by_subject
marker_by_subject$Ntot <- rowSums(marker_by_subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)
  marker_by_subject <- subset(marker_by_subject, marker_by_subject$data_type != "")

#plot marker_by_subject
marker_by_subject_plot <- ggplot(data = marker_by_subject, aes(x = subject, y = Ntot, fill = data_type)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
marker_by_subject_plot

#### marker by year ####
marker_by_year <- all_data_dedup_marker[, .N, by = .(data_type, Publication_Year)]
  marker_by_year <- marker_by_year[data_type == "", data_type := NA] #change rows with blank data_type to NA

#plot marker_by_year
#HRM & X-chromosome don't show bc just a point
marker_by_year_plot <- ggplot(data = na.omit(marker_by_year), aes(x = Publication_Year, y = N, group = data_type)) + 
  geom_smooth(aes(color = data_type), size = 4, se = FALSE) + 
  #geom_point(aes(color = data_type), size = 2) + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
marker_by_year_plot

################################################################################################

######## Generation time data exploration ########
#NOT "usual" deduplicated bc different studies have different species (w/diff generation times)

#removing rows if have same Article_Number (same publication) and same generation time (likely same organism then --> exceptions to this?)
all_data_dedup_gentime <- distinct(all_data, Article_Number, gen_time, .keep_all = TRUE)
  all_data_dedup_gentime$gen_time <- as.numeric(all_data_dedup_gentime$gen_time) #make sure gentime numeric

#### gentime by taxa ####
  
#plot gentime_by_taxa (1500x1000)
gt_by_tax_plot <- ggplot(data = all_data_dedup_gentime, aes(x = tax_group, y = gen_time, color = tax_group)) + 
  geom_boxplot(size = 2) + 
  theme_minimal() + ylab("gen_time (days)") + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right",
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
gt_by_tax_plot

################################################################################################

######## Max year & gentime data exploration ########
#NOT "usual" deduplicated bc different studies have different species (w/diff generation times)

#### get max length of study in # gens ####
#pivoting to get gen time estimates (length of study in # generations)
#pivot longer and put all time points (expect for first one) in own row for each study
all_data_dedup_gentime_long <- as.data.table(pivot_longer(data = all_data_dedup_gentime,
                                            cols = TP_2:TP_63,
                                            names_to = "time_point",
                                            values_to = "date"))

#calculate diff (in days) between TP 1 and each subsequent TP in a study (now in date column)
#have to use absolute value bc not everyone recorded sampling dates in same order
#NOTE: this might not be totally accurate --- some studies don't sample all sites at earliest & latest time point...
#should be a good approximation though?
all_data_dedup_gentime_long[, day_diff := abs(as.Date(all_data_dedup_gentime_long$TP_1, format = "%Y.%m.%d") - 
                                  as.Date(all_data_dedup_gentime_long$date, format = "%Y.%m.%d"))]

#convert to # days btwn time points to # generations
all_data_dedup_gentime_long[, gen_diff := all_data_dedup_gentime_long$day_diff/all_data_dedup_gentime_long$gen_time]

#remove date columns (will otherwise screw up conversion back to wide format)
all_data_dedup_gentime_long[, date := NULL]

#convert back to wide format
#now, number of generations from TP_1 is in TP_2, etc. columns
all_data_dedup_gentime_long_nodaydiff <- all_data_dedup_gentime_long[, !"day_diff"] #remove day_diff (otherwise screw up conversion)
all_data_dedup_gentime_wide <- as.data.table(pivot_wider(data = all_data_dedup_gentime_long_nodaydiff,
                                           names_from = "time_point",
                                           values_from = "gen_diff"))

#grab maximum gen_diff and put in own column
all_data_dedup_gentime_wide$max_gen_diff <- pmax(all_data_dedup_gentime_wide$TP_2, all_data_dedup_gentime_wide$TP_3, all_data_dedup_gentime_wide$TP_4, 
                                   all_data_dedup_gentime_wide$TP_5, all_data_dedup_gentime_wide$TP_6, all_data_dedup_gentime_wide$TP_7,
                                   all_data_dedup_gentime_wide$TP_8, all_data_dedup_gentime_wide$TP_9, all_data_dedup_gentime_wide$TP_10, 
                                   all_data_dedup_gentime_wide$TP_11, all_data_dedup_gentime_wide$TP_12, all_data_dedup_gentime_wide$TP_13, 
                                   all_data_dedup_gentime_wide$TP_14, all_data_dedup_gentime_wide$TP_15, all_data_dedup_gentime_wide$TP_16,
                                   all_data_dedup_gentime_wide$TP_17, all_data_dedup_gentime_wide$TP_18, all_data_dedup_gentime_wide$TP_19,
                                   all_data_dedup_gentime_wide$TP_20, all_data_dedup_gentime_wide$TP_21, all_data_dedup_gentime_wide$TP_22, 
                                   all_data_dedup_gentime_wide$TP_23, all_data_dedup_gentime_wide$TP_24, all_data_dedup_gentime_wide$TP_25,
                                   all_data_dedup_gentime_wide$TP_26, all_data_dedup_gentime_wide$TP_27, all_data_dedup_gentime_wide$TP_28,
                                   all_data_dedup_gentime_wide$TP_29, all_data_dedup_gentime_wide$TP_30, all_data_dedup_gentime_wide$TP_31, 
                                   all_data_dedup_gentime_wide$TP_32, all_data_dedup_gentime_wide$TP_33, all_data_dedup_gentime_wide$TP_34,
                                   all_data_dedup_gentime_wide$TP_35, all_data_dedup_gentime_wide$TP_36, all_data_dedup_gentime_wide$TP_37,
                                   all_data_dedup_gentime_wide$TP_38, all_data_dedup_gentime_wide$TP_39, all_data_dedup_gentime_wide$TP_40, 
                                   all_data_dedup_gentime_wide$TP_41, all_data_dedup_gentime_wide$TP_42, all_data_dedup_gentime_wide$TP_43,
                                   all_data_dedup_gentime_wide$TP_44, all_data_dedup_gentime_wide$TP_45, all_data_dedup_gentime_wide$TP_46,
                                   all_data_dedup_gentime_wide$TP_47, all_data_dedup_gentime_wide$TP_48, all_data_dedup_gentime_wide$TP_49, 
                                   all_data_dedup_gentime_wide$TP_50, all_data_dedup_gentime_wide$TP_51, all_data_dedup_gentime_wide$TP_52,
                                   all_data_dedup_gentime_wide$TP_53, all_data_dedup_gentime_wide$TP_54, all_data_dedup_gentime_wide$TP_55,
                                   all_data_dedup_gentime_wide$TP_56, all_data_dedup_gentime_wide$TP_57, all_data_dedup_gentime_wide$TP_58,
                                   all_data_dedup_gentime_wide$TP_59, all_data_dedup_gentime_wide$TP_60, all_data_dedup_gentime_wide$TP_61,
                                   all_data_dedup_gentime_wide$TP_62, all_data_dedup_gentime_wide$TP_63, na.rm = TRUE)

#remove excess columns
cols.to.del <- c("TP_2", "TP_3", "TP_4", "TP_5", "TP_6", "TP_7", "TP_8", "TP_9", "TP_10", "TP_11",
                 "TP_12", "TP_13", "TP_14", "TP_15", "TP_16", "TP_17", "TP_18", "TP_19", "TP_20", "TP_21",
                 "TP_22", "TP_23", "TP_24", "TP_25", "TP_26", "TP_27", "TP_28", "TP_29", "TP_30", "TP_31",
                 "TP_32", "TP_33", "TP_34", "TP_35", "TP_36", "TP_37", "TP_38", "TP_39", "TP_40", "TP_41",
                 "TP_42", "TP_43", "TP_44", "TP_45", "TP_46", "TP_47", "TP_48", "TP_49", "TP_50", "TP_51",
                 "TP_52", "TP_53", "TP_54", "TP_55", "TP_56", "TP_57", "TP_58", "TP_59", "TP_60", "TP_61", 
                 "TP_62", "TP_63")
all_data_dedup_gentime_wide[, (cols.to.del) := NULL]
dim(all_data_dedup_gentime_wide) #check 239 x 145

#### get max length of study in # years ####
#starting from all_data_dedup_gentime_wide dataset

#convert back to wide format
#now, # of DAYS from TP_1 is in TP_2, etc. columns
all_data_dedup_time_long <- all_data_dedup_gentime_long[, !"gen_diff"] #remove gen_diff (otherwise screw up conversion)
all_data_dedup_time_wide <- as.data.table(pivot_wider(data = all_data_dedup_time_long,
                                                         names_from = "time_point",
                                                         values_from = "day_diff"))

#grab maximum day_diff and put in own column
all_data_dedup_time_wide$max_day_diff <- pmax(all_data_dedup_time_wide$TP_2, all_data_dedup_time_wide$TP_3, all_data_dedup_time_wide$TP_4, 
                                                 all_data_dedup_time_wide$TP_5, all_data_dedup_time_wide$TP_6, all_data_dedup_time_wide$TP_7,
                                                 all_data_dedup_time_wide$TP_8, all_data_dedup_time_wide$TP_9, all_data_dedup_time_wide$TP_10, 
                                                 all_data_dedup_time_wide$TP_11, all_data_dedup_time_wide$TP_12, all_data_dedup_time_wide$TP_13, 
                                                 all_data_dedup_time_wide$TP_14, all_data_dedup_time_wide$TP_15, all_data_dedup_time_wide$TP_16,
                                                 all_data_dedup_time_wide$TP_17, all_data_dedup_time_wide$TP_18, all_data_dedup_time_wide$TP_19,
                                                 all_data_dedup_time_wide$TP_20, all_data_dedup_time_wide$TP_21, all_data_dedup_time_wide$TP_22, 
                                                 all_data_dedup_time_wide$TP_23, all_data_dedup_time_wide$TP_24, all_data_dedup_time_wide$TP_25,
                                                 all_data_dedup_time_wide$TP_26, all_data_dedup_time_wide$TP_27, all_data_dedup_time_wide$TP_28,
                                                 all_data_dedup_time_wide$TP_29, all_data_dedup_time_wide$TP_30, all_data_dedup_time_wide$TP_31, 
                                                 all_data_dedup_time_wide$TP_32, all_data_dedup_time_wide$TP_33, all_data_dedup_time_wide$TP_34,
                                                 all_data_dedup_time_wide$TP_35, all_data_dedup_time_wide$TP_36, all_data_dedup_time_wide$TP_37,
                                                 all_data_dedup_time_wide$TP_38, all_data_dedup_time_wide$TP_39, all_data_dedup_time_wide$TP_40, 
                                                 all_data_dedup_time_wide$TP_41, all_data_dedup_time_wide$TP_42, all_data_dedup_time_wide$TP_43,
                                                 all_data_dedup_time_wide$TP_44, all_data_dedup_time_wide$TP_45, all_data_dedup_time_wide$TP_46,
                                                 all_data_dedup_time_wide$TP_47, all_data_dedup_time_wide$TP_48, all_data_dedup_time_wide$TP_49, 
                                                 all_data_dedup_time_wide$TP_50, all_data_dedup_time_wide$TP_51, all_data_dedup_time_wide$TP_52,
                                                 all_data_dedup_time_wide$TP_53, all_data_dedup_time_wide$TP_54, all_data_dedup_time_wide$TP_55,
                                                 all_data_dedup_time_wide$TP_56, all_data_dedup_time_wide$TP_57, all_data_dedup_time_wide$TP_58,
                                                 all_data_dedup_time_wide$TP_59, all_data_dedup_time_wide$TP_60, all_data_dedup_time_wide$TP_61,
                                                 all_data_dedup_time_wide$TP_62, all_data_dedup_time_wide$TP_63, na.rm = TRUE)

#remove excess columns
all_data_dedup_time_wide[, (cols.to.del) := NULL] #deleting same cols as from max_gentime dataframe
dim(all_data_dedup_time_wide) #check -- should be same as all_data_dedup_gentime_wide dimensions (yes)

#convert days to years (for ease of interpretation)
all_data_dedup_time_wide$max_year_diff <- all_data_dedup_time_wide$max_day_diff/365

#### merge max_gentime & time datasets ####
all_data_dedup_gentime_full <- cbind(all_data_dedup_gentime_wide, max_day_diff = all_data_dedup_time_wide$max_day_diff)
  all_data_dedup_gentime_full$max_year_diff <- all_data_dedup_gentime_full$max_day_diff/365
  
#write out and read in if running separately
#write.csv(all_data_dedup_gentime_full, "Output/all_data_dedup_gentime_full.csv")
#all_data_dedup_gentime_full <- fread(here("Output", "all_data_dedup_gentime_full.csv"))

#reformat longer to put values in the same column
all_data_dedup_gentime_full_longer <- all_data_dedup_gentime_full%>%
  pivot_longer(cols = c(max_gen_diff, max_year_diff),
               names_to = "measure_type",
               values_to = "time_years")

#clean up --> removing Amphipoda & Branchipoda for now because no data for them (?)
all_data_dedup_gentime_full_longer_clean <-all_data_dedup_gentime_full_longer %>%
  filter(tax_group != "Amphipoda") %>%
  filter(tax_group != "Branchiopoda")

#### max_time by taxa ####

#plot tax distribution
maxtottime_by_tax_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean,
               aes(x = tax_group, y = time_years, fill = tax_group, col = measure_type), lwd = 1.5) + 
  scale_color_manual(values = c("#999999", "#000000")) +
  #scale_fill_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
             #                         "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
         #                             "#9467BD", "#C5B0D5", "#C49C94"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150), 
                     sec.axis = sec_axis(~., name= "Time (in Years)")) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean,
             aes(x = tax_group, y = time_years, col = measure_type), 
             position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0.2), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y.left = element_text(size = 25, color = "#999999"),
        axis.text.y.right = element_text(size = 25, color = "#000000"), axis.title.y.left = element_text(size = 25, color = "#999999"),
        axis.title.y.right = element_text(size = 25, color = "#000000"),axis.title.x = element_blank())
maxtottime_by_tax_plot

maxgentime_by_tax_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff", ],
               aes(x = tax_group, y = time_years, color = tax_group), lwd = 1.5) + 
  #scale_fill_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
  #                         "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
  #                             "#9467BD", "#C5B0D5", "#C49C94"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150)) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff", ],
             aes(x = tax_group, y = time_years), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxgentime_by_tax_plot

maxyear_by_tax_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff", ],
               aes(x = tax_group, y = time_years, color = tax_group), lwd = 1.5) + 
  #scale_fill_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
  #                         "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
  #                             "#9467BD", "#C5B0D5", "#C49C94"), 0.6)) +
  scale_y_continuous(name = "Number of Years") +
  geom_point(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff", ],
             aes(x = tax_group, y = time_years), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxyear_by_tax_plot

#### max_time by subject ####

#create dataframe of all maxgen x subject combinations
maxgt_adapt <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" &
                                                                                     all_data_dedup_gentime_full_longer_clean$subject_1 == "adaptation" |
                                                                                     all_data_dedup_gentime_full_longer_clean$subject_2 == "adaptation" | 
                                                                                     all_data_dedup_gentime_full_longer_clean$subject_3 == "adaptation" | 
                                                                                     all_data_dedup_gentime_full_longer_clean$subject_4 == "adaptation"])
  maxgt_adapt$subject <- "adaptation"
  maxgt_adapt$measure_type <- "max_gen_diff"
  colnames(maxgt_adapt) <- c("time_years", "subject", "measure_type")
maxgt_connect <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" &
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_1 == "connectivity" |
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_2 == "connectivity" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_3 == "connectivity" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_4 == "connectivity"])
  maxgt_connect$subject <- "connectivity"
  maxgt_connect$measure_type <- "max_gen_diff"
  colnames(maxgt_connect) <- c("time_years", "subject", "measure_type")
maxgt_div <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" &
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_1 == "diversity" |
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_2 == "diversity" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_3 == "diversity" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_4 == "diversity"])
  maxgt_div$subject <- "diversity"
  maxgt_div$measure_type <- "max_gen_diff"
  colnames(maxgt_div) <- c("time_years", "subject", "measure_type")
maxgt_popsize <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" &
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_1 == "popsize" |
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_2 == "popsize" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_3 == "popsize" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_4 == "popsize"])
  maxgt_popsize$subject <- "popsize"
  maxgt_popsize$measure_type <- "max_gen_diff"
  colnames(maxgt_popsize) <- c("time_years", "subject", "measure_type")
  
#create dataframe of all maxyear x subject combinations
maxyear_adapt <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" &
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_1 == "adaptation" |
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_2 == "adaptation" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_3 == "adaptation" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$subject_4 == "adaptation"])
  maxyear_adapt$subject <- "adaptation"
  maxyear_adapt$measure_type <- "max_year_diff"
  colnames(maxyear_adapt) <- c("time_years", "subject", "measure_type")
maxyear_connect <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" &
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_1 == "connectivity" |
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_2 == "connectivity" | 
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_3 == "connectivity" | 
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_4 == "connectivity"])
  maxyear_connect$subject <- "connectivity"
  maxyear_connect$measure_type <- "max_year_diff"
  colnames(maxyear_connect) <- c("time_years", "subject", "measure_type")
maxyear_div <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" &
                                                                                       all_data_dedup_gentime_full_longer_clean$subject_1 == "diversity" |
                                                                                       all_data_dedup_gentime_full_longer_clean$subject_2 == "diversity" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$subject_3 == "diversity" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$subject_4 == "diversity"])
  maxyear_div$subject <- "diversity"
  maxyear_div$measure_type <- "max_year_diff"
  colnames(maxyear_div) <- c("time_years", "subject", "measure_type")
maxyear_popsize <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" &
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_1 == "popsize" |
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_2 == "popsize" | 
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_3 == "popsize" | 
                                                                                           all_data_dedup_gentime_full_longer_clean$subject_4 == "popsize"])
  maxyear_popsize$subject <- "popsize"
  maxyear_popsize$measure_type <- "max_year_diff"
  colnames(maxyear_popsize) <- c("time_years", "subject", "measure_type")
  
#rbind all data.frames together
maxgen_maxyear_all_subjects <- rbind(maxgt_adapt, maxgt_connect, maxgt_div, maxgt_popsize, 
                                     maxyear_adapt, maxyear_connect, maxyear_div, maxyear_popsize)
  maxgen_maxyear_all_subjects <- na.omit(maxgen_maxyear_all_subjects) #remove nas

#plot maxtottime_by_subject
maxtottime_by_subject_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_subjects,
               aes(x = subject, y = time_years, fill = subject, col = measure_type), lwd = 1.5) + 
  scale_color_manual(values = c("#999999", "#000000")) +
  scale_fill_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150), 
                     sec.axis = sec_axis(~., name= "Time (in Years)")) +
  geom_point(data = maxgen_maxyear_all_subjects,
             aes(x = subject, y = time_years, col = measure_type), 
             position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0.2), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y.left = element_text(size = 25, color = "#999999"),
        axis.text.y.right = element_text(size = 25, color = "#000000"), axis.title.y.left = element_text(size = 25, color = "#999999"),
        axis.title.y.right = element_text(size = 25, color = "#000000"),axis.title.x = element_blank())
maxtottime_by_subject_plot

maxgentime_by_subject_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_subjects[maxgen_maxyear_all_subjects$measure_type == "max_gen_diff", ],
               aes(x = subject, y = time_years, col = subject), lwd = 1.5) + 
  #scale_color_manual(values = c("#999999", "#000000")) +
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150)) +
  geom_point(data = maxgen_maxyear_all_subjects[maxgen_maxyear_all_subjects$measure_type == "max_gen_diff", ],
             aes(x = subject, y = time_years), 
             alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),
        axis.title.x = element_blank())
maxgentime_by_subject_plot

maxyear_by_subject_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_subjects[maxgen_maxyear_all_subjects$measure_type == "max_year_diff", ],
               aes(x = subject, y = time_years, col = subject), lwd = 1.5) + 
  #scale_color_manual(values = c("#999999", "#000000")) +
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Years", limits = c(0, 200)) +
  geom_point(data = maxgen_maxyear_all_subjects[maxgen_maxyear_all_subjects$measure_type == "max_year_diff", ],
             aes(x = subject, y = time_years), 
             alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),
        axis.title.x = element_blank())
maxyear_by_subject_plot

#### max_time by driver ####

#fix missing driver in driver_process1 column
all_data_dedup_gentime_full_longer_clean$driver_process1[all_data_dedup_gentime_full_longer_clean$driver_process1 == ""] <- "no_driver" #bc in this column, if not listed then does not have driver

#create dataframe of all maxgentime x driver combinations
maxgt_climchange <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                        all_data_dedup_gentime_full_longer_clean$driver_process1 == "climate_change" | 
                                                                                        all_data_dedup_gentime_full_longer_clean$driver_process2 == "climate_change" | 
                                                                                        all_data_dedup_gentime_full_longer_clean$driver_process3 == "climate_change"]) 
  maxgt_climchange$driver_process <- "climate_change"
  maxgt_climchange$measure_type <- "max_gen_diff"
  colnames(maxgt_climchange) <- c("time_years", "driver_process", "measure_type")
maxgt_comp <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "competition" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "competition" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "competition"]) 
  maxgt_comp$driver_process <- "competition"
  maxgt_comp$measure_type <- "max_gen_diff"
  colnames(maxgt_comp) <- c("time_years", "driver_process", "measure_type")
maxgt_disease <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "disease" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "disease" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "disease"]) 
  maxgt_disease$driver_process <- "disease"
  maxgt_disease$measure_type <- "max_gen_diff"
  colnames(maxgt_disease) <- c("time_years", "driver_process", "measure_type")
maxgt_envvar <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "env_variation" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "env_variation" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "env_variation"]) 
  maxgt_envvar$driver_process <- "env_variation"
  maxgt_envvar$measure_type <- "max_gen_diff"
  colnames(maxgt_envvar) <- c("time_years", "driver_process", "measure_type")
maxgt_habloss <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "habitat_loss" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "habitat_loss" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "habitat_loss"]) 
  maxgt_habloss$driver_process <- "habitat_loss"
  maxgt_habloss$measure_type <- "max_gen_diff"
  colnames(maxgt_habloss) <- c("time_years", "driver_process", "measure_type")
maxgt_humexploit<- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "human_exploitation" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "human_exploitation" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "human_exploitation"]) 
  maxgt_humexploit$driver_process <- "human_exploitation"
  maxgt_humexploit$measure_type <- "max_gen_diff"
  colnames(maxgt_humexploit) <- c("time_years", "driver_process", "measure_type")
maxgt_invspecies <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "invasive_species" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "invasive_species" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "invasive_species"]) 
  maxgt_invspecies$driver_process <- "invasive_species"
  maxgt_invspecies$measure_type <- "max_gen_diff"
  colnames(maxgt_invspecies) <- c("time_years", "driver_process", "measure_type")
maxgt_natdis <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "natural_disaster" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "natural_disaster" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "natural_disaster"]) 
  maxgt_natdis$driver_process <- "natural_disaster"
  maxgt_natdis$measure_type <- "max_gen_diff"
  colnames(maxgt_natdis) <- c("time_years", "driver_process", "measure_type")
maxgt_nodriver <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "no_driver"]) 
  maxgt_nodriver$driver_process <- "no_driver"
  maxgt_nodriver$measure_type <- "max_gen_diff"
  colnames(maxgt_nodriver) <- c("time_years", "driver_process", "measure_type")
  
#create dataframe of all maxyear x driver combinations
maxyear_climchange <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "climate_change" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "climate_change" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "climate_change"]) 
  maxyear_climchange$driver_process <- "climate_change"
  maxyear_climchange$measure_type <- "max_year_diff"
  colnames(maxyear_climchange) <- c("time_years", "driver_process", "measure_type")
maxyear_comp <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                    all_data_dedup_gentime_full_longer_clean$driver_process1 == "competition" | 
                                                                                    all_data_dedup_gentime_full_longer_clean$driver_process2 == "competition" | 
                                                                                    all_data_dedup_gentime_full_longer_clean$driver_process3 == "competition"]) 
  maxyear_comp$driver_process <- "competition"
  maxyear_comp$measure_type <- "max_year_diff"
  colnames(maxyear_comp) <- c("time_years", "driver_process", "measure_type")
maxyear_disease <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process1 == "disease" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process2 == "disease" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process3 == "disease"]) 
  maxyear_disease$driver_process <- "disease"
  maxyear_disease$measure_type <- "max_year_diff"
  colnames(maxyear_disease) <- c("time_years", "driver_process", "measure_type")
maxyear_envvar <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process1 == "env_variation" | 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process2 == "env_variation" | 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process3 == "env_variation"]) 
  maxyear_envvar$driver_process <- "env_variation"
  maxyear_envvar$measure_type <- "max_year_diff"
  colnames(maxyear_envvar) <- c("time_years", "driver_process", "measure_type")
maxyear_habloss <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process1 == "habitat_loss" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process2 == "habitat_loss" | 
                                                                                       all_data_dedup_gentime_full_longer_clean$driver_process3 == "habitat_loss"]) 
  maxyear_habloss$driver_process <- "habitat_loss"
  maxyear_habloss$measure_type <- "max_year_diff"
  colnames(maxyear_habloss) <- c("time_years", "driver_process", "measure_type")
maxyear_humexploit<- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                         all_data_dedup_gentime_full_longer_clean$driver_process1 == "human_exploitation" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$driver_process2 == "human_exploitation" | 
                                                                                         all_data_dedup_gentime_full_longer_clean$driver_process3 == "human_exploitation"]) 
  maxyear_humexploit$driver_process <- "human_exploitation"
  maxyear_humexploit$measure_type <- "max_year_diff"
  colnames(maxyear_humexploit) <- c("time_years", "driver_process", "measure_type")
maxyear_invspecies <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process1 == "invasive_species" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process2 == "invasive_species" | 
                                                                                          all_data_dedup_gentime_full_longer_clean$driver_process3 == "invasive_species"]) 
  maxyear_invspecies$driver_process <- "invasive_species"
  maxyear_invspecies$measure_type <- "max_year_diff"
  colnames(maxyear_invspecies) <- c("time_years", "driver_process", "measure_type")
maxyear_natdis <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process1 == "natural_disaster" | 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process2 == "natural_disaster" | 
                                                                                      all_data_dedup_gentime_full_longer_clean$driver_process3 == "natural_disaster"]) 
  maxyear_natdis$driver_process <- "natural_disaster"
  maxyear_natdis$measure_type <- "max_year_diff"
  colnames(maxyear_natdis) <- c("time_years", "driver_process", "measure_type")
maxyear_nodriver <- as.data.frame(all_data_dedup_gentime_full_longer_clean$time_years[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff" & 
                                                                                        all_data_dedup_gentime_full_longer_clean$driver_process1 == "no_driver"]) 
  maxyear_nodriver$driver_process <- "no_driver"
  maxyear_nodriver$measure_type <- "max_year_diff"
  colnames(maxyear_nodriver) <- c("time_years", "driver_process", "measure_type")

#rbind all data.frames together
maxgen_maxyear_all_drivers <- rbind(maxgt_climchange, maxgt_comp, maxgt_disease, maxgt_envvar, 
                                    maxgt_habloss, maxgt_humexploit, maxgt_invspecies, maxgt_natdis, 
                                    maxgt_nodriver, maxyear_climchange, maxyear_comp, maxyear_disease, 
                                    maxyear_envvar, maxyear_habloss, maxyear_humexploit, maxyear_invspecies, 
                                    maxyear_natdis, maxyear_nodriver)
maxgen_maxyear_all_drivers <- na.omit(maxgen_maxyear_all_drivers) #remove nas

#plot maxtottime_by_driver
maxtottime_by_driver_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_drivers,
               aes(x = driver_process, y = time_years, fill = driver_process, col = measure_type), lwd = 1.5) + 
  scale_color_manual(values = c("#999999", "#000000")) +
  scale_fill_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
                                      "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
                                      "#9467BD"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150), 
                     sec.axis = sec_axis(~., name= "Time (in Years)")) +
  geom_point(data = maxgen_maxyear_all_drivers,
             aes(x = driver_process, y = time_years, col = measure_type), 
             position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0.2), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y.left = element_text(size = 25, color = "#999999"),
        axis.text.y.right = element_text(size = 25, color = "#000000"), axis.title.y.left = element_text(size = 25, color = "#999999"),
        axis.title.y.right = element_text(size = 25, color = "#000000"),axis.title.x = element_blank())
maxtottime_by_driver_plot

maxgentime_by_driver_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_drivers[maxgen_maxyear_all_drivers$measure_type == "max_gen_diff", ],
               aes(x = driver_process, y = time_years, col = driver_process), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
                                      "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
                                      "#9467BD"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150)) +
  geom_point(data = maxgen_maxyear_all_drivers[maxgen_maxyear_all_drivers$measure_type == "max_gen_diff", ],
             aes(x = driver_process, y = time_years, col = driver_process), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxgentime_by_driver_plot

maxyear_by_driver_plot <- ggplot() +
  geom_boxplot(data = maxgen_maxyear_all_drivers[maxgen_maxyear_all_drivers$measure_type == "max_year_diff", ],
               aes(x = driver_process, y = time_years, col = driver_process), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", 
                                       "#2CA02C","#98DF8A", "#D62728", "#FF9896", 
                                       "#9467BD"), 0.6)) +
  scale_y_continuous(name = "Number of Years", limits = c(0, 200)) +
  geom_point(data = maxgen_maxyear_all_drivers[maxgen_maxyear_all_drivers$measure_type == "max_year_diff", ],
             aes(x = driver_process, y = time_years, col = driver_process), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxyear_by_driver_plot

#### max_time by study_design ####

#subset to only studies that have a study design
all_data_dedup_gentime_full_longer_clean_nonasd <- all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$study_design != "",]

#plot maxtottime_by_sd
maxtottime_by_sd_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean_nonasd,
               aes(x = study_design, y = time_years, fill = study_design, col = measure_type), lwd = 1.5) + 
  scale_color_manual(values = c("#999999", "#000000")) +
  scale_fill_manual(values = alpha( c("#1F77B4", "#FF7F0E"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150), 
                     sec.axis = sec_axis(~., name= "Time (in Years)")) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean_nonasd,
             aes(x = study_design, y = time_years, col = measure_type), 
             position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0.2), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y.left = element_text(size = 25, color = "#999999"),
        axis.text.y.right = element_text(size = 25, color = "#000000"), axis.title.y.left = element_text(size = 25, color = "#999999"),
        axis.title.y.right = element_text(size = 25, color = "#000000"),axis.title.x = element_blank())
maxtottime_by_sd_plot

maxgentime_by_sd_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean_nonasd[all_data_dedup_gentime_full_longer_clean_nonasd$measure_type == "max_gen_diff", ],
               aes(x = study_design, y = time_years, col = study_design), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150)) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean_nonasd[all_data_dedup_gentime_full_longer_clean_nonasd$measure_type == "max_gen_diff", ],
             aes(x = study_design, y = time_years, col = study_design), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"), axis.title.x = element_blank())
maxgentime_by_sd_plot

maxyear_by_sd_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean_nonasd[all_data_dedup_gentime_full_longer_clean_nonasd$measure_type == "max_year_diff", ],
               aes(x = study_design, y = time_years, col = study_design), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E"), 0.6)) +
  scale_y_continuous(name = "Number of Years", limits = c(0, 200)) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean_nonasd[all_data_dedup_gentime_full_longer_clean_nonasd$measure_type == "max_year_diff", ],
             aes(x = study_design, y = time_years, col = study_design), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"), axis.title.x = element_blank())
maxyear_by_sd_plot

#### max_time by system ####

#plot maxgt_by_system
maxtottime_by_system_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean,
               aes(x = system, y = time_years, fill = system, col = measure_type), lwd = 1.5) + 
  scale_color_manual(values = c("#999999", "#000000")) +
  scale_fill_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150), 
                     sec.axis = sec_axis(~., name= "Time (in Years)")) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean,
             aes(x = system, y = time_years, col = measure_type), 
             position = position_jitterdodge(jitter.height = 0.2, jitter.width = 0.2), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1), axis.text.y.left = element_text(size = 25, color = "#999999"),
        axis.text.y.right = element_text(size = 25, color = "#000000"), axis.title.y.left = element_text(size = 25, color = "#999999"),
        axis.title.y.right = element_text(size = 25, color = "#000000"),axis.title.x = element_blank())
maxtottime_by_system_plot

maxgentime_by_system_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff", ], 
               aes(x = system, y = time_years, col = system), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Generations", limits = c(0, 150)) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_gen_diff", ],
             aes(x = system, y = time_years, col = system), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxgentime_by_system_plot

maxyear_by_system_plot <- ggplot() +
  geom_boxplot(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff", ], 
               aes(x = system, y = time_years, col = system), lwd = 1.5) + 
  scale_color_manual(values = alpha( c("#1F77B4", "#FF7F0E", "#2CA02C", "#C5B0D5"), 0.6)) +
  scale_y_continuous(name = "Number of Years", limits = c(0, 200)) +
  geom_point(data = all_data_dedup_gentime_full_longer_clean[all_data_dedup_gentime_full_longer_clean$measure_type == "max_year_diff", ],
             aes(x = system, y = time_years, col = system), alpha = 0.8) +
  scale_pattern_fill_manual(values = c("#000000", NA)) +
  theme(panel.grid.major = element_blank(), panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA), legend.position = "none",
        axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title.y = element_text(size = 25, color = "black"),axis.title.x = element_blank())
maxyear_by_system_plot

################################################################################################

######## Driver data exploration ########
#Using deduplicated since interested in driver(s) of publication -- don't want to inflate with multiple studies in a publication

#fix missing driver in driver_process1 column
all_data_deduplicate$driver_process1[all_data_deduplicate$driver_process1 == ""] <- "no_driver" #bc in this column, if not listed then does not have driver

#### driver distribution ####

#sum # studies w/various drivers
num_drivers1 <- all_data_deduplicate[, .N, by = .(driver_process1)]
  num_drivers1 <- num_drivers1[order(num_drivers1$driver_process1),]
  colnames(num_drivers1) <- c("driver_process", "N1")
num_drivers2 <- all_data_deduplicate[, .N, by = .(driver_process2)]
  colnames(num_drivers2) <- c("driver_process", "N2")
num_drivers3 <- all_data_deduplicate[, .N, by = .(driver_process3)]
  colnames(num_drivers3) <- c("driver_process", "N3")

#merge num_drivers data.tables
num_drivers_list <- list(num_drivers1, num_drivers2, num_drivers3)
num_drivers <- num_drivers_list %>% reduce(full_join, by = "driver_process", all = TRUE)
  num_drivers <- num_drivers[num_drivers$N1 != "NA", ] #remove extra rows where driver_process 2 & 3 only are missing
  
#sum across columns to get total N drivers
num_drivers$Ntot <- rowSums(num_drivers[, c("N1", "N2", "N3")], na.rm = TRUE)

#plot driver_dist
driver_plot <- ggplot(data = num_drivers, aes(x = reorder(driver_process, -Ntot), y = Ntot, fill = driver_process)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal() + xlab("driver_process") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_plot

#plot driver1_dist --> first driver ONLY
driver1_plot <- ggplot(data = na.omit(all_data_deduplicate[, .N, by = .(driver_process1)]), aes(x = reorder(driver_process1, -N), y = N, fill = driver_process1)) + 
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal() + xlab("driver_process1") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver1_plot

#### driver by subject ####

#count # driver occurrences by subject
driver1_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, driver_process1)]
  colnames(driver1_by_subject1) <- c("subject", "driver_process", "N11") #N11: driver1, subject1
driver1_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, driver_process1)]
  colnames(driver1_by_subject2) <- c("subject", "driver_process", "N12")
driver1_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, driver_process1)]
  colnames(driver1_by_subject3) <- c("subject", "driver_process", "N13")
driver1_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, driver_process1)]
  colnames(driver1_by_subject4) <- c("subject", "driver_process", "N14")
driver2_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, driver_process2)]
  colnames(driver2_by_subject1) <- c("subject", "driver_process", "N21")
driver2_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, driver_process2)]
  colnames(driver2_by_subject2) <- c("subject", "driver_process", "N22")
driver2_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, driver_process2)]
  colnames(driver2_by_subject3) <- c("subject", "driver_process", "N23")
driver2_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, driver_process2)]
  colnames(driver2_by_subject4) <- c("subject", "driver_process", "N24")
driver3_by_subject1 <- all_data_deduplicate[, .N, by = .(subject_1, driver_process3)]
  colnames(driver3_by_subject1) <- c("subject", "driver_process", "N31")
driver3_by_subject2 <- all_data_deduplicate[, .N, by = .(subject_2, driver_process3)]
  colnames(driver3_by_subject2) <- c("subject", "driver_process", "N32")
driver3_by_subject3 <- all_data_deduplicate[, .N, by = .(subject_3, driver_process3)]
  colnames(driver3_by_subject3) <- c("subject", "driver_process", "N33")
driver3_by_subject4 <- all_data_deduplicate[, .N, by = .(subject_4, driver_process3)]
  colnames(driver3_by_subject4) <- c("subject", "driver_process", "N34")

#merge driver_by_subject data.tables
driver_by_subject_list <- list(driver1_by_subject1, driver1_by_subject2, driver1_by_subject3, driver1_by_subject4, 
                               driver2_by_subject1, driver2_by_subject2, driver2_by_subject3, driver2_by_subject4, 
                               driver3_by_subject1, driver3_by_subject2, driver3_by_subject3, driver3_by_subject4)
driver_by_subject <- driver_by_subject_list %>% reduce(full_join, by = c("subject", "driver_process"), all = TRUE)

#sum across columns to get total N driver_by_subject
driver_by_subject$Ntot <- rowSums(driver_by_subject[, c("N11", "N12", "N13", "N14", 
                                                        "N21", "N22", "N23", "N24", 
                                                        "N31", "N32", "N33", "N34")], na.rm = TRUE)
  driver_by_subject <- driver_by_subject[driver_by_subject$subject != "", ] #remove rows where subject missing
  #driver_by_subject <- driver_by_subject[driver_by_subject$driver_process != "", ] #remove rows where driver_process missing
  driver_by_subject <- driver_by_subject[1:39, ] #remove rows that weren't caught in previous two lines
    driver_by_subject <- driver_by_subject[-33, ] #WILL NEED TO CHECK THIS EACH TIME (WANT NAs LEFT)
  
#plot driver_by_subject
driver_by_subject_plot <- ggplot(data = driver_by_subject, aes(x = subject, y = Ntot, fill = driver_process)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
       # axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_subject_plot

#### driver by study design ####

#count # driver occurrences by study design
driver1_by_sd <- all_data_deduplicate[, .N, by = .(study_design, driver_process1)]
  colnames(driver1_by_sd) <- c("study_design", "driver_process", "N1")
driver2_by_sd <- all_data_deduplicate[, .N, by = .(study_design, driver_process2)]
  colnames(driver2_by_sd) <- c("study_design", "driver_process", "N2")
driver3_by_sd <- all_data_deduplicate[, .N, by = .(study_design, driver_process3)]
  colnames(driver3_by_sd) <- c("study_design", "driver_process", "N3")
  
#merge driver_by_sd data.tables
driver_by_sd_list <- list(driver1_by_sd, driver2_by_sd, driver3_by_sd)
driver_by_sd <- driver_by_sd_list %>% reduce(full_join, by = c("study_design", "driver_process"), all = TRUE)

#sum across columns to get total N driver_by_sd
driver_by_sd$Ntot <- rowSums(driver_by_sd[, c("N1", "N2", "N3")], na.rm = TRUE)
  driver_by_sd <- driver_by_sd[driver_by_sd$study_design != "", ] #remove rows where study_design missing
  driver_by_sd <- driver_by_sd[driver_by_sd$N1 != "NA", ] #remove rows where driver_process missing

#plot driver_by_sd
driver_by_sd_plot <- ggplot(data = driver_by_sd, aes(x = study_design, y = Ntot, fill = driver_process)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        # axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_sd_plot 

#### driver by year ####

#count # driver occurrences by year
driver1_by_year <- all_data_deduplicate[, .N, by = .(Publication_Year, driver_process1)]
  colnames(driver1_by_year) <- c("Publication_Year", "driver_process", "N1")
driver2_by_year <- all_data_deduplicate[, .N, by = .(Publication_Year, driver_process2)]
  colnames(driver2_by_year) <- c("Publication_Year", "driver_process", "N2")
driver3_by_year <- all_data_deduplicate[, .N, by = .(Publication_Year, driver_process3)]
  colnames(driver3_by_year) <- c("Publication_Year", "driver_process", "N3")

#merge driver_by_year data.tables
driver_by_year_list <- list(driver1_by_year, driver2_by_year, driver3_by_year)
driver_by_year <- driver_by_year_list %>% reduce(full_join, by = c("Publication_Year", "driver_process"), all = TRUE)

#sum across columns to get total N driver_by_year
driver_by_year$Ntot <- rowSums(driver_by_year[, c("N1", "N2", "N3")], na.rm = TRUE)
  driver_by_year <- driver_by_year[driver_by_year$driver_process != "", ] #remove rows where driver_process missing
  driver_by_year <- driver_by_year[-97, ] #remove row where driver_process still missing

#plot driver_by_year
driver_by_year_plot <- ggplot(data = driver_by_year, aes(x = Publication_Year, y = Ntot, group = driver_process)) + 
  geom_smooth(aes(color = driver_process), size = 4, se = FALSE) + 
  #geom_line(aes(color = driver_process), size = 4) + 
  #geom_point(aes(color = driver_process), size = 2) + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        # axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_year_plot

#### driver by system ####

#count # driver occurrences by system
driver1_by_system <- all_data_deduplicate[, .N, by = .(system, driver_process1)]
  colnames(driver1_by_system) <- c("system", "driver_process", "N1")
driver2_by_system <- all_data_deduplicate[, .N, by = .(system, driver_process2)]
  colnames(driver2_by_system) <- c("system", "driver_process", "N2")
driver3_by_system <- all_data_deduplicate[, .N, by = .(system, driver_process3)]
  colnames(driver3_by_system) <- c("system", "driver_process", "N3")

#merge driver_by_system data.tables
driver_by_system_list <- list(driver1_by_system, driver2_by_system, driver3_by_system)
driver_by_system <- driver_by_system_list %>% reduce(full_join, by = c("system", "driver_process"), all = TRUE)

#sum across columns to get total N driver_by_system
driver_by_system$Ntot <- rowSums(driver_by_system[, c("N1", "N2", "N3")], na.rm = TRUE)
#driver_by_system <- driver_by_system[driver_by_system$driver_process != "", ] #remove rows where driver_process missing
driver_by_system <- driver_by_system[-29, ] #remove row where driver_process still missing, CHECK EACH TIME (WANT NAs)

#plot driver_by_system
driver_by_system_plot <- ggplot(data = driver_by_system, aes(x = system, y = Ntot, fill = driver_process)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        # axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_system_plot

#### driver by taxa ####

#count # driver occurrences by taxa
driver1_by_taxa <- all_data_deduplicate[, .N, by = .(tax_group, driver_process1)]
  colnames(driver1_by_taxa) <- c("tax_group", "driver_process", "N1")
driver2_by_taxa <- all_data_deduplicate[, .N, by = .(tax_group, driver_process2)]
  colnames(driver2_by_taxa) <- c("tax_group", "driver_process", "N2")
driver3_by_taxa <- all_data_deduplicate[, .N, by = .(tax_group, driver_process3)]
  colnames(driver3_by_taxa) <- c("tax_group", "driver_process", "N3")

#merge driver_by_taxa data.tables
driver_by_taxa_list <- list(driver1_by_taxa, driver2_by_taxa, driver3_by_taxa)
driver_by_taxa <- driver_by_taxa_list %>% reduce(full_join, by = c("tax_group", "driver_process"), all = TRUE)

#sum across columns to get total N driver_by_taxa
driver_by_taxa$Ntot <- rowSums(driver_by_taxa[, c("N1", "N2", "N3")], na.rm = TRUE)
  driver_by_taxa <- driver_by_taxa[driver_by_taxa$N1 != "", ] #remove rows where driver_process missing

#plot driver_by_taxa
driver_by_taxa_plot <- ggplot(data = driver_by_taxa, aes(x = reorder(tax_group, -Ntot), y = Ntot, fill = driver_process)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + xlab("taxonomic group (class)") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_taxa_plot 

##########################################################################################################################################

######## Library prep data exploration ########
#NOT "usual" deduplicated bc different studies use different markers (and different lib prep techniques)

#removing rows if have same Article_Number (same publication) and same lib prep
all_data_dedup_libprep <- distinct(all_data, Article_Number, lib_prep_method, .keep_all = TRUE)

#### lib_prep by subject ####

#count # marker occurrences by subject
libprep_by_subject1 <- all_data_dedup_libprep[, .N, by = .(subject_1, lib_prep_method)]
  colnames(libprep_by_subject1) <- c("subject", "lib_prep_method", "N1")
libprep_by_subject2 <- all_data_dedup_libprep[, .N, by = .(subject_2, lib_prep_method)]
  libprep_by_subject2 <- subset(libprep_by_subject2, libprep_by_subject2$subject_2 != "")
  colnames(libprep_by_subject2) <- c("subject", "lib_prep_method", "N2")
libprep_by_subject3 <- all_data_dedup_libprep[, .N, by = .(subject_3, lib_prep_method)]
  libprep_by_subject3 <- subset(libprep_by_subject3, libprep_by_subject3$subject_3 != "")
  colnames(libprep_by_subject3) <- c("subject", "lib_prep_method", "N3")
libprep_by_subject4 <- all_data_dedup_libprep[, .N, by = .(subject_4, lib_prep_method)]
  libprep_by_subject4 <- subset(libprep_by_subject4, libprep_by_subject4$subject_4 != "")
  colnames(libprep_by_subject4) <- c("subject", "lib_prep_method", "N4")
  libprep_by_subject4 <- libprep_by_subject4[-6 ,] #wouldn't delete 5th row with missing data for some reason, doing manually

#merge libprep_by_subject data.tables
libprep_by_subject_list <- list(libprep_by_subject1, libprep_by_subject2, libprep_by_subject3, libprep_by_subject4)
libprep_by_subject <- libprep_by_subject_list %>% reduce(full_join, by = c("subject", "lib_prep_method"), all = TRUE)

#sum across columns to get total N libprep_by_subject
libprep_by_subject$Ntot <- rowSums(libprep_by_subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)
  libprep_by_subject <- subset(libprep_by_subject, libprep_by_subject$lib_prep_method != "")

#plot libprep_by_subject
libprep_by_subject_plot <- ggplot(data = libprep_by_subject, aes(x = subject, y = Ntot, fill = lib_prep_method)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
libprep_by_subject_plot

#### libprep by year ####
libprep_by_year <- all_data_dedup_libprep[, .N, by = .(lib_prep_method, Publication_Year)]
libprep_by_year <- libprep_by_year[lib_prep_method == "", lib_prep_method := NA] #change rows with blank lib_prep_method to NA

#plot libprep_by_year
#HRM_assay, RNA, Whole_genome don't show bc just a point
libprep_by_year_plot <- ggplot(data = na.omit(libprep_by_year), aes(x = Publication_Year, y = N, group = lib_prep_method)) + 
  geom_line(aes(color = lib_prep_method), size = 4, se = FALSE) + 
  #geom_point(aes(color = lib_prep_method), size = 2) + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
libprep_by_year_plot

################################################################################################

#### TO DO
#incorporate # of samples and sample size ranges into analyses
#geographic trends -- count up countries and plot, see if there is an interaction with type/driver of change, taxa, system, etc.
#incorporate preservation methods/tissue type/seq_platform trends through time