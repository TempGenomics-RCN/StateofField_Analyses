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

#read in data
all_data <- fread(here("Output", "all_tempgen_data.csv"))

#create non-duplicated dataset
#some studies have multiple rows (bc looked at more than one species, marker type, etc)
#don't want to double count these
all_data_deduplicate <- all_data[!duplicated(all_data$Article_Number), ]

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
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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
  geom_bar(position = "stack", stat = "identity", color = "black") + 
  theme_minimal() + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "top", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
sd_by_subject_plot

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
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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

######## Max generation time data exploration ########
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
all_data_dedup_gentime_long[, day_diff := abs(as.Date(all_data_dedup_gentime_long$TP_1, format = "%Y.%m.%d") - 
                                  as.Date(all_data_dedup_gentime_long$date, format = "%Y.%m.%d"))]

#convert to # days btwn time points to # generations
all_data_dedup_gentime_long[, gen_diff := all_data_dedup_gentime_long$day_diff/all_data_dedup_gentime_long$gen_time]

#remove day_diff & date columns (will otherwise screw up conversion back to wide format)
all_data_dedup_gentime_long[, date := NULL]
all_data_dedup_gentime_long[, day_diff := NULL]

#convert back to wide format
#now, number of generations from TP_1 is in TP_2, etc. columns
all_data_dedup_gentime_wide <- as.data.table(pivot_wider(data = all_data_dedup_gentime_long,
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
dim(all_data_dedup_gentime_wide) #check

#### max_gentime by taxa ####

#plot maxgentime_by_tax
maxgt_by_tax_plot <- ggplot(data = all_data_dedup_gentime_wide, aes(x = tax_group, y = max_gen_diff, color = tax_group)) + 
  geom_boxplot(size = 2) + 
  ylim(c(0, 100)) + #cutting off some groups but there are some tax (Branchiopoda) with large gen diff
  theme_minimal() + ylab("max_num_gen") + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
maxgt_by_tax_plot

#### max_gentime by subject ####

#create dataframe of all maxgentime x subject combinations
maxgt_adapt <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$subject_1 == "adaptation" | 
                                                                        all_data_dedup_gentime$subject_2 == "adaptation" | 
                                                                        all_data_dedup_gentime$subject_3 == "adaptation" | 
                                                                        all_data_dedup_gentime$subject_4 == "adaptation"])
  maxgt_adapt$subject <- "adaptation"
  colnames(maxgt_adapt) <- c("max_gen_diff", "subject")
maxgt_connect <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$subject_1 == "connectivity" | 
                                                                          all_data_dedup_gentime$subject_2 == "connectivity" | 
                                                                          all_data_dedup_gentime$subject_3 == "connectivity" | 
                                                                          all_data_dedup_gentime$subject_4 == "connectivity"])
  maxgt_connect$subject <- "connectivity"
  colnames(maxgt_connect) <- c("max_gen_diff", "subject")
maxgt_div <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$subject_1 == "diversity" | 
                                                                      all_data_dedup_gentime$subject_2 == "diversity" | 
                                                                      all_data_dedup_gentime$subject_3 == "diversity" | 
                                                                      all_data_dedup_gentime$subject_4 == "diversity"])
  maxgt_div$subject <- "diversity"
  colnames(maxgt_div) <- c("max_gen_diff", "subject")
maxgt_popsize <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$subject_1 == "popsize" | 
                                                                          all_data_dedup_gentime$subject_2 == "popsize" | 
                                                                          all_data_dedup_gentime$subject_3 == "popsize" | 
                                                                          all_data_dedup_gentime$subject_4 == "popsize"])
  maxgt_popsize$subject <- "popsize"
  colnames(maxgt_popsize) <- c("max_gen_diff", "subject")
  
#rbind all data.frames together
maxgt_all_subjects <- rbind(maxgt_adapt, maxgt_connect, maxgt_div, maxgt_popsize)

#plot maxgt_by_subject 
maxgt_by_subject_plot <- ggplot(data = na.omit(maxgt_all_subjects), aes(x = subject, y = max_gen_diff, color = subject)) + 
  geom_boxplot(size = 2) +
  #ylim(c(0, 100)) +
  theme_minimal() + ylab("max_num_gen") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
maxgt_by_subject_plot

#### max_gentime by driver ####

#create dataframe of all maxgentime x driver combinations
maxgt_climchange <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "climate_change" | 
                                                                             all_data_dedup_gentime$driver_process2 == "climate_change" | 
                                                                             all_data_dedup_gentime$driver_process3 == "climate_change"]) 
  maxgt_climchange$driver_process <- "climate_change"
  colnames(maxgt_climchange) <- c("max_gen_diff", "driver_process")
maxgt_comp <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "competition" | 
                                                                       all_data_dedup_gentime$driver_process2 == "competition" | 
                                                                       all_data_dedup_gentime$driver_process3 == "competition"]) 
  maxgt_comp$driver_process <- "competition"
  colnames(maxgt_comp) <- c("max_gen_diff", "driver_process")
maxgt_disease <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "disease" | 
                                                                          all_data_dedup_gentime$driver_process2 == "disease" | 
                                                                          all_data_dedup_gentime$driver_process3 == "disease"]) 
  maxgt_disease$driver_process <- "disease"
  colnames(maxgt_disease) <- c("max_gen_diff", "driver_process")
maxgt_envvar <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "env_variation" | 
                                                                         all_data_dedup_gentime$driver_process2 == "env_variation" | 
                                                                         all_data_dedup_gentime$driver_process3 == "env_variation"]) 
  maxgt_envvar$driver_process <- "env_variation"
  colnames(maxgt_envvar) <- c("max_gen_diff", "driver_process")
maxgt_habloss <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "habitat_loss" | 
                                                                          all_data_dedup_gentime$driver_process2 == "habitat_loss" | 
                                                                          all_data_dedup_gentime$driver_process3 == "habitat_loss"]) 
  maxgt_habloss$driver_process <- "habitat_loss"
  colnames(maxgt_habloss) <- c("max_gen_diff", "driver_process")
maxgt_humexploit <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "human_exploitation" | 
                                                                             all_data_dedup_gentime$driver_process2 == "human_exploitation" | 
                                                                             all_data_dedup_gentime$driver_process3 == "human_exploitation"]) 
  maxgt_humexploit$driver_process <- "human_exploitation"
  colnames(maxgt_humexploit) <- c("max_gen_diff", "driver_process")
maxgt_invspecies <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "invasive_species" | 
                                                                             all_data_dedup_gentime$driver_process2 == "invasive_species" | 
                                                                             all_data_dedup_gentime$driver_process3 == "invasive_species"]) 
  maxgt_invspecies$driver_process <- "invasive_species"
  colnames(maxgt_invspecies) <- c("max_gen_diff", "driver_process")
maxgt_natdisast <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == "natural_disaster" | 
                                                                            all_data_dedup_gentime$driver_process2 == "natural_disaster" | 
                                                                            all_data_dedup_gentime$driver_process3 == "natural_disaster"]) 
  maxgt_natdisast$driver_process <- "natural_disaster"
  colnames(maxgt_natdisast) <- c("max_gen_diff", "driver_process")
maxgt_nodriver <- as.data.frame(all_data_dedup_gentime_wide$max_gen_diff[all_data_dedup_gentime$driver_process1 == ""]) #only in first driver column bc if missing in subsequent ones, then only have 1 driver
  maxgt_nodriver$driver_process <- "no_driver"
  colnames(maxgt_nodriver) <- c("max_gen_diff", "driver_process")
  
#rbind all data.frames together
maxgt_all_drivers <- rbind(maxgt_climchange, maxgt_comp, maxgt_disease, maxgt_envvar, 
                           maxgt_habloss, maxgt_humexploit, maxgt_invspecies, 
                           maxgt_natdisast, maxgt_nodriver)
  
#plot maxgt_by_driver
maxgt_by_driver_plot <- ggplot(data = na.omit(maxgt_all_drivers), aes(x = driver_process, y = max_gen_diff, color = driver_process)) + 
  geom_boxplot(size = 2) +
  ylim(c(0, 100)) +
  theme_minimal() + ylab("max_num_gen") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
maxgt_by_driver_plot

#### max_gentime by study_design ####

#subset to only studies that have a study design
all_data_dedup_gentime_wide_nonasd <- all_data_dedup_gentime_wide[study_design != "",]

#plot maxgt_by_sd
maxgt_by_sd_plot <- ggplot(data = all_data_dedup_gentime_wide_nonasd, aes(x = study_design, y = max_gen_diff, color = study_design)) + 
  geom_boxplot(size = 2) +
  #ylim(c(0, 100)) +
  theme_minimal() + ylab("max_num_gen") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
maxgt_by_sd_plot

#### max_gentime by system ####

#plot maxgt_by_system
maxgt_by_system_plot <- ggplot(data = all_data_dedup_gentime_wide, aes(x = system, y = max_gen_diff, color = system)) + 
  geom_boxplot(size = 2) +
  ylim(c(0, 100)) +
  theme_minimal() + ylab("max_num_gen") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
maxgt_by_system_plot

################################################################################################

######## Driver data exploration ########
#using deduplicated since interested in driver(s) of publication -- don't want to inflate with multiple studies in a publication

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
  driver_by_subject <- driver_by_subject[driver_by_subject$driver_process != "", ] #remove rows where driver_process missing
  driver_by_subject <- driver_by_subject[1:32, ] #remove rows that weren't caught in previous two lines

#plot driver_by_subject
driver_by_subject_plot <- ggplot(data = driver_by_subject, aes(x = subject, y = Ntot, fill = driver_process)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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
  geom_bar(position = "fill", stat = "identity", color = "black") + 
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
driver_by_system <- driver_by_system[driver_by_system$driver_process != "", ] #remove rows where driver_process missing
driver_by_system <- driver_by_system[-30, ] #remove row where driver_process still missing

#plot driver_by_system
driver_by_system_plot <- ggplot(data = driver_by_system, aes(x = system, y = Ntot, fill = driver_process)) + 
  geom_bar(position = "stack", stat = "identity", color = "black") + 
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
  driver_by_taxa <- driver_by_taxa[driver_by_taxa$driver_process != "", ] #remove rows where driver_process missing
  driver_by_taxa <- driver_by_taxa[-58, ] #remove row where driver_process still missing

#plot driver_by_taxa
driver_by_taxa_plot <- ggplot(data = driver_by_taxa, aes(x = reorder(tax_group, -Ntot), y = Ntot, fill = driver_process)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") + 
  theme_minimal() + xlab("taxonomic group (class)") +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 22), legend.position = "right", 
        axis.text.x = element_text(angle = 315),
        legend.title = element_text(size = 22), legend.text = element_text(size = 22))
driver_by_taxa_plot 

#### TO DO
#incorporate # of samples and sample size ranges into analyses
#look at total range of time (not just generations) as well --> AND average time/# generations between samples (does this differ by study design?)
#geographic trends -- count up countries and plot, see if there is an interaction with type/driver of change, taxa, system, etc.
#incorporate preservation methods/tissue type/lib prep/seq_platform trends through time