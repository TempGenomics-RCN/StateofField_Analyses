################################################### Script for Manuscript Figures #######################################################

#scripts for manuscript figures specifically
#starts from cleaned & aggregated lit review data (created by "assemble_data_SoF.R" script)

##########################################################################################################################################

######## Set-up ########

remove(list = ls())
getwd() #check working directory

#load libraries
library(here) #v.1.0.1
library(data.table) #v.1.14.8
library(tidyverse) #v.2.0.0
library(ggpattern) #v.1.0.1
library(gridExtra) #v.2.3
library(cowplot) #v.1.1.1
library(ggthemes) #v.4.2.4

#read in data
all_data <- fread(here("Output", "all_tempgen_data.csv"))
country_samp <- fread(here("Output", "country_samp_df.csv"))
country_house <- fread(here("Output", "country_house_df.csv"))
country_predesign <- fread(here("Output", "country_predesigned_df.csv"))

#create non-duplicated dataset
#some studies have multiple rows (bc looked at more than one species, marker type, etc)
#don't want to double count these
all_data_deduplicate <- all_data[!duplicated(all_data$Article_Number), ] #down from 282 to 218 rows

##########################################################################################################################################

######## Fig 1 - System & Taxa ########

#### Fig 1A - System ####
#Using deduplicated since interested in system of publication -- don't want to inflate with multiple studies in a publication

#sum # studies w/various systems
systems_count <- all_data_deduplicate[, .N, by = (system)]
  systems_count$system[systems_count$system == "freshwater"] <- "Freshwater"
  systems_count$system[systems_count$system == "marine"] <- "Marine"
  systems_count$system[systems_count$system == "other"] <- "Other"
  systems_count$system[systems_count$system == "terrestrial"] <- "Terrestrial"

## create system figure ##
#(4500 x 4500)
systems_plot <- ggplot(data = systems_count, aes(x = reorder(system, -N), y = N)) + 
  geom_bar(stat = "identity", color = "grey", fill = "grey") + 
  geom_text(data = NULL, x = 4.5, y = 125, label = "A", size = 100) + 
  scale_x_discrete(labels=c("", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme_minimal() + ylab("N") +
  theme(plot.margin = unit(c(8, 2, 10, 2), "cm"), 
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 150, color = "black"),
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Terrestrial", x = 1, y = -6, size = 150) + 
  draw_label("Marine", x = 2, y = -6, size = 150) + 
  draw_label("Other", x = 3, y = -6, size = 150) + 
  draw_label("Freshwater", x = 4, y = -6, size = 150) + 
  draw_label("System", x = 2.5, y = -14, size = 170) + 
  draw_label("110", x = 1, y = 104, size = 150) + 
  draw_label("51", x = 2, y = 45, size = 150) + 
  draw_label("29", x = 3, y = 23, size = 150) + 
  draw_label("28", x = 4, y = 22, size = 150)
systems_plot

#### Fig 1B - Taxa ####
#Using deduplicated since interested in taxa of publication -- don't want to inflate with multiple studies in a publication

#sum # studies w/various taxa
tax_common_names <- c("Fish", "Mammals", "Birds", "Insects", "Reptiles", "Amphibians", "Crustaceans", 
                      "Mollusks", "Coral", "Tunicates", "Annelids", "Arachnids") #combining classes together
tax_common_count <- c(67, 57, 40, 21, 9, 4, 6, 8, 1, 2, 1, 2)
tax_common_df <- as.data.frame(cbind(tax_common_names, tax_common_count))
colnames(tax_common_df) <- c("Taxon", "N")
tax_common_df$N <- as.numeric(as.character(tax_common_df$N))

## create taxa figure ##
#(8000 x 5000)  
tax_plot <- ggplot(data = tax_common_df, aes(x = reorder(Taxon, -N), y = N)) + 
  geom_bar(stat = "identity", color = "grey", fill = "grey") + 
  geom_text(data = NULL, x = 12.25, y = 75, label = "B", size = 100) + 
  scale_x_discrete(labels=c("", "", "", "", "", "", "", "", "", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme_minimal() + ylab("N") + 
  theme(plot.margin = unit(c(6, 2, 10, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 150, color = "black"), 
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Fish", x = 1, y = -3, size = 120) + 
  draw_label("Mammals", x = 2, y = -3, size = 120) + 
  draw_label("Birds", x = 3, y = -3, size = 120) + 
  draw_label("Insects", x = 4, y = -3, size = 120) + 
  draw_label("Rept.", x = 5, y = -3, size = 120) + 
  draw_label("Mollus.", x = 6, y = -3, size = 120) + 
  draw_label("Crustac.", x = 7, y = -3, size = 120) + 
  draw_label("Amphib.", x = 8, y = -3, size = 120) + 
  draw_label("Arach.", x = 9, y = -3, size = 120) + 
  draw_label("Tunic.", x = 10, y = -3, size = 120) + 
  draw_label("Annelids", x = 11, y = -3, size = 120) + 
  draw_label("Corals", x = 12, y = -3, size = 120) + 
  draw_label("Taxon", x = 6.5, y = -10, size = 170) + 
  draw_label("67", x = 1, y = 64, size = 150) + 
  draw_label("57", x = 2, y = 54, size = 150) + 
  draw_label("40", x = 3, y = 37, size = 150) + 
  draw_label("21", x = 4, y = 18, size = 150) + 
  draw_label("9", x = 5, y = 6, size = 150) + 
  draw_label("8", x = 6, y = 5, size = 150) + 
  draw_label("6", x = 7, y = 3, size = 150) + 
  draw_label("4", x = 8, y = 7, size = 150) + 
  draw_label("2", x = 9, y = 5, size = 150) + 
  draw_label("2", x = 10, y = 5, size = 150) + 
  draw_label("1", x = 11, y = 4, size = 150) + 
  draw_label("1", x = 12, y = 4, size = 150)
tax_plot

Fig1_syst_tax_plot <- grid.arrange(systems_plot, tax_plot, ncol = 1)

##########################################################################################################################################

######## Fig 2 - Museum Sample Distribution ########

#### Fig 2A - Original Location ####

#create factor levels for mapping
country_samp$Samples <- as.character(country_samp$Samples)
country_samp$Samples <- factor(country_samp$Samples, levels = c("20", "10", "5", "2", "1", "0"))

#get world map data
world_map <- map_data("world") %>% 
  filter(! long > 180)

## create samp original distribution figure ##
#(5000 x 3000)
country_samp_plot <- country_samp %>% 
  ggplot(aes(fill = Samples, map_id = Region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = c("#4379C0", "#5D93DA", "#81ABE2","#A5C3EA", "#DBE7F6", "#E0E0E1"), 
                    labels = c("\u226520", "\u226510", "\u22655", "\u22652", "\u22651", "0")) + #\u2265 is unicode for greater than or equal to sign
  theme_map() + 
  theme(legend.title = element_text(size = 90), legend.text = element_text(size = 90),
        legend.position = c(-0.005, 0.25), legend.key.size = unit(2, "cm"), 
        plot.margin = unit(c(6, 2, 2, 2), "cm")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("A", x = 179, y = 90, size = 180) + 
  draw_label("archive sampling distribution", x = -126, y = 90, size = 110)
country_samp_plot

#### Fig 2B - Housing Location ####

#create factor levels for mapping
country_house$Samples <- as.character(country_house$Samples)
country_house$Samples <- factor(country_house$Samples, levels = c("50", "20", "5", "2", "1", "0", "Displaced"))

#get world map data
world_map <- map_data("world") %>% 
  filter(! long > 180)

## create samp house distribution figure ##
#(5000 x 3000)
country_house_plot <- country_house %>% 
  ggplot(aes(fill = Samples, map_id = Region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = c("#25436B", "#4379C0", "#81ABE2","#A5C3EA", "#DBE7F6", "#E0E0E1", "#5B5B5B"), 
                    labels = c("\u226550", "\u226520", "\u22655", "\u22652", "\u22651", "0", "0 (Displaced)")) + #\u2265 is unicode for greater than or equal to 
  theme_map() + 
  theme(legend.title = element_text(size = 90), legend.text = element_text(size = 90),
        legend.position = c(-0.005, 0.285), legend.key.size = unit(2, "cm"),
        plot.margin = unit(c(6, 2, 2, 2), "cm")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("B", x = 179, y = 90, size = 180) + 
  draw_label("archive housing distribution", x = -128, y = 90, size = 110)
country_house_plot

Fig2_country_dist_plot <- grid.arrange(country_samp_plot, country_house_plot,
                                       ncol = 1) #(5000 x 6000)

##########################################################################################################################################

######## Fig 3 - Subject & Driver ########

#### Fig 3A - Subject ####
#Using deduplicated since interested in subject(s) of publication -- don't want to inflate with multiple studies in a publication

#sum # studies w/various subjects
#one study can investigate more than one, hence subject 1, 2, 3, etc.
subject1 <- all_data_deduplicate[, .N, by = .(subject_1)]
  colnames(subject1) <- c("subject", "N1")
subject2 <- all_data_deduplicate[, .N, by = .(subject_2)]
  subject2 <- subset(subject2, subject2$subject_2 != "")
  colnames(subject2) <- c("subject", "N2")
subject3 <- all_data_deduplicate[, .N, by = .(subject_3)]
  subject3 <- subset(subject3, subject3$subject_3 != "")
  colnames(subject3) <- c("subject", "N3")
subject4 <- all_data_deduplicate[, .N, by = .(subject_4)]
  subject4 <- subset(subject4, subject4$subject_4 != "")
  colnames(subject4) <- c("subject", "N4")

#merge subject data.tables
subject_list <- list(subject1, subject2, subject3, subject4)
  subject <- subject_list %>% reduce(full_join, by = c("subject"), all = TRUE)

#sum across columns to get total N subject_by_year
subject$Ntot <- rowSums(subject[, c("N1", "N2", "N3", "N4")], na.rm = TRUE)
  subject <- subject[-5, ] #remove last row

## create subject figure ##
#(4500 x 4500)
subject_plot <- ggplot(data = subject, aes(x = reorder(subject, -Ntot), y = Ntot)) + 
  geom_bar(stat = "identity", color = "grey", fill = "grey") + 
  geom_text(data = NULL, x = 4.5, y = 170, label = "A", size = 100) + #50
  scale_x_discrete(labels=c("", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme_minimal() + ylab("N") +
  theme(plot.margin = unit(c(2, 2, 8, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 150, color = "black"),
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Diversity", x = 1, y = -8, size = 150) +
  draw_label("Connectivity", x = 2, y = -8, size = 150) + 
  draw_label("Pop. Size", x = 3, y = -8, size = 150) + 
  draw_label("Adaptation", x = 4, y = -8, size = 150) + 
  draw_label("Subject", x = 2.5, y = -24, size = 170) + 
  draw_label("158", x = 1, y = 150, size = 150) + 
  draw_label("116", x = 2, y = 108, size = 150) + 
  draw_label("93", x = 3, y = 85, size = 150) + 
  draw_label("34", x = 4, y = 26, size = 150)
subject_plot

#### Fig 3B - Driver of Change ####
#Using deduplicated since interested in driver(s) of publication -- don't want to inflate with multiple studies in a publication

#fix missing driver in driver_process1 column
all_data_deduplicate$driver_process1[is.na(all_data_deduplicate$driver_process1)] <- "no_driver" #bc in this column, if not listed then does not have driver

#sum # studies w/various drivers
#one study can investigate more than one, hence driver 1, 2, 3, etc.
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

## create driver figure ##
#(7000 x 5000)
driver_plot <- ggplot(data = num_drivers, aes(x = reorder(driver_process, -Ntot), y = Ntot)) + 
  geom_bar(stat = "identity", color = "grey", fill = "grey") + 
  geom_text(data = NULL, x = 9.25, y = 75, label = "B", size = 100) + 
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme_minimal() + ylab("N") + 
  theme(plot.margin = unit(c(2, 2, 18, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 150, color = "black"),
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Habitat\nLoss", x = 1, y = -8, size = 150) + 
  draw_label("Env.\nVariation", x = 2, y = -8, size = 150) +
  draw_label("Human\nExploit.", x = 3, y = -8, size = 150) +
  draw_label("Invasive\nSpecies", x = 4, y = -8, size = 150) +
  draw_label("Climate\nChange", x = 5, y = -8, size = 150) +
  draw_label("Disease", x = 6, y = -8, size = 150) +
  draw_label("Competition", x = 7, y = -4, size = 150) +
  draw_label("Nat.\nDisaster", x = 8, y = -8, size = 150) +
  draw_label("No Driver", x = 9, y = -8, size = 150) + 
  draw_label("Driver of Change", x = 5, y = -22, size = 170) + 
  draw_label("71", x = 1, y = 67, size = 150) + 
  draw_label("65", x = 2, y = 61, size = 150) + 
  draw_label("64", x = 3, y = 60, size = 150) + 
  draw_label("37", x = 4, y = 33, size = 150) + 
  draw_label("18", x = 5, y = 14, size = 150) + 
  draw_label("13", x = 6, y = 9, size = 150) + 
  draw_label("7", x = 7, y = 3, size = 150) + 
  draw_label("7", x = 8, y = 3, size = 150) + 
  draw_label("28", x = 9, y = 24, size = 150)
driver_plot

Fig3_subj_driv_plot <- grid.arrange(subject_plot, driver_plot, ncol = 1) #(7000 x 5000)

#########################################################################################################################################

######## Fig 4 - Marker ########

#NOT "usual" deduplicated bc same study may use different markers

#removing rows if have same Article_Number (same publication) and same marker type
all_data_dedup_marker <- distinct(all_data, Article_Number, data_type, .keep_all = TRUE)

#sum marker by year
marker_by_year <- all_data_dedup_marker[, .N, by = .(data_type, Publication_Year)]
  marker_by_year <- marker_by_year[data_type == "", data_type := NA] #change rows with blank data_type to NA

## create marker figure ##
#HRM & X-chromosome don't show bc just a point
#(4500 x 4500)
marker_by_year_plot <- ggplot(data = na.omit(marker_by_year), aes(x = Publication_Year, y = N, group = data_type)) + 
  geom_smooth(aes(color = data_type, linetype = data_type), size = 10, se = FALSE) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + 
  scale_color_manual(values = c("#332288", "#117733", "#44AA99", "#88CCEE"), 
                     labels = c("microsatellite", "mtDNA seq", "nuclear seq", "SNP")) + 
  scale_linetype_manual(values = c("solid", "dotdash", "dashed", "dotted"), 
                        labels = c("microsatellite", "mtDNA seq", "nuclear seq", "SNP")) + 
  theme_minimal() + labs(color = "Marker", linetype = "Marker") + 
  theme(plot.margin = unit(c(2, 2, 8, 2), "cm"), 
        axis.ticks = element_line(color = "black", size = 4),
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.line = element_line(size = 2, color = "black"),
        axis.text = element_text(size = 150, color = "black"), legend.position = c(0.2, 0.9), 
        legend.title = element_blank(), legend.text = element_text(size = 120), 
        legend.key.size = unit(6, "cm")) + 
  coord_cartesian(clip = "off") + 
  draw_label("Publication Year", x = 2010, y = -1.5, size = 170)
marker_by_year_plot

#########################################################################################################################################

######## Fig 5 - Year/Gen & Driver ########

#NOT "usual" deduplicated bc same study may have different organisms with different time points OR temporal sampling scheme

#removing rows if have same Article_Number (same publication) and same earliest time point, latest time point, and gen time
all_data_dedup_time <- distinct(all_data, Article_Number, earliest_timepoint, latest_timepoint, gen_time, .keep_all = TRUE)

#calculate diff (in days) between earliest and latest timepoint
#using absolute values just in case columns accidentally switched
all_data_dedup_time[, day_diff := abs(as.Date(all_data_dedup_time$latest_timepoint, format = "%Y.%m.%d") - 
                                                as.Date(all_data_dedup_time$earliest_timepoint, format = "%Y.%m.%d"))]

#convert # days btwn timepoints to # generations
all_data_dedup_time[, gen_diff := all_data_dedup_time$day_diff/all_data_dedup_time$gen_time]

#convert # days btwn timepoints to # years
all_data_dedup_time[, year_diff := all_data_dedup_time$day_diff/365]

#fix missing driver in driver_process1 column
all_data_dedup_time$driver_process1[is.na(all_data_dedup_time$driver_process1)] <- "no_driver" #bc in this column, if not listed then does not have driver

#### Fig 3A - Change in Years ####

## create dataframe of all maxyear x driver combinations ##

## summarize by driver (1, 2, 3) ##
#climate change studies
maxyear_climchange <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "climate_change" | 
                                                                    all_data_dedup_time$driver_process2 == "climate_change" | 
                                                                    all_data_dedup_time$driver_process3 == "climate_change"]) 
  maxyear_climchange$driver_process <- "climate_change"
  colnames(maxyear_climchange) <- c("time_years", "driver_process")

#habitat loss studies
maxyear_habloss <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "habitat_loss" | 
                                                                    all_data_dedup_time$driver_process2 == "habitat_loss" | 
                                                                    all_data_dedup_time$driver_process3 == "habitat_loss"]) 
  maxyear_habloss$driver_process <- "habitat_loss"
  colnames(maxyear_habloss) <- c("time_years", "driver_process")

#env variation studies
maxyear_envvar <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "env_variation" | 
                                                                    all_data_dedup_time$driver_process2 == "env_variation" | 
                                                                    all_data_dedup_time$driver_process3 == "env_variation"]) 
  maxyear_envvar$driver_process <- "env_variation"
  colnames(maxyear_envvar) <- c("time_years", "driver_process")

#human exploitation studies
maxyear_humexploit <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "human_exploitation" | 
                                                                    all_data_dedup_time$driver_process2 == "human_exploitation" | 
                                                                    all_data_dedup_time$driver_process3 == "human_exploitation"]) 
  maxyear_humexploit$driver_process <- "human_exploitation"
  colnames(maxyear_humexploit) <- c("time_years", "driver_process")

#invasive species studies
maxyear_invspecies <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "invasive_species" | 
                                                                    all_data_dedup_time$driver_process2 == "invasive_species" | 
                                                                    all_data_dedup_time$driver_process3 == "invasive_species"]) 
  maxyear_invspecies$driver_process <- "invasive_species"
  colnames(maxyear_invspecies) <- c("time_years", "driver_process")

#disease studies
maxyear_disease <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "disease" | 
                                                                    all_data_dedup_time$driver_process2 == "disease" | 
                                                                    all_data_dedup_time$driver_process3 == "disease"]) 
  maxyear_disease$driver_process <- "disease"
  colnames(maxyear_disease) <- c("time_years", "driver_process")

#competition studies
maxyear_competition <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "competition" | 
                                                                    all_data_dedup_time$driver_process2 == "competition" | 
                                                                    all_data_dedup_time$driver_process3 == "competition"]) 
  maxyear_competition$driver_process <- "competition"
  colnames(maxyear_competition) <- c("time_years", "driver_process")

#natural disaster studies
maxyear_natdisaster <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "natural_disaster" | 
                                                                    all_data_dedup_time$driver_process2 == "natural_disaster" | 
                                                                    all_data_dedup_time$driver_process3 == "natural_disaster"]) 
  maxyear_natdisaster$driver_process <- "natural_disaster"
  colnames(maxyear_natdisaster) <- c("time_years", "driver_process")
  
#no driver studies
maxyear_nodriver <- as.data.frame(all_data_dedup_time$year_diff[all_data_dedup_time$driver_process1 == "no_driver"]) 
  maxyear_nodriver$driver_process <- "no_driver"
  colnames(maxyear_nodriver) <- c("time_years", "driver_process")

## rbind all data.frames together ##
maxyear_all_drivers <- rbind(maxyear_climchange, maxyear_competition, maxyear_disease, maxyear_envvar, 
                             maxyear_habloss, maxyear_humexploit, maxyear_invspecies, 
                             maxyear_natdisaster, maxyear_nodriver)

## create maxyear by driver figure ##
#(8000 x 5000)
maxyear_by_driver_plot <- ggplot() +
  geom_boxplot(data = maxyear_all_drivers, aes(x = driver_process, y = time_years), color = "black", lwd = 8) + 
  geom_point(data = maxyear_all_drivers, aes(x = driver_process, y = time_years), size = 30, alpha = 0.4) + 
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(name = "Number of Years", expand = expansion(mult = c(0, .1)), limits = c(0, 160)) +
  theme_minimal() + 
  theme(plot.margin = unit(c(4, 2, 16, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 130, color = "black"),
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Climate\nChange", x = 1, y = -12, size = 130) + 
  draw_label("Comp.", x = 2, y = -12, size = 130) + 
  draw_label("Disease", x = 3, y = -12, size = 130) + 
  draw_label("Env.\nVariation", x = 4, y = -12, size = 130) + 
  draw_label("Habitat\nLoss", x = 5, y = -12, size = 130) + 
  draw_label("Human\nExploit.", x = 6, y = -12, size = 130) + 
  draw_label("Invasive\nSpecies", x = 7, y = -12, size = 130) + 
  draw_label("Nat.\nDisaster", x = 8, y = -12, size = 130) + 
  draw_label("No Driver", x = 9, y = -12, size = 130) + 
  draw_label("Driver of Change", x = 5, y = -34, size = 170) +
  draw_label("A", x = 9.5, y = 175, size = 300)
maxyear_by_driver_plot

#### Fig 3B - Change in Generations ####

## create dataframe of all maxgen x driver combinations ##

## summarize by driver (1, 2, 3) ##
#climate change studies
maxgen_climchange <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "climate_change" | 
                                                                    all_data_dedup_time$driver_process2 == "climate_change" | 
                                                                    all_data_dedup_time$driver_process3 == "climate_change"]) 
maxgen_climchange$driver_process <- "climate_change"
colnames(maxgen_climchange) <- c("time_gens", "driver_process")

#habitat loss studies
maxgen_habloss <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "habitat_loss" | 
                                                                 all_data_dedup_time$driver_process2 == "habitat_loss" | 
                                                                 all_data_dedup_time$driver_process3 == "habitat_loss"]) 
maxgen_habloss$driver_process <- "habitat_loss"
colnames(maxgen_habloss) <- c("time_gens", "driver_process")

#env variation studies
maxgen_envvar <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "env_variation" | 
                                                                all_data_dedup_time$driver_process2 == "env_variation" | 
                                                                all_data_dedup_time$driver_process3 == "env_variation"]) 
maxgen_envvar$driver_process <- "env_variation"
colnames(maxgen_envvar) <- c("time_gens", "driver_process")

#human exploitation studies
maxgen_humexploit <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "human_exploitation" | 
                                                                    all_data_dedup_time$driver_process2 == "human_exploitation" | 
                                                                    all_data_dedup_time$driver_process3 == "human_exploitation"]) 
maxgen_humexploit$driver_process <- "human_exploitation"
colnames(maxgen_humexploit) <- c("time_gens", "driver_process")

#invasive species studies
maxgen_invspecies <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "invasive_species" | 
                                                                    all_data_dedup_time$driver_process2 == "invasive_species" | 
                                                                    all_data_dedup_time$driver_process3 == "invasive_species"]) 
maxgen_invspecies$driver_process <- "invasive_species"
colnames(maxgen_invspecies) <- c("time_gens", "driver_process")

#disease studies
maxgen_disease <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "disease" | 
                                                                 all_data_dedup_time$driver_process2 == "disease" | 
                                                                 all_data_dedup_time$driver_process3 == "disease"]) 
maxgen_disease$driver_process <- "disease"
colnames(maxgen_disease) <- c("time_gens", "driver_process")

#competition studies
maxgen_competition <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "competition" | 
                                                                     all_data_dedup_time$driver_process2 == "competition" | 
                                                                     all_data_dedup_time$driver_process3 == "competition"]) 
maxgen_competition$driver_process <- "competition"
colnames(maxgen_competition) <- c("time_gens", "driver_process")

#natural disaster studies
maxgen_natdisaster <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "natural_disaster" | 
                                                                     all_data_dedup_time$driver_process2 == "natural_disaster" | 
                                                                     all_data_dedup_time$driver_process3 == "natural_disaster"]) 
maxgen_natdisaster$driver_process <- "natural_disaster"
colnames(maxgen_natdisaster) <- c("time_gens", "driver_process")

#no driver studies
maxgen_nodriver <- as.data.frame(all_data_dedup_time$gen_diff[all_data_dedup_time$driver_process1 == "no_driver"]) 
maxgen_nodriver$driver_process <- "no_driver"
colnames(maxgen_nodriver) <- c("time_gens", "driver_process")

## rbind all data.frames together ##
maxgen_all_drivers <- rbind(maxgen_climchange, maxgen_competition, maxgen_disease, maxgen_envvar, 
                             maxgen_habloss, maxgen_humexploit, maxgen_invspecies, 
                             maxgen_natdisaster, maxgen_nodriver)
  maxgen_all_drivers <- na.omit(maxgen_all_drivers)

## create maxgen by driver figure ##
#(8000 x 5000)
maxgen_by_driver_plot <- ggplot() +
  geom_boxplot(data = maxgen_all_drivers, aes(x = driver_process, y = time_gens), color = "black", lwd = 8) + 
  geom_point(data = maxgen_all_drivers, aes(x = driver_process, y = time_gens), size = 30, alpha = 0.4) + 
  scale_x_discrete(labels = c("", "", "", "", "", "", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(name = "Number of Generations", expand = expansion(mult = c(0, .1)), limits = c(0, 160)) +
  theme_minimal() + 
  theme(plot.margin = unit(c(4, 2, 16, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 130, color = "black"),
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("Climate\nChange", x = 1, y = -12, size = 130) + 
  draw_label("Comp.", x = 2, y = -12, size = 130) + 
  draw_label("Disease", x = 3, y = -12, size = 130) + 
  draw_label("Env.\nVariation", x = 4, y = -12, size = 130) + 
  draw_label("Habitat\nLoss", x = 5, y = -12, size = 130) + 
  draw_label("Human\nExploit.", x = 6, y = -12, size = 130) + 
  draw_label("Invasive\nSpecies", x = 7, y = -12, size = 130) + 
  draw_label("Nat.\nDisaster", x = 8, y = -12, size = 130) + 
  draw_label("No Driver", x = 9, y = -12, size = 130) + 
  draw_label("Driver of Change", x = 5, y = -34, size = 170) +
  draw_label("B", x = 9.5, y = 175, size = 300)
maxgen_by_driver_plot

Fig5_maxtime_driv_plot <- grid.arrange(maxyear_by_driver_plot, maxgen_by_driver_plot, ncol = 1) #(7000 x 5000)

##########################################################################################################################################

######## Fig S1 - Author Affiliation ########

## build author databases ##

author_groups <- c(rep("None", 3), rep("All", 3), rep(">1", 3))
country_group <- c(rep(c("Australia", "New Zealand", "All other countries"), 3))

#### museum samples ####
author_count_museum <- c(0, 0, 13, 3, 6, 15, 0, 0, 5)

author_museum_df <- as.data.frame(cbind(author_groups, country_group, author_count_museum)) #merge together
  colnames(author_museum_df) <- c("Author", "Country", "N")
  author_museum_df$N <- as.numeric(as.character(author_museum_df$N))
  author_museum_df$Author <- factor(author_museum_df$Author, levels = c("All", ">1", "None"))

## create author museum samples figure ##
#4500 x 5000
author_museum_plot <- ggplot(data = author_museum_df, aes(x = Author, y = N, fill = Country)) + 
  geom_col() + labs(y = "Number of Studies") + 
  geom_text(data = NULL, x = 0.75, y = 61, label = "A", size = 80) + 
  theme_minimal() + xlab("Author Affiliations From Sampled Countries") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  scale_fill_manual(values = c("#d6d2e7", "#847ab7", "#332288")) + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 140),
        axis.text = element_text(size = 140, color = "black"), legend.position = "none",
        legend.title = element_blank(), legend.text = element_text(size = 120),
        legend.key.size = unit(6, "cm"), legend.box = "horizontal")
author_museum_plot 

#### predesigned samples #### 
author_count_predesign <- c(0, 0, 12, 11, 0, 19, 0, 0, 2)

author_predesign_df <- as.data.frame(cbind(author_groups, country_group, author_count_predesign)) #merge together
  colnames(author_predesign_df) <- c("Author", "Country", "N")
  author_predesign_df$N <- as.numeric(as.character(author_predesign_df$N))
  author_predesign_df$Author <- factor(author_predesign_df$Author, levels = c("All", ">1", "None"))

## create author predesign samples figure ##
#4500 x 5000
author_predesign_plot <- ggplot(data = author_predesign_df, aes(x = Author, y = N, fill = Country)) + 
  geom_col() + labs(y = "Number of Studies") + 
  geom_text(data = NULL, x = 0.75, y = 95, label = "B", size = 80) + 
  theme_minimal() + xlab("Author Affiliations From Sampled Countries") +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  scale_fill_manual(values = c("#d6d2e7", "#847ab7", "#332288")) + 
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 140),
        axis.text = element_text(size = 140, color = "black"), legend.position = c(0.65, 0.83),
        legend.title = element_blank(), legend.text = element_text(size = 120),
        legend.key.size = unit(6, "cm"), legend.box = "horizontal")
author_predesign_plot 

FigS1_author_plot <- grid.arrange(author_museum_plot, author_predesign_plot, ncol = 2) #(8500 x 5000)

#################################################################################################################################

######## Fig S2 - Driver by Taxa ########

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
  driver_by_taxa <- driver_by_taxa[driver_by_taxa$N1 != "", ] #remove rows where driver_process missing (N2 & N3 only)
    
#build dataset where condensed classes to higher order taxa
#Fish = Actinopterygii & Chondrichthyes, Mollusks = Bivalvia & Gastropoda, Crustaceans = Malacostraca, Branchiopoda & Hexanauplia, Tunicates = Ascidiacea, Annelids = Polychaeta
tax_names <- c(rep("Amphibians", 3), rep("Annelids", 1), rep("Arachnids", 2), rep("Birds", 7), 
               rep("Coral", 1), rep("Crustaceans", 4), rep("Fish", 6), rep("Insects", 6), 
               rep("Mammals", 9), rep("Mollusks", 3), rep("Reptiles", 6), rep("Tunicates", 2))
tax_drivers <- c("Env. Variation", "Nat. Disaster", "No Driver", "Env. Variation", "Climate Change", 
                 "Env. Variation", "Climate Change", "Competition", "Disease", "Env. Variation", 
                 "Habitat Loss", "Human Exploit.", "Invasive Species", "Climate Change", "Env. Variation",
                 "Habitat Loss", "Invasive Species", "No Driver", "Climate Change", "Env. Variation", 
                 "Habitat Loss", "Human Exploit.", "Invasive Species", "No Driver", "Disease", "Env. Variation", 
                 "Habitat Loss", "Invasive Species", "Nat. Disaster", "No Driver", "Climate Change", "Competition", 
                 "Disease", "Env. Variation", "Habitat Loss", "Human Exploit.", "Invasive Species", "Nat. Disaster", 
                 "No Driver", "Env. Variation", "Human Exploit.", "No Driver", "Climate Change", "Env. Variation", 
                 "Habitat Loss", "Human Exploit.", "Invasive Species", "No Driver", "Env. Variation", "Invasive Species")
tax_driver_num <- c(1, 1, 2, 1, 1, 1, 3, 1, 5, 6, 28, 10, 11, 1, 3, 1, 2, 1, 7, 28, 9, 20, 8, 15, 2, 9, 4, 7, 1, 2, 
                    3, 4, 5, 8, 28, 28, 5, 1, 5, 4, 2, 2, 1, 2, 1, 4, 3, 1, 2, 1)

tax_driver_df <- as.data.frame(cbind(tax_names, tax_drivers, tax_driver_num)) #merge datasets together
  colnames(tax_driver_df) <- c("Taxon", "Driver_Process", "N")
  tax_driver_df$N <- as.numeric(as.character(tax_driver_df$N))

## create driver by taxa figure ##
#5000 x 4500
driver_by_taxa_plot <- ggplot(data = tax_driver_df, aes(x = reorder(Taxon, -N), y = N, fill = Driver_Process)) + 
  geom_col() + labs(y = "Count") + 
  theme_minimal() + xlab("Taxon") + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.title = element_text(size = 170),
        axis.text = element_text(size = 120, color = "black"), legend.position = c(0.8, 0.6),
        axis.text.x = element_text(angle = 90),
        legend.title = element_blank(), legend.text = element_text(size = 100), 
        legend.key.size = unit(6, "cm"))
driver_by_taxa_plot 

#################################################################################################################################

######## Fig S3 - Time Points ########

#sum # studies w/various time points
time_points <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "multiple")
time_points_count <- c(104, 21, 14, 10, 3, 5, 2, 5, 13, 41)
time_points_df <- as.data.frame(cbind(time_points, time_points_count))
colnames(time_points_df) <- c("TimePoint", "N")
  time_points_df$N <- as.numeric(as.character(time_points_df$N))

#create factor levels for mapping
time_points_df$TimePoint <- factor(time_points_df$TimePoint, levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "multiple"))
  
## create time points figure ##
#(7000 x 5000)  
time_points_plot <- ggplot(data = time_points_df, aes(x = TimePoint, y = N)) + 
  geom_bar(stat = "identity", color = "grey", fill = "grey") + 
  scale_x_discrete(labels=c("", "", "", "", "", "", "", "", "", "")) + #remove x-axis labels to fill in manually
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + #get bars to touch x-axis
  theme_minimal() + ylab("N") + 
  theme(plot.margin = unit(c(6, 2, 10, 2), "cm"),
        axis.ticks.y = element_line(color = "black", size = 4),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 170),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 150, color = "black"), 
        axis.line = element_line(size = 2, color = "black")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("2", x = 1, y = -3, size = 150) + 
  draw_label("3", x = 2, y = -3, size = 150) + 
  draw_label("4", x = 3, y = -3, size = 150) + 
  draw_label("5", x = 4, y = -3, size = 150) + 
  draw_label("6", x = 5, y = -3, size = 150) + 
  draw_label("7", x = 6, y = -3, size = 150) + 
  draw_label("8", x = 7, y = -3, size = 150) + 
  draw_label("9", x = 8, y = -3, size = 150) + 
  draw_label("\u226510", x = 9, y = -3, size = 150) + 
  draw_label("Multiple", x = 10, y = -3, size = 150) + 
  draw_label("Time Points", x = 5.5, y = -10, size = 170) + 
  draw_label("104", x = 1, y = 100, size = 150) + 
  draw_label("21", x = 2, y = 17, size = 150) + 
  draw_label("14", x = 3, y = 10, size = 150) + 
  draw_label("10", x = 4, y = 6, size = 150) + 
  draw_label("3", x = 5, y = 6, size = 150) + 
  draw_label("5", x = 6, y = 8, size = 150) + 
  draw_label("2", x = 7, y = 5, size = 150) + 
  draw_label("5", x = 8, y = 8, size = 150) + 
  draw_label("13", x = 9, y = 9, size = 150) + 
  draw_label("41", x = 10, y = 37, size = 150)
time_points_plot

##########################################################################################################################################

######## Fig S4 - Pre-designed Sample Distribution ########

#create factor levels for mapping
country_predesign$Samples <- as.character(country_predesign$Samples)
country_predesign$Samples <- factor(country_predesign$Samples, levels = c("20", "10", "5", "2", "1", "0"))

#get world map data
world_map <- map_data("world") %>% 
  filter(! long > 180)

## create samp predesign distribution figure ##
#(5000 x 3000)
country_predesign_plot <- country_predesign %>% 
  ggplot(aes(fill = Samples, map_id = Region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = c("#4379C0", "#5D93DA", "#81ABE2","#A5C3EA", "#DBE7F6", "#E0E0E1"), 
                    labels = c("\u226520", "\u226510", "\u22655", "\u22652", "\u22651", "0")) + #\u2265 is unicode for greater than or equal to sign
  theme_map() + 
  theme(legend.title = element_text(size = 90), legend.text = element_text(size = 90),
        legend.position = c(-0.005, 0.25), legend.key.size = unit(2, "cm"), 
        plot.margin = unit(c(6, 2, 2, 2), "cm")) + 
  coord_cartesian(clip = "off") + #to plot outside the box
  draw_label("predesign sampling distribution", x = -117, y = 90, size = 110)
country_predesign_plot
