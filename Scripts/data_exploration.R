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
  geom_line(aes(color = subject_1, size = 2)) + 
  geom_point(aes(color = subject_1, size = 2)) + 
  theme_minimal()
sub_by_year_plot

#### system trends ####
system_by_subject1 <- all_data[, .N, by = .(subject_1, system)]

#plot system_by_subject1
s_by_s_plot <- ggplot(data = system_by_subject1, aes(x = subject_1, y = N, fill = system)) + 
  geom_bar(stat = "identity", color = "black") + 
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
  geom_bar(stat = "identity", color = "black") + 
  theme_minimal()
t_by_s_plot
