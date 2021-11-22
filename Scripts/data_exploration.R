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

#count #system occurrences by subject_1
#data.table structure data.table[filter, function, grouped by what]
#.N stores the # of rows in a subject (Ex: count # rows in each cat cross-section)

system_by_subject1 <- all_data[, .N, by = .(subject_1, system)]


#plot system_by_subject1
s_by_s_plot <- ggplot(data = system_by_subject1, aes(x = subject_1, y = N, fill = system)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  theme_minimal()