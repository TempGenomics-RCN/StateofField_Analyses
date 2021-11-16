################################################### Script for Compiling Accepted Data  #######################################################

#assemble all accepted data and do basic QA/QC
#starts from csv versions of "*_accepted.xlsx" files each group filtered independently

##########################################################################################################################################

######## Set-up ########

remove(list = ls())
getwd() #check working directory

#load libraries
library(data.table)

#read in data
adaptation_dt <- fread("Data/adaptation_accepted.csv")
  adaptation_dt <- adaptation_dt[, 1:40] #subset to just first 40 rows with relevant info
connectivity_dt <- fread("Data/connectivity_accepted.csv")
  connectivity_dt <- connectivity_dt[, 1:40]
diversity_dt <- fread("Data/diversity_accepted.csv")
  diversity_dt <- diversity_dt[, 1:40]
popsize_dt <- fread("Data/popsize_accepted.csv")
  popsize_dt <- popsize_dt[, 1:40]

##########################################################################################################################################

######## Check for duplicates ########
#checking across databases to make sure same study wasn't recorded 2X

#check for duplicate studies across spreadsheets
poss_dup_studies <- sort(unique(c(adaptation_dt$Article_Title[adaptation_dt$Article_Title %in% connectivity_dt$Article_Title],
                                  adaptation_dt$Article_Title[adaptation_dt$Article_Title %in% diversity_dt$Article_Title],
                                  adaptation_dt$Article_Title[adaptation_dt$Article_Title %in% popsize_dt$Article_Title],
                                  connectivity_dt$Article_Title[connectivity_dt$Article_Title %in% diversity_dt$Article_Title],
                                  connectivity_dt$Article_Title[connectivity_dt$Article_Title %in% popsize_dt$Article_Title],
                                  diversity_dt$Article_Title[diversity_dt$Article_Title %in% popsize_dt$Article_Title])))
#one duplicate study ("Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia")
  
#check dataframe for study
adaptation_dup <- subset(adaptation_dt, adaptation_dt$Article_Title == "Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia")
connectivity_dup <- subset(connectivity_dt, connectivity_dt$Article_Title == "Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia") #one hit
diversity_dup <- subset(diversity_dt, diversity_dt$Article_Title == "Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia") #one hit
popsize_dup <- subset(popsize_dt, popsize_dt$Article_Title == "Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia")

#dup study in connectivity & diversity --> looks like was filtered in diversity so removing from connectivity
connectivity_dt <- subset(connectivity_dt, connectivity_dt$Article_Title != "Range-wide fragmentation in a threatened fish associated with post-European settlement modification in the Murray-Darling Basin, Australia")


######## Merge datasets ########

#verify all column names match
all(names(adaptation_dt) == names(connectivity_dt))
all(names(adaptation_dt) == names(diversity_dt))
all(names(adaptation_dt) == names(popsize_dt))

#merge datasets together
all_dt <- rbind(adaptation_dt, connectivity_dt, diversity_dt, popsize_dt)
dim(all_dt) #487x40

##########################################################################################################################################

######## Clean newly merged dataset ########

#remove studies that were rejected in "decision" column
all_dt_accepted <- subset(all_dt, all_dt$Decision == "accept") #359x40 (128 studies from this list that got rejected)

#remove studies that don't have "accept" in "removal_criteria" column
all_dt_accepted_final <- subset(all_dt_accepted, all_dt_accepted$Removal_Criteria == "accept") #359 rows, shouldn't really remove any

#TEMP: trim to studies that have data
#doing this on "system" column bc this should be recorded for every study/record
all_dt_accepted_wdata <- subset(all_dt_accepted_final, all_dt_accepted_final$system != "") #bc not coded as "NA" but just left blank
#dim: 164 rows (out of 359) --> 46% complete

#split time period up into different columns
#data.table version (much like separate() from tidyverse)
#set dataframe, and in that dataframe quickly add (:=) new columns headed TP_# by splitting year_samp column at ,
#puts NA where doesn't have a record
#keeping original year_samp column for now
dt_wdata_splitTP <- setDT(all_dt_accepted_wdata)[,paste0("TP_", 1:63) := tstrsplit(year_samp, ",")] #had to add 63 columns

#split num samp up into different columns as well
#should be same # added columns as with year_samp
dt_wdata_splitTP_splitNS <- setDT(dt_wdata_splitTP)[,paste0("NS_", 1:63) := tstrsplit(num_samp, ",")] #had to add 63 columns
  dim(dt_wdata_splitTP_splitNS) #164x166 --> 63x2 + 40

#########################################################################################################################################
  
######## QAxQC ########
  
#Check string length of TP rows --> make sure in proper format

#check taxa sampled list
#check country sample list
#check gen time list (and character type)
#check study design, etc list

#look for rows where data is missing --> esp TP & NS