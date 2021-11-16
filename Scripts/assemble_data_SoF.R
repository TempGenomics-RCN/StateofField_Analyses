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
all_dt_accepted <- all_dt[Decision == "accept", ] #359x40 (128 studies from this list that got rejected)

#remove studies that don't have "accept" in "removal_criteria" column
all_dt_accepted <- all_dt_accepted[Removal_Criteria == "accept", ] #359 rows, shouldn't really remove any

#TEMP: trim to studies that have data
#doing this on "system" column bc this should be recorded for every study/record
all_dt_accepted <- all_dt_accepted[system != "", ] #bc not coded as "NA" but just left blank
#dim: 164 rows (out of 359) --> 46% complete

#split time period up into different columns
#data.table version (much like separate() from tidyverse)
#set dataframe, and in that dataframe quickly add (:=) new columns headed TP_# by splitting year_samp column at ,
#puts NA where doesn't have a record
#keeping original year_samp column for now
all_dt_accepted <- setDT(all_dt_accepted)[, paste0("TP_", 1:63) := tstrsplit(year_samp, ",")] #had to add 63 columns

#split num samp up into different columns
#should be same # added columns as with year_samp
all_dt_accepted <- setDT(all_dt_accepted)[, paste0("NS_", 1:63) := tstrsplit(num_samp, ",")] #had to add 63 columns
  dim(all_dt_accepted) #164x166 --> 63x2 + 40

#split country samp up into different columns
#remove white spaces first
all_dt_accepted$country_samp <- gsub(' ', '', all_dt_accepted$country_samp)
all_dt_accepted <- setDT(all_dt_accepted)[, paste0("country_", 1:39) := tstrsplit(country_samp, ",")] #had to add 39 columns

#########################################################################################################################################
  
######## QA/QC ########

#### Check taxon names ####
#get list of taxa
taxa <- sort(unique(all_dt_accepted$tax_group))

#check rows with mistakes
check <- all_dt_accepted[tax_group == "Teleostei", ]
View(check)
  
#fix taxa names where mis-spelled/misreported
all_dt_accepted$tax_group[all_dt_accepted$tax_group == "\nMammalia"] <- "Mammalia" #get rid of carriage return character
all_dt_accepted$tax_group[all_dt_accepted$tax_group == "Acidiacea"] <- "Ascidiacea" #fix typo
all_dt_accepted$tax_group[all_dt_accepted$tax_group == "Teleostei"] <- "Actinopterygii" #Teleostei is infraclass in Actinopterygii

#check correct
taxa <- sort(unique(dt_wdata_splitTP_splitNS$tax_group)) #good

#### Check country names ####
#get list of countries
countries <- sort(unique(c(all_dt_accepted$country_1, all_dt_accepted$country_2, all_dt_accepted$country_3, 
                           all_dt_accepted$country_4, all_dt_accepted$country_5, all_dt_accepted$country_6,
                           all_dt_accepted$country_7, all_dt_accepted$country_8, all_dt_accepted$country_9,
                           all_dt_accepted$country_10, all_dt_accepted$country_11, all_dt_accepted$country_12,
                           all_dt_accepted$country_13, all_dt_accepted$country_14, all_dt_accepted$country_15,
                           all_dt_accepted$country_16, all_dt_accepted$country_17, all_dt_accepted$country_18,
                           all_dt_accepted$country_19, all_dt_accepted$country_20, all_dt_accepted$country_21,
                           all_dt_accepted$country_22, all_dt_accepted$country_23, all_dt_accepted$country_24,
                           all_dt_accepted$country_25, all_dt_accepted$country_26, all_dt_accepted$country_27,
                           all_dt_accepted$country_28, all_dt_accepted$country_29, all_dt_accepted$country_30,
                           all_dt_accepted$country_31, all_dt_accepted$country_32, all_dt_accepted$country_33,
                           all_dt_accepted$country_34, all_dt_accepted$country_35, all_dt_accepted$country_36, 
                           all_dt_accepted$country_37, all_dt_accepted$country_38, all_dt_accepted$country_39)))

#check rows with mistakes
check <- which(all_dt_accepted == "BalticSea", arr.ind = TRUE) #checking all columns (aka all country columns simultaneously) and recording indices
View(check)
View(all_dt_accepted[18, ]) #row from check (column just informs which country column it is)


#Check string length of TP rows --> make sure in proper format

#check taxa sampled list
#check country sample list
#check gen time list (and character type)
#check study design, etc list

#look for rows where data is missing --> esp TP & NS