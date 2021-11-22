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
#make sure recorder & first subject match

#### Check publication metadata ####
#check for studies without publication year
nopub_check <- all_dt_accepted[Publication_Year == ""] #all have pub year

#### Check system ####
#check for studies without system recorded
nosystem_check <- all_dt_accepted[system == ""] #all have system recorded

#get list of systems
systems <- sort(unique(all_dt_accepted$system)) #only 4 options --> good

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
taxa <- sort(unique(all_dt_accepted$tax_group)) #good

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
check <- which(all_dt_accepted == "Wales", arr.ind = TRUE) #checking all columns (aka all country columns simultaneously) and recording indices
View(check)
View(all_dt_accepted[92, ]) #row from check (column just informs which country column it is)

#fix country names where mis-spelled/incorrect
all_dt_accepted$country_1[all_dt_accepted$country_1 == "Australia(Tasmania)"] <- "Australia" #should just be Australia for consistency
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Australia(Tasmania)"] <- "Australia"
all_dt_accepted$country_2[all_dt_accepted$country_2 == "BalticSea"] <- "NA" #should just be Estonia
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Estonia,BalticSea"] <- "Estonia"
all_dt_accepted$country_38[all_dt_accepted$country_38 == "Caucasus"] <- "NA" #Caucasus not a country (a region)
  all_dt_accepted$country_23[all_dt_accepted$country_23 == "Macedonia"] <- "NorthMacedonia" #change to proper country name
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Uzbekistan,Iran,China,Japan,Germany,Poland,CzechRepublic,Sweden,Lithuania,Latvia,Austria,Denmark,Finland,Italy,Morocco,Turkey,Croatia,Switzerland,Spain,France,Greece,Hungary,NorthMacedonia,Tunesia,Slovenia,Portugal,Netherlands,,Lebanon,Belgium,Algeria,Syria,Romania,Bulgaria,Ukraine,Russia,Azerbaijan,Caucasus,Georgia"] <- "Uzbekistan,Iran,China,Japan,Germany,Poland,CzechRepublic,Sweden,Lithuania,Latvia,Austria,Denmark,Finland,Italy,Morocco,Turkey,Croatia,Switzerland,Spain,France,Greece,Hungary,Macedonia,Tunesia,Slovenia,Portugal,Netherlands,,Lebanon,Belgium,Algeria,Syria,Romania,Bulgaria,Ukraine,Russia,Azerbaijan,Georgia"
all_dt_accepted$country_1[all_dt_accepted$country_1 == "China(+USA"] <- "China" #USA should be separate entry
  all_dt_accepted$country_6[all_dt_accepted$country_6 == "Japan)"] <- "Japan" #fixing end of () as well
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "China(+USA,Russia,Greece,Lithuania,Kazakhstan,Japan)"] <- "China,USA,Russia,Greece,Lithuania,Kazakhstan,Japan"
all_dt_accepted$country_1[all_dt_accepted$country_1 == "England"] <- "UnitedKingdom" #technical term
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "England,Scotland"] <- "UnitedKingdom"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "England,France,Italy,Portugal,Spain"] <- "UnitedKingdom,France,Italy,Portugal,Spain"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "England,Ireland,Scotland,Wales"] <- "UnitedKingdom,Ireland"
all_dt_accepted$country_2[all_dt_accepted$country_2 == "FloridaKeys"] <- "NA" #NA since country_1 will change to USA
  all_dt_accepted$country_1[all_dt_accepted$country_1 == "MainlandFlorida"] <- "USA" #should just be USA
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "MainlandFlorida,FloridaKeys,Cuba"] <- "USA,Cuba"
all_dt_accepted$country_4[all_dt_accepted$country_4 == "INdia"] <- "India" #fix typo
  all_dt_accepted$country_5[all_dt_accepted$country_5 == "INdonesia"] <- "Indonesia" #fix typo
  all_dt_accepted$country_7[all_dt_accepted$country_7 == "Laous"] <- "Laos" #fix typo
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Bangladesh,Cambodia,China,INdia,INdonesia,Iraq,Laous,Malaysia,Myanmar,Nepal,Pakistan,Thailand,Vietnam"] <- "Bangladesh,Cambodia,China,India,Indonesia,Iraq,Laos,Malaysia,Myanmar,Nepal,Pakistan,Thailand,Vietnam"
all_dt_accepted$country_1[all_dt_accepted$country_1 == "Scotland"] <- "UnitedKingdom" #technical term
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Scotland"] <- "UnitedKingdom"
  all_dt_accepted$country_2[all_dt_accepted$country_2 == "Scotland"] <- "NA" #here should be NA bc England changed to UK
  all_dt_accepted$country_3[all_dt_accepted$country_3 == "Scotland"] <- "NA" #here should be NA bc England changed to UK
all_dt_accepted$country_1[all_dt_accepted$country_1 == "UnitedStates"] <- "USA" #bc USA more common term
  all_dt_accepted$country_2[all_dt_accepted$country_2 == "UnitedStates"] <- "USA"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "UnitedStates"] <- "USA"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "Canada,UnitedStates"] <- "Canada,USA"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "UnitedStates,Ukraine"] <- "USA,Ukraine"
  all_dt_accepted$country_samp[all_dt_accepted$country_samp == "UnitedStates,Switzerland"] <- "USA,Switzerland"
all_dt_accepted$country_4[all_dt_accepted$country_4 == "Wales"] <- "NA" #change to NA bc England changed to UK

#### Check loc_samp, year_samp & num_samp ####
#check to make sure all studies have loc_samp
noloc_check <- all_dt_accepted[loc_samp == ""] #5 rows without loc_samp

#check to make sure all studies have year_samp
noyear_check <- all_dt_accepted[year_samp == ""] #5 rows without year_samp

#check to make sure all studies have num_samp
nonum_check <- all_dt_accepted[num_samp == ""] #10 rows without num_samp

#### Check gen length ####
#check for studies without gen_time
nogen_check <- all_dt_accepted[gen_time == ""] #10 rows without gen_time

#get list of generation times
gen <- sort(unique(all_dt_accepted$gen_time))

#check rows with extra info
check <- all_dt_accepted[gen_time == "XX", ]
View(check)

#fix gen time when necessary
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "1095-1825"] <- 1460 #Taking average
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "1095-2920 (DOI: 10.2960/J.v25.a10)"] <- 2737.5 #Atlantic cod -- gen time is 7.5 yrs according to FishBase
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "1095-3650"] <- 730 #Myotis lucifugus, gen time is 2 yrs (DOI:10.1093/jhered/esu012)
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "1095 - 6205"] <- 3102.5 #average of 7-10 years, from Jacoby & Gollock (2014)
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "1135 (FishBase)"] <- 1153 #removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "12 (DOI: 10.1051/apido:2005016)"] <- 12 #removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "21 (see paper)"] <- 21 #removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "243-365 (fishbase)"] <- 304 #took average and removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "3215 - 4854.5"] <- 4034.5 #took average
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "36.5-182"] <- 109.25 #took average
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "39-148"] <- 93.5 #took average
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "520 (Smith et al. 1996)"] <- 520 #removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "730 (Bauman and Metter, 1977)"] <- 730 #removed citation
all_dt_accepted$gen_time[all_dt_accepted$gen_time == "XX"] <- 3285 #European hake, from fishbase

#check correct
gen <- sort(unique(all_dt_accepted$gen_time)) #good

#### Check study design ####
#check for studies without study design
nosd_check <- all_dt_accepted[study_design == ""] #3 rows without study design

#get list of study design
st_design <- sort(unique(all_dt_accepted$study_design)) #only 2 options, good

#### Check type change ####
#check for studies without type change
notc_check <- all_dt_accepted[type_change == ""] #5 rows without type change

#get list of type change
type <- sort(unique(all_dt_accepted$type_change)) #4 options

#check rows with extra info
check <- all_dt_accepted[type_change == "natural, anthropogenic", ]
View(check)

#marking "natural, anthropogenic" and "anthorpogenic, natural" as both for now
#only in diversity spreadsheet so perhaps not applied across the sheets?
all_dt_accepted$type_change[all_dt_accepted$type_change == "anthorpogenic, natural"] <- "both" #changing to both for now
all_dt_accepted$type_change[all_dt_accepted$type_change == "natural, anthropogenic"] <- "both" #changing to both for now

#### Check driver of process ####
#check for studies without driver process (only in first column)
nodp_check <- all_dt_accepted[driver_process1 == ""] #16 rows without driver_process

#get list of driver_process
drive_process <- sort(unique(c(all_dt_accepted$driver_process1, all_dt_accepted$driver_process2, all_dt_accepted$driver_process3))) #9 options, good
                           
#### Check length of process ####
#check for studies without length process
nolp_check <- all_dt_accepted[length_process == ""] #18 rows without length_process

#get list of length_process
length_process <- sort(unique(all_dt_accepted$length_process)) #2 options, good

#### Check data type ####
#check for studies without data type
nodt_check <- all_dt_accepted[data_type == ""] #6 rows without data_type

#get list of data_type
data_type <- sort(unique(all_dt_accepted$data_type)) #6 options

#changing STR rows to microsats
all_dt_accepted$data_type[all_dt_accepted$data_type == "STR"] <- "microsat" #same thing

#### Check tissue type ####
#check for studies without tissue type
nott_check <- all_dt_accepted[tissue_type == ""] #25 rows without tissue type

#get list of tissue_type
tissue_type <- sort(unique(all_dt_accepted$tissue_type)) #32 options which is fine
#will need to separate this column out & perhaps condense later but good enough now

#### Check preservation method ####
#check for studies without preservation method
nopm_check <- all_dt_accepted[preserv_method == ""] #45 rows without preservation method

#get list of preserv_method
preserv_method <- sort(unique(all_dt_accepted$preserv_method)) #9 options, good
#will need to separate this column out later but good enough for now

#### Check extraction method ####
#check for studies without extraction method
noem_check <- all_dt_accepted[extract_method == ""] #17 rows without extraction method

#get list of extract_method
extract_method <- sort(unique(all_dt_accepted$extract_method)) #76 options
#definitely needs to be condensed, will work on later

#### Check sequence platform ####
#check for  studies without sequence platform
nosp_check <- all_dt_accepted[seq_platform == ""] #28 rows without sequence platform

#get list of seq_platform
seq_platform <- sort(unique(all_dt_accepted$seq_platform)) #28 options
#definitely needs to be condensed, will work on later

#check rows with extra info
check <- all_dt_accepted[seq_platform == "SNaPshot Multiplex SNP assay", ]
View(check)

#fix seq platform as necessary
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "ABI 3130 automated sequencer"] <- "Sanger" #ABI 3130 is a 16-capillary electrophoresis sequencer (aka Sanger)
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "ABI 3730 capillary sequencer.BEckman CEQ2000XL"] <- "Sanger" #ABI 3730 is a 96-capillary electrophoresis sequencer (aka Sanger)
  all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Applied Biosystems 3730xl DNA Analyzer"] <- "Sanger"
  all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "ABI 3730 DNA automated sequencer"] <- "Sanger"
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "BioMark HD System)"] <- "Fluidigm BioMark HD" #per paper -- (Fluidigm Biomark 96.96 Dynamic Array) often used for single-cell genotyping/gene expression
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "First BASE"] <- "Sanger" #First BASE is sequencing facility in Malaysia that uses ABI sequencers (aka Sanger)
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Fluid Biomark HD System"] <- "Fluidigm BioMark HD"
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Fluidigm EP1 instrumentation"] <- "Fluidigm EP1" #different than BioMark HD but still commonly for gene expression
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Hitachi SQ-5500"] <- "Sanger" #slab gel electrophoresis (more labor-intensive than capillary but still essentially Sanger)
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Illumina-BeadXPress SNP assay"] <- "Illumina_BeadXpress" #format to match notation of other Illumina platforms
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Illumina GoldenGate"] <- "Illumina_BeadXpress" #per paper, done at Roslin Institute (think mostGolden Gate assays there are with Illumina BeadXpress, not iScan...)
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Ion Protontm"] <- "Ion_Torrent" #typo & also Ion Proton is one of Ion Torrent's sequencers (other one is PGM)
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Li-Cor 4200 Global IR2"] <- "Sanger" #type of chain-termination & gel electrophoresis platform (Sanger)
  all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "LI-COR IR2 two-dye"] <- "Sanger"
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "MegaBACE1000"] <- "Sanger" #automated DNA capillary sequencer
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "MJ Basestation Genetic Analyzer"] <- "Sanger" #automated DNA capillary sequencer
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "Pharmacia ALFexpresss automatic sequencer"] <- "Sanger" #type of gel electorphoresis sequencer
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "SEQ 8000 Automatic DNA Analyser"] <- "Sanger" #automated DNA capillary sequencer
all_dt_accepted$seq_platform[all_dt_accepted$seq_platform == "SNaPshot Multiplex SNP assay"] <- "Sanger" #SNP assay but designed to be run on any ABI (capillary/Sanger sequencer) system, which would be the sequencing platform
 
#### Check library prep method ####
#check for studies without lib prep
nolp_check <- all_dt_accepted[lib_prep_method == ""] #16 rows without lib prep

#get list of lib_prep
lib_prep <- sort(unique(all_dt_accepted$lib_prep_method)) #8 options --> too many

#check rows with extra info
check <- all_dt_accepted[lib_prep_method == "RAPTURE", ]
View(check)

#fix lib prep when necessary
all_dt_accepted$lib_prep_method[all_dt_accepted$lib_prep_method == "Meyer & Kircher 2010"] <- "Targeted_sequence_capture" #Meyer & Kircher 2010 is targeted sequence capture (exon)
all_dt_accepted$lib_prep_method[all_dt_accepted$lib_prep_method == "RAPTURE"] <- "Targeted_sequence_capture" #RAPTURE == targeted sequence capture (of RAD sites)
