`Original_Excel/Secondary_Filtered_Database.xlsx` contains the full list of studies that were originally included in the literature search, as well as their initial "accept/reject" status and assigned subject categories during the first round of title & abstract filtering.

All other files contains source & raw data read into R scripts for cleaning.


Raw data (*.xlsx* files) are in the sub-directory **Original_Excel**. These files are where data were directly recorded. Source data read into R for data cleaning are in *.csv* files.
  * NOTE: In these spreadsheets, studies are assigned to rows. If a study had more than one unique study design (e.g., looked at multiple kinds of markers or organisms) each unique study design was given an additional row. Thus, one study could have more than one row.

The only difference between the raw and source data files are as follows:
1. Source data contains only the first 43 columns.
2. Source data contains only the rows with empirical studies where the **subject_1** column is the same subject as name of the source data file (e.g. "adaptation" for `adaptation.csv`).


All files (except `Secondary_Filtered_Database.xlsx` contain the following information in each column (for more information, look at `Filtering_Steps\Database_Guidelines.docx`):
1. **Article Number:** The number of the article in the `Secondary_filtered_Database.xlsx`
2. **Second_Filter_Flag:** A flag indicating the order in which the study was reviewed during title/abstract filtering. 'Review later' means the study was originally flagged as likely being outside of the scope of our study and was in the last set of articles to be filtered by hand.
3. **Article_Title:** The title of the study.
4. **Authors:** The authors of the study.
5: **Abstract:** The abstract of the study.
6: **Email_Addresses:** The email address(es) of the corresponding author of the study.
7. **Publication_Year:** The year the study was published in.
8. **DOI:** The DOI for the study.
9. **Source_Title:** The title of the journal the study was published in.
10. **Keyword_Search:** The keyword search that initially flagged the study and added it to our database.
11. **Reviewer:** The initials of the person who conducted the title/abstract filtering.
12. **Recorder:** The intials of the person who recorded all the subsequent data for the study.
13. **Decision:** Whether or not the study was accepted or rejected from our final database.
14. **Removal_Criteria:** If the study was rejected, the reason for the rejection.
15. **study_type:** The article type (e.g., empirical, review, theoretical). Data was only recorded from empirical studies.
16. **subject:** The subject(s) of the study (adaptation, connectivity, popsize, diversity). The most predominant subject was listed in subject_1, with all other subjects the study covered listed in subsequent columns (through column 19).
20. **hybridization:** Whether or not the study looked at temporal trends in hybridization.
21. **system:** The system of the study (marine, terrestrial, freshwater, other).
22. **tax_group:** The taxonomic group the study looked at (recorded at the class level).
23. **country_samp:** The countries from which samples included in the study came from. Multiple countries separated by commas.
24. **loc_samp:** The locations from which samples included in the study came from. Multiple locations separated by commas.
25. **year_samp:** The years from which samples included in the study came from. Multiple years separated by commas. Years recorded in YYYY.MM.DD format.
26. **num_samp:** The number of samples for each time point included in the study. Time points separated by commas, sampling locations within a year separated by periods. Order matches that of the year_samp column (e.g., If 2007.01.01 is the first year in year_samp, and 40 is the first number in num_samp, then there were 40 samples from 2007 in the study).
27. **num_timepoints:** The number of unique time points included in the study. If more than one study design was present in the study (e.g., some time points were combined for some analyses, or some populations had fewer time points), each study design is included, separated by a comma.
28. **earliest_timepoint:** The earliest time point included in the analysis.
29. **latest_timepoint:** The latest time point included in the analysis. This, combined with column 28, indicates the maximum period over which genetic change was observed.
30. **gen_time:** The generation time (in days) for the organism included in the study.
31. **study_design:** Whether the study used museum/archived samples (opportunistic) or used samples that were all collected by the authors (predesigned).
32. **type_change:** Whether the study looked at genomic change from natural or anthropogenic forces (or both).
33. **driver_process:**The driver(s) of change analyzed in study. The most predominant driver was listed in driver_1, with all other drivers the study covered listed in subsequent columns (through column 35).
36. **length_process:** Whether the study looked at change from a driver that occured over acute time periods (0-10 generations) or chronic time periods (>10 generations).
37. **data_type:** The type of genetic marker used in the study.
38. **tissue_type:** The type of tissue(s) DNA was extracted from in the study.
39. **preserv_method:** The type(s) of preservation methods used in the study.
40. **extract_method:** The type(s) of extraction methods used in the study.
41. **seq_platform:** The sequencing platform used in the study.
42. **lib_prep_method:** THe library preparation method used in th study.
43. **notes:** Any notes made by the reviewer/recorder during the data collection process.
