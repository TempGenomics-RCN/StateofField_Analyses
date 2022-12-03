Contains files read into R scripts for data visualization. *Files generated in R scripts not included in directory, with 1 exception.*

Data unrelated to specific scripts:
  * **Country_spreadsheet.xlsx:** Excel spreadsheet with information on 1) countries from which samples came from for all studies included in the literature review, 2) whether those studies included museum samples or not (and if so, the country in which those samples are housed), and 3) whether those studies had an author from the originally sampled country.

Data for R scripts:
  * **all_temp_gen_data.csv:** Cleaned and combined output from `assemble_data_SoF.R`. Read into `data_exploration.R` and `manuscript_figures.R`.
  * **country_samp_df.csv:** Dataset with list of all the countries in the world and the number of studies in the literature review database with museum samples originating from each country. Read into `manuscript_figures.R`.
  * **country_house_df.csv:** Dataset with list of all the countries in the world and the number of studies in the literature review database with museum samples housed in each country. Read into `manuscript_figures.R`.
  * **country_predesigned_df.csv:** Dataset with list of all the countries in the world and the number of studies in the literature review database with non-museum samples (e.g. those where all samples/timepoints were collected by the authors) originating from each country. Read into `manuscript_figures.R`.
  
