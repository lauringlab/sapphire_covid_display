### Data Files

* cases_deaths_by_county_state_of_michigan_20220420.csv - Downloaded from https://www.michigan.gov/coronavirus/stats on 2022-04-26

* county_map_bones.rds - shape file information on Michigan county borders

* cb_2018_us_zcta510_500k - shape file information on U.S. zip code borders

* mi_zip_list.csv - list of zip codes in Michigan

* patient_data_sample.csv - Sample practice dataset with no real patient information; in order to use your own data within this Shiny application, you should have a dataset with the following columns, in no particular order:

| Columns | Description |
| --- | --- |
| PATIENT_RACE | Race of the patient, indicated by a one letter abbreviation |
| SPECIMEN_COLLECTION_DATE | Date the sample was collected from the individual. Format = YYYYMMDD |
| PATIENT_COUNTY | In capital letters, the county the individual tested resides in |
| PATIENT_MRN | A unique identifier for each individual |
| collection_week | The year and week of the date specified in SPECIMEN_COLLECTION_DATE. Format = YYYY-WW |
| RESULT | The lineage detected from the sample collected from the individual. Ex. B.1.1.7, AY.3 |
| extra_clade | The Greek letter name for the lineage indicated in RESULT. Ex. Delta, Omicron |
| PATIENT_SEX | The sex of the individual, indicated as either M or F |
| APPROX_AGE | The age of the individual in years |
