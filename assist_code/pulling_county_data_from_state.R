### Code to pull cases by county down from State of Michigan website

library(tidyverse)
library(rvest)
library(openxlsx)

# set where you'd like to download the file to: 
output_filepath <- "/Users/juliegil/Documents/git_synced_code/misapphire_share/data/"
# set the file name of what you're downloading:
save_file_name <- "cases_deaths_by_county_state_of_michigan_20220420.xlsx"

# read the html of the state website: 
state_page <- read_html("https://www.michigan.gov/coronavirus/stats")

# get a list of all the html a nodes, where the urls are kept
state_a <- html_nodes(state_page, "a")

# find the a node that has the phrase we care about
phrase_of_interest <- "Onset of Symptoms"

for (i in seq(1,length(state_a))){
  
  if (grepl(phrase_of_interest, as.character(state_a[[i]]))){
    keep_url <- as.character(state_a[[i]])
  } 
  
}

# get the portion of the url from the a node that we care about, to make the data pull
i <- gsub("\\", "", as.character(keep_url), fixed = TRUE)
get_doc_url <- strsplit(i, ".xlsx")[[1]][1]
get_doc_url <- strsplit(get_doc_url, '.href=\"')[[1]][2]
doc_url <- substr(get_doc_url, 2, nchar(get_doc_url))

url <- paste0("https://www.michigan.gov/", doc_url, ".xlsx")
download.file(url, paste0(output_filepath, save_file_name), mode = "wb")

################################################################################

# this downloads an excel file, but we want a csv file

# read in the excel file you just created
excel_in <- read.xlsx(paste0(output_filepath, save_file_name), sheet = 1, detectDates = TRUE)

# write out the data into a csv instead
new_file_name_and_loc <- gsub(".xlsx", ".csv", paste0(output_filepath, save_file_name))
write.csv(excel_in, new_file_name_and_loc, row.names = FALSE, na = "")

# and delete the excel version
file.remove(paste0(output_filepath, save_file_name))
