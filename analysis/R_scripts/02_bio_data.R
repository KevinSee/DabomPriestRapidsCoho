# Author: Kevin See
# Purpose: create tag lists to feed to PTAGIS query
# Created: 6/15/2021
# Last Modified: 6/15/2021
# Notes:

#-----------------------------------------------------------------
# load needed libraries
# library(PITcleanr)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(magrittr)
library(writexl)
library(here)

#-----------------------------------------------------------------
# read in biological data from trap
list.files(here('analysis/data/raw_data/YakimaNation'))

bio_df = excel_sheets(here('analysis/data/raw_data/YakimaNation/prc.xlsx')) %>%
  as.list() %>%
  rlang::set_names() %>%
  map_df(.id = 'trap_date',
         .f = function(x) {
           data_df = read_excel(here('analysis/data/raw_data/YakimaNation/prc.xlsx'),
                                x)
           return(data_df)
         }) %>%
  rename(tag_code = `PIT Tag`) %>%
  mutate(across(tag_code,
                str_remove,
                "^\\*")) %>%
  mutate(trap_date = mdy(paste(trap_date, "2020", sep = "."))) %>%
  mutate(year = year(trap_date)) %>%
  select(year, tag_code, everything())


# any duplicated tags?
bio_df %>%
  filter(tag_code %in% tag_code[duplicated(tag_code)]) %>%
  arrange(year, tag_code, trap_date) %>%
  tabyl(year)

# fish with more than one tag?
bio_df %>%
  filter(dna %in% dna[duplicated(dna)]) %>%
  tabyl(year)

#-----------------------------------------------------------------
# save as Excel file
#-----------------------------------------------------------------
bio_df %>%
  split(list(.$year)) %>%
  write_xlsx(path = here('analysis/data/derived_data',
                         'PRA_Coho_BioData.xlsx'))

#-----------------------------------------------------------------
# for tag lists
#-----------------------------------------------------------------
# put bounds around years
min_yr = min(bio_df$year)
max_yr = max(bio_df$year)


# pull out PIT tag numbers
tag_list = bio_df %>%
  split(list(.$year)) %>%
  map(.f = function(x) {
    x %>%
      pivot_longer(cols = starts_with("tag"),
                   names_to = "source",
                   values_to = "tag_code") %>%
      filter(!is.na(tag_code)) %>%
      select(tag_code) %>%
      distinct()
  })

# save tags to upload to PTAGIS

# for(yr in names(tag_list)) {
#   write_delim(tag_list[[yr]],
#               file = here('analysis/data/raw_data/tag_lists',
#                           paste0('UC_Coho_Tags_', yr, '.txt')),
#               delim = '\n',
#               col_names = F)
# }

# just write the latest year
write_delim(tag_list[[as.character(max_yr)]],
            file = here('analysis/data/raw_data/tag_lists',
                        paste0('UC_Coho_Tags_', max_yr, '.txt')),
            delim = '\n',
            col_names = F)

# save biological data for later
write_rds(bio_df,
          file = here('analysis/data/derived_data',
                      paste0('Bio_Data_', min_yr, '_', max_yr, '.rds')))


