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

bio_df = excel_sheets(here('analysis/data/raw_data/YakimaNation/PRCTAGLISTALLYEARS.xlsx')) %>%
  as.list() %>%
  extract(1:2) %>%
  rlang::set_names() %>%
  map_df(.id = 'year',
         .f = function(x) {
           data_df = read_excel(here('analysis/data/raw_data/YakimaNation/PRCTAGLISTALLYEARS.xlsx'),
                                x) %>%
             clean_names()
           return(data_df)
         }) %>%
  rename(tag_code = pit_tag,
         trap_date = date) %>%
  mutate(year = year(trap_date)) %>%
  select(year, tag_code, trap_date, sex, dna)


# any duplicated tags?
bio_df %>%
  filter(tag_code %in% tag_code[duplicated(tag_code)]) %>%
  arrange(year, tag_code, trap_date) %>%
  tabyl(year)

# dna sample associated with more than one tag?
bio_df %>%
  group_by(year) %>%
  filter(dna %in% dna[duplicated(dna)]) %>%
  ungroup() %>%
  arrange(dna, year, tag_code) %>%
  tabyl(year)

# clearly some duplicates in 2019. Not sure why, but we'll ignore the dna number

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
# all years
for(yr in names(tag_list)) {
  write_delim(tag_list[[yr]],
              file = here('analysis/data/raw_data/tag_lists',
                          paste0('UC_Coho_Tags_', yr, '.txt')),
              delim = '\n',
              col_names = F)
}

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


