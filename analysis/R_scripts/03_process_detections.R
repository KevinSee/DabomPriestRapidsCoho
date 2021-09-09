# Author: Kevin See
# Purpose: clean PTAGIS data with PITcleanr
# Created: 4/27/20
# Last Modified: 9/8/21
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(PITcleanr)
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(here)

#-----------------------------------------------------------------
# load configuration and site_df data
load(here('analysis/data/derived_data/site_config.rda'))

# which spawn year are we dealing with?
yr = 2019

# load and file biological data
bio_df = read_rds(here('analysis/data/derived_data/Bio_Data_2019_2020.rds')) %>%
  filter(year == yr)

# any double-tagged fish?
# dbl_tag = bio_df %>%
#   filter(!is.na(tag_other))


#-----------------------------------------------------------------
# start date is June 1 of year
start_date = paste0(yr, '0601')
# when is the last possible observation date?
max_obs_date = paste0(yr+1, "0531")

# get raw observations from PTAGIS
# These come from running a saved query on the list of tags to be used
ptagis_file = here("analysis/data/raw_data/PTAGIS",
                   paste0("UC_Coho_", yr, "_CTH.csv"))

# recode the PTAGIS observations of double tagged fish so that the tag code matches the TagID (not TagOther)
ptagis_obs = readCTH(ptagis_file)

# if(nrow(dbl_tag) > 0) {
#   ptagis_obs %<>%
#     left_join(dbl_tag %>%
#                 mutate(fish_id = 1:n()) %>%
#                 select(fish_id, starts_with("tag")) %>%
#                 mutate(use_tag = tag_code) %>%
#                 pivot_longer(cols = starts_with("tag"),
#                              names_to = "source",
#                              values_to = "tag_code") %>%
#                 select(tag_code, use_tag),
#               by = "tag_code") %>%
#     rowwise() %>%
#     mutate(tag_code = if_else(!is.na(use_tag),
#                               use_tag,
#                               tag_code)) %>%
#     ungroup() %>%
#     select(-use_tag) %>%
#     distinct()
# }

# any orphaned or disowned tags?
qc_cth = qcTagHistory(ptagis_obs, T)

# single orphan tag in 2019; also single orphan tag in 2020

# compress and process those observations with PITcleanr
prepped_ch = PITcleanr::prepWrapper(ptagis_file = ptagis_obs,
                                    configuration = configuration,
                                    parent_child = parent_child %>%
                                      addParentChildNodes(configuration = configuration),
                                    min_obs_date = start_date,
                                    max_obs_date = max_obs_date,
                                    ignore_event_vs_release = T,
                                    save_file = F,
                                    file_name = here('analysis/outgoing/PITcleanr', paste0('UC_Coho_', yr, '.xlsx')))

# save some stuff
save(parent_child, configuration, start_date, bio_df, qc_cth, prepped_ch,
     file = here('analysis/data/derived_data/PITcleanr',
                 paste0('UC_Coho_', yr, '.rda')))


# rm(start_date, bio_df, prepped_ch,
#    ptagis_file,
#    ptagis_obs,
#    dbl_tag)
# }

#-------------------------------------------
# NEXT STEPS
#-------------------------------------------
# open that Excel file, and filter on the column user_keep_obs, looking for blanks. Fill in each row with TRUE or FALSE, depending on whether that observation should be kept or not. The column auto_keep_obs provides a suggestion, but the biologist's best expert judgment should be used based on detection dates, detection locations before and after, etc.

#-----------------------------------------------------------------
# Read in local biologist keep_obs
#-----------------------------------------------------------------
load(here('analysis/data/derived_data/PITcleanr',
          paste0('UC_Coho_', yr, '.rda')))

file_nm = if_else(yr == 2020,
                  "UC_Coho_2020_RA_FINAL.xlsx",
                  if_else(yr == 2019,
                          "UC_Coho_2019_KM_FINAL.xlsx",
                          NA_character_))

ch_df = read_excel(here('analysis/data/raw_data/YakimaNation',
                        file_nm))

identical(dim(prepped_ch), dim(ch_df))

ch_df %<>%
  # recode METHB0 as MSHB0
  mutate(node = recode(node,
                       "METHB0" = "MSHB0"))

# do some comparisons between the two prepped data.frames
comp_df = prepped_ch %>%
  left_join(ch_df %>%
              select(tag_code:min_det,
                     ends_with("keep_obs")) %>%
              rename(auto = auto_keep_obs,
                     user = user_keep_obs))


# need to fix some of the user_keep_obs that were entered
fix_list = list(no_prob_tags = comp_df %>%
                  select(-auto) %>%
                  anti_join(comp_df %>%
                              filter(auto_keep_obs != user) %>%
                              select(tag_code) %>%
                              distinct()),
                prob_tags = comp_df %>%
                  select(-auto) %>%
                  inner_join(comp_df %>%
                               filter(auto_keep_obs != user) %>%
                               select(tag_code) %>%
                               distinct()))

writexl::write_xlsx(fix_list,
                    path = here('analysis/data/derived_data/PITcleanr', paste0('UC_Coho_', yr, '_KS.xlsx')),
                    col_names = T,
                    format_headers = T)


# read back in the fixed file (based on my interpretation of the Yakama Nation's user_keep_obs)
ch_df = excel_sheets(here('analysis/data/derived_data/PITcleanr', paste0('UC_Coho_', yr, '_KS.xlsx'))) %>%
  as.list() %>%
  map_df(.f = function(x) {
    read_excel(path = here('analysis/data/derived_data/PITcleanr', paste0('UC_Coho_', yr, '_KS.xlsx')),
               sheet = x)
  }) %>%
  select(-user) %>%
  arrange(tag_code, slot)


ch_df %>%
  filter(auto_keep_obs != user_keep_obs) %>%
  select(tag_code) %>%
  distinct() %>%
  slice(4) %>%
  # slice_sample(n = 1) %>%
  left_join(ch_df) %>%
  select(tag_code:max_det,
         direction:user_keep_obs)


# replace PITcleanr output with what was sent back by local biologist
prepped_ch = ch_df

save(parent_child, configuration, start_date, bio_df, qc_cth, prepped_ch,
     file = here('analysis/data/derived_data/PITcleanr',
                 paste0('UC_Coho_', yr, '.rda')))

#-----------------------------------------------------------------
# tag summaries
#-----------------------------------------------------------------
# use auto_keep_obs for the moment
tag_summ = summarizeTagData(prepped_ch %>%
                              mutate(user_keep_obs = auto_keep_obs),
                            bio_df %>%
                              group_by(tag_code) %>%
                              slice(1) %>%
                              ungroup())

# any duplicated tags?
sum(duplicated(tag_summ$tag_code))
tag_summ %>%
  filter(tag_code %in% tag_code[duplicated(tag_code)]) %>%
  as.data.frame()

# where are tags assigned?
janitor::tabyl(tag_summ, spawn_node) %>%
  # arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  janitor::adorn_pct_formatting()

# preliminary estimate of node efficiency
node_eff = prepped_ch %>%
  mutate(user_keep_obs = auto_keep_obs) %>%
  filter(user_keep_obs) %>%
  estNodeEff(node_order = buildNodeOrder(addParentChildNodes(parent_child, configuration)))

node_eff %>%
  filter(tags_at_node > 0,
         eff_est < 1)

#-----------------------------------------------------------------
# examine some of the output
#-----------------------------------------------------------------
# which tags have "strange" capture histories?
prepped_ch %>%
  summarise(n_tags = n_distinct(tag_code),
            n_weird = n_distinct(tag_code[direction == "unknown"]),
            n_fix = n_distinct(tag_code[is.na(user_keep_obs)]),
            prop_weird = n_weird / n_tags,
            prop_fix = n_fix / n_tags)

# look at which branch each tag was assigned to for spawning
brnch_df = buildNodeOrder(addParentChildNodes(parent_child, configuration)) %>%
  separate(col = path,
           into = paste("step", 1:max(.$node_order), sep = "_"),
           remove = F) %>%
  mutate(branch_nm = if_else(node == "PRA",
                             "Start",
                             if_else(grepl('LWE', path) | node %in% c("CLK"),
                                     "Wenatchee",
                                     if_else(grepl("ENL", path),
                                             "Entiat",
                                             if_else(grepl("LMR", path),
                                                     "Methow",
                                                     if_else(grepl("OKL", path) | node %in% c("FST"),
                                                             "Okanogan",
                                                             if_else(step_2 != "RIA" & !is.na(step_2),
                                                                     "Downstream",
                                                                     "Mainstem"))))))) %>%
  select(-starts_with("step"))

tag_summ %<>%
  left_join(brnch_df %>%
              select(spawn_node = node,
                     branch_nm))

# how many tags in each branch?
tag_summ %>%
  janitor::tabyl(branch_nm) %>%
  janitor::adorn_pct_formatting() %>%
  arrange(desc(n))

# sex comp in each branch
tag_summ %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = branch_nm,
             fill = as.ordered(sex))) +
  geom_bar(position = position_fill()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Branch",
       y = "Proportion of Tags",
       fill = "Sex")


# age comp in each branch, by sex
# tag_summ %>%
#   filter(!is.na(final_age)) %>%
#   ggplot(aes(x = branch_nm,
#              fill = as.ordered(final_age))) +
#   geom_bar(position = position_fill()) +
#   facet_wrap(~ sex) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45,
#                                    hjust = 1)) +
#   labs(x = "Branch",
#        y = "Percent of Tags",
#        fill = "Age")


# look at run timing between branches
tag_summ %>%
  ggplot(aes(x = trap_date,
             color = branch_nm,
             fill = branch_nm)) +
  geom_density(alpha = 0.1) +
  theme_bw() +
  scale_color_brewer(palette = 'Set1',
                     name = "Branch") +
  scale_fill_brewer(palette = 'Set1',
                    name = "Branch") +
  labs(x = "Trap Date at Priest Rapids")

# cumulative run timing by branch
tag_summ %>%
  select(branch_nm, trap_date) %>%
  arrange(branch_nm, trap_date) %>%
  group_by(branch_nm, trap_date) %>%
  summarise(n = n()) %>%
  mutate(n = cumsum(n)) %>%
  ungroup() %>%
  group_by(branch_nm) %>%
  mutate(prop = n / max(n)) %>%
  ungroup() %>%
  pivot_longer(cols = c(n, prop),
               names_to = "type",
               values_to = "value") %>%
  mutate(type = recode(type,
                       "n" = "Cumulative Number of Tags",
                       "prop" = "Cumulative Proportion of Tags")) %>%
  ggplot(aes(x = trap_date,
             y = value,
             color = branch_nm)) +
  geom_line(lwd = 1) +
  # geom_step(lwd = 1) +
  theme_bw() +
  facet_wrap(~ type,
             ncol = 1,
             scales = "free_y") +
  theme(legend.position = "bottom",
        axis.title.y = element_blank()) +
  scale_color_brewer(palette = 'Set1',
                     name = "Branch") +
  labs(x = "Trap Date at Priest Rapids",
       title = "Cumulative Run Timing")
