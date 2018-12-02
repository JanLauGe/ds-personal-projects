library(here)
library(tidyverse)
library(haven)

dat_sav_member <- 'data/FFA_EXPANSION_BASELINE_2017_2018_10_02_04_24_12_692999/B1_hh_member.sav' %>%
  here::here() %>%
  read_sav()

dat_member <- dat_sav_member %>%
  as_factor()

zap_labels()
