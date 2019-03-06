library(here)
library(tidyverse)
library(officer)
library(rvg)


# dat_sav_member <- 'data/FFA_EXPANSION_BASELINE_2017_2018_10_02_04_24_12_692999/B1_hh_member.sav' %>%
#   here::here() %>%
#   read_sav()
# 
# q_baseline <- 'data/FFA_EXPANSION_BASELINE_2017_2018_10_02_04_24_12_692999/FFA_BASELINE_SURVEY_2017v7.sav' %>%
#   here::here() %>%
#   read_sav()
# 
# dat_member <- dat_sav_member %>%
#   as_factor()
# 
# ?zap_labels()
# 
# x <- q_baseline %>%
#   # nest by question
#   gather(key = 'question', value = 'answer') %>%
#   group_by(question) %>% nest() %>%
#   # append variable type
#   mutate(col_type = map_chr(.x = q_baseline, ~class(.x)[[1]]))
# 
# 
# 
# q_baseline %>%
#   ggplot(aes(x = A15_total_persons)) +
#   geom_histogram(stat = 'bin', binwidth = 1)
# 
# q_baseline %>%
#   mutate(
#     A15_total_persons = A15_total_persons %>%
#       as.numeric() %>% na_if(402009)) %>%
#   ggplot(aes(x = A15_total_persons)) +
#   geom_histogram(binwidth = 1)



# read the data
file_name <- "data/FFA_BASELINE.csv"
col_names <- file_name %>%
  read_csv(n_max = 1)
data <- file_name %>%
  read_csv(skip = 2, col_names = colnames(col_names), guess_max = 1e6)

# generate metadata (name, label, coltype)
metadata <- col_names %>%
  gather(key = "name", value = "label") %>%
  bind_cols("type" = map_chr(.x = data, .f = ~ class(.x)[[1]]))

# have a look
glimpse(data)

# personal identifiable information
vars_pii <- c(
  "deviceid", "subscriberid", "simserial", "phonenumber",
  "A5_Household_id", "A6_HHld_Cell_phone_number", "A7_Alt_cell_number",
  "A8_HHH_name", "A9_primary_resp_name",
  # these are data collection variables, could test them later
  "A12_enumerator_name", "A13_supervisor", "A14_enumerator_phone_number"
)
# details on form filling etc
vars_form <- c(
  "start", "end", "today", "A11_date_first_visit",
  "coordinates", "meta/instanceID",
  colnames(data) %>% str_subset("^_")
)
# columns with ordinal information
vars_ordinal <- c(
  "C8_time_collect_water", "agri_far_cereal_seed_local_OPV", "rainfall"
)
# columns with additional specification to previous answer
vars_detail <- colnames(data) %>% str_subset("_specify$")
# columns that specify units of previous answer
vars_units <- colnames(data) %>% str_subset("_unit$")
# columns that specify quantity for previous answer
vars_quantity <- colnames(data) %>% str_subset("_how_many$")
# sometimes there is additional "other" fields
vars_other <- colnames(data) %>% str_subset(".*_other")

# subset to exclude variables
df <- data %>%
  # no pii
  select(-one_of(vars_pii)) %>%
  # no form info for now
  select(-one_of(vars_form)) %>%
  # other information is messy
  select(-one_of(vars_other)) %>%
  # specify information is messy, leaving it out for now
  select(-one_of(vars_detail)) %>%
  # units are messy, leaving them out for now
  select(-one_of(vars_units))



vars_simple <- c(
  "live_cattle", "live_goats", "live_pigs", "live_sheep", "live_donkeys",
  "live_chickens", "live_guinea_fowls", "live_ducks", "live_pigeons", 
  "live_other", "live_khola_shelter", "live_fish_pond", "live_beekeeping",
  "shock_first", "shock_second", "shock_third", "rainfall"
)


# plotting for single choice
plot_single_choice <- function(x, var_name) {
  ggp <- x %>%
    as_factor() %>%
    data_frame("x" = .) %>%
    ggplot(aes(x = x)) +
    geom_bar(colour = "black", fill = "#0093d8") +
    theme_minimal() +
    ggtitle(label = var_name, subtitle = "Single Choice Question") +
    xlab("") + ylab("Number of respondents")
  return(ggp)
}

# plotting for numeric
plot_numeric <- function(x, var_name) {
  n_values <- x %>% unique %>% length()
  n_bins <- ifelse(n_values < 30, n_values, 30)
  ggp <- x %>%
    data_frame("x" = .) %>%
    ggplot(aes(x = x)) +
    geom_histogram(bins = n_bins, colour = "black", fill = "#0093d8") +
    theme_minimal() +
    ggtitle(label = var_name, subtitle = "Numeric Answer") +
    xlab("") + ylab("Number of respondents in value range")
  return(ggp)
}


# creating plots

# single choice example
data %>%
  select(A10_household_type) %>%
  plot_single_choice()

# numeric value example
data %>%
  select(A15_total_persons) %>%
  # TODO: remove outliers
  filter(A15_total_persons < 30) %>%
  plot_numeric()

# create plot objects
plots <- map2(
  .x = df,
  .y = colnames(df),
  .f = function(x, y) {
    if (class(x) != "character") {
      plot_numeric(x, y)
    } else {
      plot_single_choice(x, y)
    }
  }
)


# Save plots -------------------------------------------------------------------

# Add a new slide into the ppt document 
write_plot <- function(doc, plot_obj) {
  doc <- add_slide(doc, 'Title and Content', 'Office Theme')
  doc <- ph_with_vg(doc, ggobj = plot_obj, type = 'body')
  return(doc)
}

# Create a new powerpoint document
doc <- read_pptx()
map(.x = plots, .f = write_plot, doc = doc)
# write the document to a file
print(doc, target = here::here('pp_with_plots.pptx'))

