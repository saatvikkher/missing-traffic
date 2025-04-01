source("outcome_test.R")
library(dplyr)
library(purrr)
library(stringr)
library(furrr)

num_counties <- function(x) {
  length(unique(x))
}

disparity_estimate_table <- function(p_ate, name) {
  p_ate %>%
    filter(white != 0 & black != 0) %>%
    mutate(disparity_base = black - white,
           disparity_imp = p_black - p_white) %>%
    group_by(county_name) %>%
    summarise(num_counties = num_counties(.),
              disparity_base = mean(disparity_base),
              could_change = sum(disparity_base > 0 & disparity_imp < 0) + sum(disparity_base < 0 & disparity_imp > 0),
              median_disparity = median(disparity_imp)
    ) %>%
    filter(disparity_base != 0) %>%
    summarise(
      naive_sign_neg = sum(disparity_base < 0),
      naive_sign_pos = sum(disparity_base > 0),
      could_switch_neg_to_pos = sum(disparity_base < 0 & could_change > 0),
      could_switch_pos_to_neg = sum(disparity_base > 0 & could_change > 0),
      cannot_switch_neg = sum(disparity_base <= 0 & could_change <= 0),
      cannot_switch_pos = sum(disparity_base >= 0 & could_change <= 0),
      median_disparity = sum(median_disparity < disparity_base)
    ) %>%
    mutate(dataset_name = name,
           num_counties = naive_sign_neg+naive_sign_pos, .before = naive_sign_neg) %>%
    return()
}


# Set up parallel processing plan
plan(multisession)  # Use multisession for Windows/macOS; on Linux, plan(multicore) may be preferred

states <- c("OH", "CO", "WI", "WA", "MD")

read_and_transform <- function(state) {
  file_path <- sprintf("data/%s_statewide_2020_04_01.rds", tolower(state))
  df <- readRDS(file_path)
  
  if (state == "OH") {
    df <- df %>%
      mutate(contraband_found = case_when(
        search_conducted & is.na(contraband_found) ~ FALSE,
        TRUE ~ contraband_found
      ))
  }
  
  if (state == "MD") {
    df <- df %>%
      mutate(group_name = extract_first_word(department_name)) %>% 
      group_by(group_name) %>%
      mutate(county_name = first(department_name)) %>%
      ungroup()
  }
  
  df
}

# Read and transform each state's data in parallel
data_list <- set_names(future_map(states, read_and_transform), states)

# Calculate disparity for each state in parallel
disparities <- future_map(data_list, calculate_disparity)

# Build a combined disparity estimate table
# disparity_table <- bind_rows(
#   map2(disparities, names(disparities), disparity_estimate_table)
# )
# print(disparity_table)

# Calculate the number of counties for each state in parallel
county_summary <- bind_rows(
  map2_df(data_list, names(data_list), ~ .x %>%
            summarise(num_counties = num_counties(county_name)) %>%
            mutate(state = .y))
)
print(county_summary)



