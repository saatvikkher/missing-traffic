library(tidyverse)

extract_first_word <- function(text) {
  space_pos <- str_locate(text, " ")[, 1]  # Extract start position of first space
  ifelse(is.na(space_pos), text, str_sub(text, 1, space_pos - 1))  # Handle cases with no space
}

hit_rate_summary <- function(dataset) {
  dataset %>%
    filter(subject_race %in% c("white", "black") | is.na(subject_race)) %>%
    filter(search_conducted) %>%
    group_by(subject_race, county_name) %>%
    summarize(
      hit_rate = mean(contraband_found, na.rm = T)
    ) %>%
    pivot_wider(names_from=subject_race, values_from = hit_rate)
}

calculate_ate_from_tibble <- function(data) {
  # Extract counts for black, white, and NA
  black_false <- data %>% 
    filter(subject_race == "black", contraband_found == FALSE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  black_true <- data %>% 
    filter(subject_race == "black", contraband_found == TRUE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  white_false <- data %>% 
    filter(subject_race == "white", contraband_found == FALSE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  white_true <- data %>% 
    filter(subject_race == "white", contraband_found == TRUE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  na_false <- data %>% 
    filter(is.na(subject_race), contraband_found == FALSE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  na_true <- data %>% 
    filter(is.na(subject_race), contraband_found == TRUE) %>%
    pull(n) %>% 
    {if (length(.) == 0) 0 else .}
  
  
  # Create a grid of possible NA assignments to black and white
  results <- expand.grid(na_false_black = 0:na_false,
                         na_true_black = 0:na_true)
  
  # Function to calculate ATE for a given assignment of NA values
  calculate_p_black <- function(na_false_black, na_true_black) {
    # Black assignments
    black_false_total <- black_false + na_false_black
    black_true_total <- black_true + na_true_black
    
    # White assignments
    white_false_total <- white_false + (na_false - na_false_black)
    white_true_total <- white_true + (na_true - na_true_black)
    
    # Probabilities of contraband found
    p_black <- black_true_total / (black_true_total + black_false_total)
    p_white <- white_true_total / (white_true_total + white_false_total)
    
    return(p_black)
  }
  
  # Function to calculate ATE for a given assignment of NA values
  calculate_p_white <- function(na_false_black, na_true_black) {
    # Black assignments
    black_false_total <- black_false + na_false_black
    black_true_total <- black_true + na_true_black
    
    # White assignments
    white_false_total <- white_false + (na_false - na_false_black)
    white_true_total <- white_true + (na_true - na_true_black)
    
    # Probabilities of contraband found
    p_black <- black_true_total / (black_true_total + black_false_total)
    p_white <- white_true_total / (white_true_total + white_false_total)
    
    return(p_white)
  }
  
  
  results$p_white <- mapply(calculate_p_white, 
                            results$na_false_black, 
                            results$na_true_black)
  
  results$p_black <- mapply(calculate_p_black, 
                            results$na_false_black, 
                            results$na_true_black)
  
  
  
  results <- results %>% mutate(prop_assigned_white = ((na_false + na_true) - (na_true_black + na_false_black))/(na_false + na_true))
  
  # Return the results with all permutations and ATEs
  return(results)
}

calculate_base_data <- function(dataset) {
  
  base <- dataset %>%
    filter(subject_race %in% c("white", "black")) %>%
    filter(search_conducted) %>% 
    group_by(subject_race, county_name) %>% 
    summarize(
      hit_rate = mean(contraband_found, na.rm = T)
    ) %>%
    pivot_wider(names_from=subject_race, values_from = hit_rate)
  
  
  blackimp <- dataset %>%
    filter(subject_race %in% c("white", "black") | is.na(subject_race)) %>%
    filter(search_conducted) %>% 
    group_by(subject_race, county_name) %>%
    mutate(subject_race = case_when(is.na(subject_race) ~ "black",
                                    TRUE ~ subject_race)) %>%
    summarize(
      hit_rate = mean(contraband_found, na.rm = T)
    ) %>%
    pivot_wider(names_from=subject_race, values_from = hit_rate) %>%
    rename(black_hit_rate_blackimp = black, white_hit_rate_blackimp = white)
  
  whiteimp <- dataset %>%
    filter(subject_race %in% c("white", "black") | is.na(subject_race)) %>%
    filter(search_conducted) %>% 
    group_by(subject_race, county_name) %>%
    mutate(subject_race = case_when(is.na(subject_race) ~ "white",
                                    TRUE ~ subject_race)) %>%
    summarize(
      hit_rate = mean(contraband_found, na.rm = T)
    ) %>%
    pivot_wider(names_from=subject_race, values_from = hit_rate) %>%
    rename(black_hit_rate_whiteimp = black, white_hit_rate_whiteimp = white)
  
  base_data <- full_join(base, blackimp, by="county_name") %>%
    full_join(whiteimp)
  
  return(base_data)
}

calculate_disparity <- function(dataset) {
  ate_results <- tibble()
  
  for (county in unique(dataset$county_name)) {
    
    dataset %>%
      filter(county_name == county) %>%
      filter(search_conducted) %>%
      filter(subject_race %in% c("white", "black") | is.na(subject_race)) %>%
      count(subject_race, contraband_found) %>%
      calculate_ate_from_tibble() %>%
      mutate(county_name = county) %>%
      bind_rows(ate_results) -> ate_results
  }
  
  sizes <- dataset %>% filter(
    search_conducted, 
    subject_race %in% c("black", "white")) %>%  
    count(county_name)
  
  base_data <- calculate_base_data(dataset)
  
  ate_results %>%
    filter(!is.na(county_name)) %>%
    left_join(sizes, by = "county_name") %>%
    left_join(base_data, by = "county_name") %>%
    group_by(county_name) %>%
    filter(!(any(is.na(black) | is.na(white)))) %>%
    ungroup() %>%
    # remove counties with no black or white searches
    filter(white != 0 & black != 0) %>%
    return()
  
}
