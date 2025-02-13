source("outcome_test.R")

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

disparity_estimates(calculate_disparity(readRDS("data/oh_statewide_2020_04_01.rds")), "OH") %>%
  bind_rows(
    disparity_estimates(calculate_disparity(readRDS("data/co_statewide_2020_04_01.rds")), "CO"),
    disparity_estimates(calculate_disparity(readRDS("data/wi_statewide_2020_04_01.rds")), "WI"),
    disparity_estimates(calculate_disparity(readRDS("data/wa_statewide_2020_04_01.rds")), "WA"),
    disparity_estimates(calculate_disparity(readRDS("data/md_statewide_2020_04_01.rds")), "MD")
  ) %>%
  print()
