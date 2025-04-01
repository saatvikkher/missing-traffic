options(warn=-1)
library(tidyverse)
library(ggridges)
library(furrr)
source("outcome_test.R")  # This should load calculate_disparity, outcome_test_plot, outcome_test_plot_base, extract_first_word


outcome_test_plot <- function(disparity_results) {
  disparity_results %>%
    ggplot() +
    geom_jitter(aes(y = reorder(county_name, -(black-white)), 
                    x = (p_black-p_white),
                    color=prop_assigned_white), alpha=0.2) +
    geom_boxplot(aes(y = reorder(county_name, -(black-white)), 
                     x = (p_black-p_white)), alpha=0.2) +
    geom_point(
      aes(y = reorder(county_name, -(black-white)), 
          x = (black-white), 
          size = n),
      pch = 21
    ) +
    geom_point(aes(y=county_name, x=0, color=prop_assigned_white), size=0) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_point_color_gradient(high = "yellow", low = "purple", name = "Proportion NA\nassigned to White") +
    scale_color_gradient(low = "yellow", high = "purple", name = "Proportion NA\nassigned to White") +
    theme_ridges(center_axis_labels = TRUE) +
    # Need to add color gradient legend
    xlab("Disparity") +
    ylab("County Name") +
    guides(size = "none", color = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
    theme(legend.direction = "vertical",
          legend.box = "vertical",
          legend.title = element_text(vjust = 1),
          legend.spacing.x = unit(0.2, 'cm')) %>%
    return()
}
outcome_test_plot_base <- function(disparity_results){
  disparity_results %>%
    ggplot() +
    geom_point(
      aes(y = reorder(county_name, -(black-white)), 
          x = (black-white), 
          size = n),
      pch = 21
    ) +
    geom_vline(xintercept = 0, linetype = 2) +
    theme_ridges(center_axis_labels = TRUE) +
    # Need to add color gradient legend
    xlab("Disparity") +
    ylab("County Name") +
    guides(size = "none")
}

# Set up a parallel plan (adjust as needed for your OS)
plan(multisession)

process_state <- function(state) {
  # Construct file paths
  file_path <- sprintf("data/%s_statewide_2020_04_01.rds", tolower(state))
  dataset <- readRDS(file_path)
  
  # State-specific data transformation
  if (state == "OH") {
    dataset <- dataset %>%
      mutate(contraband_found = case_when(
        search_conducted & is.na(contraband_found) ~ FALSE,
        TRUE ~ contraband_found
      ))
  }
  if (state == "MD") {
    dataset <- dataset %>%
      mutate(group_name = extract_first_word(department_name)) %>% 
      group_by(group_name) %>%
      mutate(county_name = first(department_name)) %>%
      ungroup()
  }
  
  # Calculate disparity
  disparity <- calculate_disparity(dataset)
  
  # Generate the main outcome test plot using the custom function
  p <- outcome_test_plot(disparity)
  
  # Apply state-specific theme modifications and save the "all counties" plot
  if (state == "OH") {
    p_all <- p +
      xlim(-0.24, 0.6) +
      theme(axis.title = element_text(size = 13),
            axis.text = element_text(size = 12),
            legend.position = c(0.6, 0.6),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    ggsave(sprintf("figures/%s_outcome_test_all.png", state),
           p_all, width = 8, height = 13, dpi = 300)
    
    # Additionally, for OH, create plots for a sample of 15 counties
    set.seed(42)
    sampled_counties <- sample(unique(disparity$county_name), 15)
    disparity_sampled <- disparity %>% filter(county_name %in% sampled_counties)
    
    p_sampled <- outcome_test_plot(disparity_sampled) +
      xlim(-0.15, 0.255) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 15),
            legend.position = c(0.7, 0.6),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    ggsave("figures/OH_outcome_test_sampled.png",
           p_sampled, width = 8, height = 8, dpi = 300)
    
    p_sampled_base <- outcome_test_plot_base(disparity_sampled) +
      xlim(-0.15, 0.255) +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 15))
    ggsave("figures/OH_outcome_test_sampled_base.png",
           p_sampled_base, width = 8, height = 8, dpi = 300)
    
  } else if (state %in% c("CO", "WI", "WA")) {
    p_state <- p +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 15),
            legend.position = c(0.6, 0.6),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    ggsave(sprintf("figures/%s_outcome_test_all.png", state),
           p_state, width = 8, height = 13, dpi = 300)
    
  } else if (state == "MD") {
    p_state <- p +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = c(0.6, 0.6),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    ggsave(sprintf("figures/%s_outcome_test_all.png", state),
           p_state, width = 8, height = 13, dpi = 300)
  }
  
  return(NULL)
}

# List of states to process
states <- c("OH", "CO", "WI", "WA", "MD")

# Process all states in parallel
future_map(states, process_state)
