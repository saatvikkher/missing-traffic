library(tidyverse)
library(ggridges)
source("outcome_test.R")

outcome_test_plot <- function(disparity_results) {
  disparity_results %>%
    ggplot() +
    geom_density_ridges(
      aes(y = reorder(county_name, -(black-white)),
          x = (p_black - p_white),
          point_color = prop_assigned_white),
      jittered_points = TRUE, rel_min_height = 0, fill="white",
      alpha = 0.7, scale = 0.95, linewidth=0
    ) +
    geom_density_ridges(
      aes(y = reorder(county_name, -(black-white)), 
          x = (p_black - p_white)),
      rel_min_height = 0.05, scale = 0.95, alpha=0,
      quantile_lines = TRUE, quantiles = 2,
    ) +
    
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

# Ohio, Statewide

dataset <- readRDS("data/oh_statewide_2020_04_01.rds")

# When search is not conducted, contraband_found can be assumed to be FALSE
dataset <- dataset %>%
  mutate(contraband_found = case_when(search_conducted & is.na(contraband_found) ~ FALSE,
                                      TRUE ~ contraband_found))

OH_disparity <- calculate_disparity(dataset)

# Sample of 15 Counties
set.seed(42)
sampled_counties <- sample(unique(OH_disparity$county_name), 15)

OH_p <- outcome_test_plot(OH_disparity %>% filter(county_name %in% sampled_counties))
OH_p +
  xlim(-0.15,0.255) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position = c(0.7, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
        ) 
ggsave("figures/OH_outcome_test_sampled.png", width = 8, height = 8, dpi = 300)

# Base Estimates
OH_p_base <- outcome_test_plot_base(OH_disparity %>% filter(county_name %in% sampled_counties))
OH_p_base +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15)) +
  xlim(-0.15,0.255)
ggsave("figures/OH_outcome_test_sampled_base.png", width = 8, height = 8, dpi = 300)


# All Counties
OH_p <- outcome_test_plot(OH_disparity)
OH_p +
  xlim(-0.24,0.6) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.position = c(0.6, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
  )
ggsave("figures/OH_outcome_test_all.png", width = 8, height = 13, dpi = 300)

# Colorado, Statewide

dataset <- readRDS("data/co_statewide_2020_04_01.rds")

CO_disparity <- calculate_disparity(dataset)
CO_p <- outcome_test_plot(CO_disparity)
CO_p +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position = c(0.6, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
  )
ggsave("figures/CO_outcome_test_all.png", width = 8, height = 13, dpi = 300)


# Wisconsin, Statewide

dataset <- readRDS("data/wi_statewide_2020_04_01.rds")

WI_disparity <- calculate_disparity(dataset)
WI_p <- outcome_test_plot(WI_disparity)
WI_p +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position = c(0.6, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
  )
ggsave("figures/WI_outcome_test_all.png", width = 8, height = 13, dpi = 300)


# Washington, Statewide
dataset <- readRDS("data/wa_statewide_2020_04_01.rds")

WA_disparity <- calculate_disparity(dataset)
WA_p <- outcome_test_plot(WA_disparity)
WA_p +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position = c(0.6, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
  )
ggsave("figures/WA_outcome_test_all.png", width = 8, height = 13, dpi = 300)


# Maryland, Statewide
# using department name instead of county name
dataset <- readRDS("data/md_statewide_2020_04_01.rds")
dataset %>%
  mutate(group_name = extract_first_word(department_name)) %>%  # Extract first word
  group_by(group_name) %>%
  mutate(county_name = first(department_name)) %>% # Assign first occurrence of department_name 
  ungroup() -> dataset

# group by str_sub, name slice_n(1) first in that group

MD_disparity <- calculate_disparity(dataset)
MD_p <- outcome_test_plot(MD_disparity)
MD_p +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = c(0.6, 0.6),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)
  )
ggsave("figures/MD_outcome_test_all.png", width = 8, height = 13, dpi = 300)



