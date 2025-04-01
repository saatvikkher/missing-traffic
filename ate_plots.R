library(tidyverse)
theme_set(theme_minimal())

knox_results <- read_csv("ate_results/ate_results.csv")
knox_base <- read_csv("ate_results/ate_results_base.csv")
knox_manski <- read_csv("ate_results/ate_results_manski.csv")

knox_results %>%
  full_join(knox_manski, by = c("dataset_name", "query_threshold", "naive_estimate", "lower_bound", "upper_bound", "prop")) %>%
  full_join(knox_base, by = c("dataset_name", "query_threshold")) %>%
  mutate(
    # Extract the first two letters (state) and convert to uppercase
    state = toupper(str_extract(dataset_name, "^[a-z]{2}")),
    # Extract the descriptor between the first underscore and the next underscore,
    # and convert to title case
    descriptor = str_to_title(str_extract(dataset_name, "(?<=_)[a-z]+")),
    # Combine into the final format
    state = paste0(state, ", ", descriptor)
  ) -> knox_results


# Avoid overplotting
knox_results <- bind_rows(
  knox_results,
  knox_results %>% filter(!is.na(anti_black_bias)))


### Recreate Figure 9 from the paper

theme_set(theme_minimal())
knox_results %>%
  filter(dataset_name %in% c("wa_statewide_2020_04_01.csv", "oh_statewide_2020_04_01.csv")) %>%
  mutate(shift = case_when(query_threshold == 0.25 ~ 0.02,
                           query_threshold == 0.5 ~ 0.04,
                           query_threshold == 0.75 ~ 0.06),
         Manski = ifelse(is.na(anti_black_bias), "Random Assignment", "Extreme")) %>%
  ggplot() +
  geom_rect(aes(xmin=-0.31+shift, xmax=1+shift, ymin=base_lower_bound, ymax=base_upper_bound,
                fill = as.factor(query_threshold)), alpha = 0.03) +
  geom_point(aes(prop - 0.01, naive_estimate, color="Naive\nEstimator"), size=3) +
  geom_errorbar(aes(x=prop + shift, ymin=lower_bound, ymax=upper_bound, color = as.factor(query_threshold)), width=0.015) +
  labs(x = "Proportion NA assigned to White",
       y = "Disparity",
       color = " ") +
  geom_point(aes(-0.31, base_naive_estimate, color="Naive\nEstimator"), size=3) +
  geom_hline(aes(yintercept = base_naive_estimate), alpha=0.2) +
  geom_errorbar(aes(-0.3+shift, ymin=base_lower_bound, ymax=base_upper_bound, color = as.factor(query_threshold)), width=0.02, linetype=2)+
  geom_hline(yintercept = 0, linetype = 2) +
  guides(fill="none", alpha="none", color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(vjust = 0.5, size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.2, 'lines'),
        legend.position = "right",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)
  ) +
  scale_color_brewer(palette = "Dark2", name = "Proportion of racially\ndiscriminatory stops") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  facet_grid(state~factor(Manski, levels=c("Random Assignment", "Extreme"))) +
  theme(strip.background = element_rect(colour="black", fill="white", 
                                        size=1, linetype="solid"))
ggsave("figures/knox_OH_WA.png", width = 10, height = 8, dpi = 300)


### Recreate Random Assignment Plots

for (name in unique(knox_results$state)) {
  knox_results %>%
    filter(state == name) %>%  
    filter(is.na(anti_black_bias)) %>%
    mutate(shift = case_when(query_threshold == 0.25 ~ 0.02,
                             query_threshold == 0.5 ~ 0.04,
                             query_threshold == 0.75 ~ 0.06)) %>%
    ggplot() +
    geom_point(aes(prop - 0.01, naive_estimate, color="Naive Estimate"), size=3) +
    geom_errorbar(aes(x=prop + shift, ymin=lower_bound, ymax=upper_bound, color = as.factor(query_threshold)), width=0.015) +
    labs(x = "Proportion NA assigned to White",
         y = "ATE",
         color = " ") +
    geom_point(aes(-0.31, base_naive_estimate, color="Naive Estimate"), size=3) +
    geom_hline(aes(yintercept = base_naive_estimate), alpha=0.2) +
    geom_errorbar(aes(-0.3+shift, ymin=base_lower_bound, ymax=base_upper_bound, color = as.factor(query_threshold)), width=0.02, linetype=2) +
    geom_rect(aes(xmin=-0.31+shift, xmax=1+shift, ymin=base_lower_bound, ymax=base_upper_bound, fill = as.factor(query_threshold)), alpha = 0.03) +
    geom_hline(yintercept = 0, linetype = 2) +
    # theme_bw() +
    guides(fill="none", color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.title = element_text(vjust = 0.5, size = 14),
          legend.text = element_text(size = 12),
          legend.spacing.x = unit(0.5, 'cm'),
          legend.key.size = unit(1.2, 'lines'),
          legend.position = "top",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.y=element_blank()
    ) +
    scale_color_brewer(palette = "Dark2", name = name) +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) -> p
  ggsave(p, filename = paste0("figures/knox/", name, "_knox_random.png"), width = 150, height = 100, dpi = 300, units = "mm")
}


### Recreate Manski Plots

for (name in unique(knox_results$state)) {
  knox_results %>%
    filter(state == name) %>%
    filter(!is.na(anti_black_bias)) %>%
    mutate(shift = case_when(query_threshold == 0.25 ~ 0.02,
                             query_threshold == 0.5 ~ 0.04,
                             query_threshold == 0.75 ~ 0.06)) %>%
    ggplot() +
    geom_point(aes(prop - 0.01, naive_estimate, color="Naive Estimate"), size=3) +
    geom_errorbar(aes(x=prop + shift, ymin=lower_bound, ymax=upper_bound, color = as.factor(query_threshold)), width=0.015) +
    labs(x = "Proportion NA assigned to White",
         y = "ATE",
         color = " ") +
    geom_point(aes(-0.31, base_naive_estimate, color="Naive Estimate"), size=3) +
    geom_hline(aes(yintercept = base_naive_estimate), alpha=0.2) +
    geom_errorbar(aes(-0.3+shift, ymin=base_lower_bound, ymax=base_upper_bound, color = as.factor(query_threshold)), width=0.02, linetype=2) +
    geom_rect(aes(xmin=-0.31+shift, xmax=1+shift, ymin=base_lower_bound, ymax=base_upper_bound, fill = as.factor(query_threshold)), alpha = 0.03) +
    geom_hline(yintercept = 0, linetype = 2) +
    guides(fill="none", color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.title = element_text(vjust = 0.5, size = 14),
          legend.text = element_text(size = 12),
          legend.spacing.x = unit(0.5, 'cm'),
          legend.key.size = unit(1.2, 'lines'),
          legend.position = "top",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.y=element_blank()
    ) +
    scale_color_brewer(palette = "Dark2", name = name) +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) -> p
  ggsave(p, filename = paste0("figures/knox/", name, "_knox_manski.png"), width = 150, height = 100, dpi = 300, units = "mm")
}