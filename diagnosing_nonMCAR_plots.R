library(tidyverse)
library(latex2exp)
theme_set(theme_minimal())

## Creating datasets that include the covariates of interest
## using only vehicular stops, not pedestrian stops

relevant_covariates = c("date", "subject_race",   "outcome",  "location",
                        "time", "subject_sex", "citation_issued",  "subject_age", 
                        "lat", "lng", "warning_issued", "arrest_made", "search_conducted", 
                        "violation", "officer_id_hash", "contraband_found", "search_basis", 
                        "reason_for_stop", "county_name", "vehicle_make")
                        
az_statewide_2020_04_01 <- readRDS("data/az_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ca_bakersfield_2020_04_01 <- readRDS("data/ca_bakersfield_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ca_los_angeles_2020_04_01 <- readRDS("data/ca_los_angeles_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ca_statewide_2023_01_26 <- readRDS("data/ca_statewide_2023_01_26.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
co_aurora_2023_01_26 <- readRDS("data/co_aurora_2023_01_26.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
co_statewide_2020_04_01 <- readRDS("data/co_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
fl_tampa_2020_04_01 <- readRDS("data/fl_tampa_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ia_statewide_2020_04_01 <- readRDS("data/ia_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
il_chicago_2023_01_26 <- readRDS("data/il_chicago_2023_01_26.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ks_wichita_2023_01_26 <- readRDS("data/ks_wichita_2023_01_26.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
ky_louisville_2023_01_26 <- readRDS("data/ky_louisville_2023_01_26.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
md_statewide_2020_04_01 <- readRDS("data/md_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
mn_saint_paul_2020_04_01 <- readRDS("data/mn_saint_paul_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
nc_charlotte_2020_04_01 <- readRDS("data/nc_charlotte_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
nj_statewide_2020_04_01 <- readRDS("data/nj_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
oh_statewide_2020_04_01 <- readRDS("data/oh_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
sc_statewide_2020_04_01 <- readRDS("data/sc_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
tn_nashville_2020_04_01 <- readRDS("data/tn_nashville_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
wa_statewide_2020_04_01 <- readRDS("data/wa_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)
wi_statewide_2020_04_01 <- readRDS("data/wi_statewide_2020_04_01.rds") |> 
  filter(type == "vehicular") |> 
  select(any_of(relevant_covariates))
  #select(subject_race, search_conducted, contraband_found, time, date, lat, lng)



## Depending on the plot, different dCMR is calculated


ohio_dcmr_time <- count_nas_dCMR('oh_statewide_2020_04_01', oh_statewide_2020_04_01, "time")
chicago_dcmr_time <- count_nas_dCMR('il_chicago_2023_01_26', il_chicago_2023_01_26, "time")
md_dcmr_time <- count_nas_dCMR('md_statewide_2020_04_01', md_statewide_2020_04_01, "time")
nj_dcmr_time <- count_nas_dCMR('nj_statewide_2020_04_01', nj_statewide_2020_04_01, "time")
wa_dcmr_time <- count_nas_dCMR('wa_statewide_2020_04_01', wa_statewide_2020_04_01, "time")
wi_dcmr_time <- count_nas_dCMR('wi_statewide_2020_04_01', wi_statewide_2020_04_01, "time")

chicago_dcmr_date <- count_nas_dCMR('il_chicago_2023_01_26', il_chicago_2023_01_26, "date")
ia_dcmr_date <- count_nas_dCMR('ia_statewide_2020_04_01', ia_statewide_2020_04_01, "date")
louisville_dcmr_date <- count_nas_dCMR('ky_louisville_2023_01_26', ky_louisville_2023_01_26, "date")
nashville_dcmr_date <- count_nas_dCMR('tn_nashville_2020_04_01', tn_nashville_2020_04_01, "date")
stpaul_dcmr_date <- count_nas_dCMR('mn_saint_paul_2020_04_01', mn_saint_paul_2020_04_01, "date")
sc_dcmr_date <- count_nas_dCMR('sc_statewide_2020_04_01', sc_statewide_2020_04_01, "date")
wichita_dcmr_date <- count_nas_dCMR('ks_wichita_2023_01_26', ks_wichita_2023_01_26, "date")
losangeles_dcmr_date <- count_nas_dCMR('ca_los_angeles_2020_04_01', ca_los_angeles_2020_04_01, "date")
bakersfield_dcmr_date <- count_nas_dCMR('ca_bakersfield_2020_04_01', ca_bakersfield_2020_04_01, "date")
tampa_dcmr_date <- count_nas_dCMR('fl_tampa_2020_04_01', fl_tampa_2020_04_01, "date")
az_dcmr_date <- count_nas_dCMR('az_statewide_2020_04_01', az_statewide_2020_04_01, "date")
aurora_dcmr_date <- count_nas_dCMR('co_aurora_2023_01_26', co_aurora_2023_01_26, "date")
ca_dcmr_date <- count_nas_dCMR('ca_statewide_2023_01_26', ca_statewide_2023_01_26, "date")
chicago_dcmr_date <- count_nas_dCMR('il_chicago_2023_01_26', il_chicago_2023_01_26, "date")
co_dcmr_date <- count_nas_dCMR('co_statewide_2020_04_01', co_statewide_2020_04_01, "date")
md_dcmr_date <- count_nas_dCMR('md_statewide_2020_04_01', md_statewide_2020_04_01, "date")
nj_dcmr_date <- count_nas_dCMR('nj_statewide_2020_04_01', nj_statewide_2020_04_01, "date")
oh_dcmr_date <- count_nas_dCMR('oh_statewide_2020_04_01', oh_statewide_2020_04_01, "date")
wa_dcmr_date <- count_nas_dCMR('wa_statewide_2020_04_01', wa_statewide_2020_04_01, "date")
wi_dcmr_date <- count_nas_dCMR('wi_statewide_2020_04_01', wi_statewide_2020_04_01, "date")

chicago_dcmr_g6 <- count_nas_dCMR('il_chicago_2023_01_26', il_chicago_2023_01_26, c("lat", "lng"))
wi_dcmr_g6 <- count_nas_dCMR('wi_statewide_2020_04_01', wi_statewide_2020_04_01, c("lat", "lng"))
oh_dcmr_g6 <- count_nas_dCMR('oh_statewide_2020_04_01', oh_statewide_2020_04_01, c("lat", "lng"))
wa_dcmr_g6 <- count_nas_dCMR('wa_statewide_2020_04_01', wa_statewide_2020_04_01, c("lat", "lng"))


#####################################################
# Figure 1 - dCMR broken down by hour, Ohio statewide
#####################################################

p1 <- ohio_dcmr_time |>
       ggplot(aes(x = hour_24, y = dCMR)) +
         geom_point(size=1) +
         theme(axis.title.y=element_text(size=10),
               axis.text.x = element_text(size=8, angle=0),
               axis.text.y = element_text(size=8),
               legend.text = element_text(size=8),
               legend.title = element_text(size=8),
               strip.text.x =element_text(size=8),
               legend.key.height = unit(3, "pt"),
               legend.title.position = "left",
               legend.position="bottom") +
         xlab("") +
         ylab(TeX("$\\bar{\\omega}(k = time, b = hour)$")) +
         scale_color_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
         labs(title = "dCMR: Ohio, Statewide")

ggsave("F1_dcmr_time_OH.png", plot = p1, height=8, width=8) 


#####################################################
# Figure 2 - histogram of Charlotte, NC 5 year recording
#####################################################

p2 <- ggplot(nc_charlotte_2020_04_01,
       aes(x = subject_age)) + 
  geom_histogram(breaks = seq(10,100,1)) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  scale_x_continuous(
    breaks = seq(floor(min(nc_charlotte_2020_04_01$subject_age, na.rm = TRUE)), 
                 ceiling(max(nc_charlotte_2020_04_01$subject_age, na.rm = TRUE)), by = 5),
    labels = function(x) ifelse(x %% 10 == 0, x, "")
  ) +
  labs(x = "Motorist Age",
       y = "Number of stops (thousands)")

ggsave("F2_charlotte_age.png", plot = p2, height=3, width=5) 

#####################################################
# Figure 3 - NA
#####################################################

#####################################################
# Figure 4 - dCMR week
#####################################################

dcmr_week <- rbind(ia_dcmr_date, louisville_dcmr_date, nashville_dcmr_date,
                   stpaul_dcmr_date, sc_dcmr_date, wichita_dcmr_date)

p4 <- dcmr_week |>
  mutate(dataset_name_clean = case_when(substr(dataset_name, 1, 2) == "ia"~ "Iowa, Statewide",
                                        substr(dataset_name, 1, 2) == "ky"~ "Louisville, Kentucky",
                                        substr(dataset_name, 1, 2) == "tn"~ "Nashville, Tennessee",
                                        substr(dataset_name, 1, 2) == "mn"~ "Saint Paul, Minnesota",
                                        substr(dataset_name, 1, 2) == "sc"~ "South Carolina, Statewide",
                                        substr(dataset_name, 1, 2) == "ks"~ "Wichita, Kansas"),
         dataset_name_clean = paste0(dataset_name_clean, "\n(", round(max_cor, 2), ")")) |>
  ggplot(aes(x = ymd(week), y = dCMR)) +
  geom_point(size=.5) +
  theme(axis.title.y=element_text(size=10),
        axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        strip.text.x =element_text(size=8),
        legend.key.height = unit(3, "pt"),
        legend.title.position = "left",
        legend.position="bottom") +
  xlab("") +
  ylab(TeX("$\\bar{\\omega}(k = date, b = week)$")) +
  scale_color_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~ dataset_name_clean, scales="free")

ggsave("F4_dcmr_week.png", plot = p4, height=5, width=7)

#####################################################
# Figure 5 - dCMR week @ quantiles
#####################################################


dcmr_week_q <- rbind(losangeles_dcmr_date, bakersfield_dcmr_date,
                   tampa_dcmr_date, az_dcmr_date, aurora_dcmr_date)

p5 <- dcmr_week_q |>
  mutate(dataset_name_clean = case_when(substr(dataset_name, 1, 4) == "ca_l"~ "Los Angeles, California",
                                        substr(dataset_name, 1, 4) == "ca_b"~ "Bakersfield, California",
                                        substr(dataset_name, 1, 2) == "fl"~ "Tampa, Florida",
                                        substr(dataset_name, 1, 2) == "az"~ "Arizona, Statewide",
                                        substr(dataset_name, 1, 2) == "co"~ "Aurora, Colorado"),
         dataset_name_clean = paste0(dataset_name_clean, "\n(", round(max_cor, 2), ")")) |>
  mutate(dataset_name_clean = forcats::fct_reorder(dataset_name_clean,
                                                   max_cor, .fun = mean)) |> 
  ggplot(aes(x = ymd(week), y = dCMR)) +
  geom_point(size=.5) +
  theme(axis.title.y=element_text(size=10),
        axis.text.x = element_text(size=8, angle=30),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        strip.text.x =element_text(size=6),
        legend.key.height = unit(3, "pt"),
        legend.title.position = "left",
        legend.position="bottom") +
  xlab("") +
  ylab(TeX("$\\bar{\\omega}(k = date, b = week)$")) +
  scale_color_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~ dataset_name_clean, scales="free", nrow = 1)

ggsave("F5_dcmr_week_q.png", plot = p5, height=2, width=7) 



#####################################################
# Figure 6 - dCMR week for 8 main datasets
#####################################################


dcmr_week_8 <- rbind(ca_dcmr_date, chicago_dcmr_date, co_dcmr_date,
                   md_dcmr_date, nj_dcmr_date, oh_dcmr_date,
                   wa_dcmr_date, wi_dcmr_date)

p6 <- dcmr_week_8 |>
  mutate(dataset_name_clean = case_when(substr(dataset_name, 1, 2) == "ca"~ "California, Statewide",
                                        substr(dataset_name, 1, 2) == "co"~ "Colorado, Statewide",
                                        substr(dataset_name, 1, 2) == "oh"~ "Ohio, Statewide",
                                        substr(dataset_name, 1, 2) == "wa"~ "Washington, Statewide",
                                        substr(dataset_name, 1, 2) == "md"~ "Maryland, Statewide",
                                        substr(dataset_name, 1, 2) == "nj"~ "New Jersey, Statewide",
                                        substr(dataset_name, 1, 2) == "wi"~ "Wisconsin, Statewide",
                                        substr(dataset_name, 1, 2) == "il"~ "Chicago, Illinois"),
  dataset_name_clean = paste0(dataset_name_clean, "\n(", round(max_cor, 2), ")")) |>
  ggplot(aes(x = ymd(week), y = dCMR)) +
  geom_point(size=.5) +
  theme(axis.title.y=element_text(size=10),
        axis.text.x = element_text(size=8, angle=30),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        strip.text.x =element_text(size=8),
        legend.key.height = unit(3, "pt"),
        legend.title.position = "left",
        legend.position="bottom") +
  xlab("") +
  ylab(TeX("$\\bar{\\omega}(k = date, b = week)$")) +
  scale_color_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~ dataset_name_clean, scales="free", nrow = 3)

ggsave("F6_dcmr_week_8.png", plot = p6, height=6, width=7) 




#####################################################
# Figure 7 - dCMR time for 6 main datasets
#####################################################


dcmr_time <- rbind(ohio_dcmr_time, chicago_dcmr_time, md_dcmr_time, 
                   nj_dcmr_time, wa_dcmr_time, wi_dcmr_time )

p7 <- dcmr_time |>
  mutate(dataset_name_clean = case_when(substr(dataset_name, 1, 2) == "ca"~ "California, Statewide",
                                        substr(dataset_name, 1, 2) == "co"~ "Colorado, Statewide",
                                        substr(dataset_name, 1, 2) == "oh"~ "Ohio, Statewide",
                                        substr(dataset_name, 1, 2) == "wa"~ "Washington, Statewide",
                                        substr(dataset_name, 1, 2) == "md"~ "Maryland, Statewide",
                                        substr(dataset_name, 1, 2) == "nj"~ "New Jersey, Statewide",
                                        substr(dataset_name, 1, 2) == "wi"~ "Wisconsin, Statewide",
                                        substr(dataset_name, 1, 2) == "il"~ "Chicago, Illinois"),
         dataset_name_clean = paste0(dataset_name_clean, "\n(", round(max_cor, 2), ")")) |>
  ggplot(aes(x = hour_24, y = dCMR)) +
  geom_point(size=1) +
  theme(axis.title.y=element_text(size=10),
        axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        strip.text.x =element_text(size=8),
        legend.key.height = unit(3, "pt"),
        legend.title.position = "left",
        legend.position="bottom") +
  xlab("") +
  ylab(TeX("$\\bar{\\omega}(k = time, b = hour)$")) +
  scale_color_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~ dataset_name_clean, scales="free", nrow = 2)

ggsave("F7_dcmr_time.png", plot = p7, height=5, width=7)



#####################################################
# Figure 8 -- dCMR lat long
#####################################################

chicago_lat_long <- data.frame(geohashTools::gh_decode(chicago_dcmr_g6$g6)) |> 
  bind_cols(chicago_dcmr_g6) 

wi_lat_long <- data.frame(geohashTools::gh_decode(wi_dcmr_g6$g6)) |> 
  bind_cols(wi_dcmr_g6) 

wa_lat_long <- data.frame(geohashTools::gh_decode(wa_dcmr_g6$g6)) |> 
  bind_cols(wa_dcmr_g6) 

oh_lat_long <- data.frame(geohashTools::gh_decode(oh_dcmr_g6$g6)) |> 
  bind_cols(oh_dcmr_g6) 

dcmr_g6 <- rbind(chicago_lat_long, wi_lat_long, wa_lat_long,
                 oh_lat_long)

p8 <- dcmr_g6 |>
  mutate(dataset_name_clean = case_when(substr(dataset_name, 1, 2) == "ca"~ "California, Statewide",
                                        substr(dataset_name, 1, 2) == "co"~ "Colorado, Statewide",
                                        substr(dataset_name, 1, 2) == "oh"~ "Ohio, Statewide",
                                        substr(dataset_name, 1, 2) == "wa"~ "Washington, Statewide",
                                        substr(dataset_name, 1, 2) == "md"~ "Maryland, Statewide",
                                        substr(dataset_name, 1, 2) == "nj"~ "New Jersey, Statewide",
                                        substr(dataset_name, 1, 2) == "wi"~ "Wisconsin, Statewide",
                                        substr(dataset_name, 1, 2) == "il"~ "Chicago, Illinois")) |>
  filter(latitude > 10) |>
  filter((dataset_name_clean == "Ohio, Statewide" & longitude < -80 & longitude > -85 & latitude < 42)| dataset_name_clean != "Ohio, Statewide") |>
  mutate(longitude = case_when(dataset_name_clean=="Wisconsin, Statewide" ~ -abs(longitude),
                               TRUE ~ longitude)) |>
  filter((dataset_name_clean == "Wisconsin, Statewide" & latitude >  42)| dataset_name_clean != "Wisconsin, Statewide") |>
  mutate(dataset_name_clean = paste0(dataset_name_clean, "\n(", round((max_cor_lat + max_cor_lng)/2, 2), ")")) |>
  ggplot(aes(y = latitude, x = longitude, 
             color = dCMR)) +
  geom_point(size=0.02, alpha=0.8) +
  theme(axis.title.y=element_text(size=10),
        axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        strip.text.x =element_text(size=8),
        legend.key.height = unit(3, "pt"),
        legend.title.position = "left",
        legend.position="bottom") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(color=TeX("$\\bar{\\omega} (k = (latitude, longitude), b = geohash )$     ")) +
  facet_wrap(~ dataset_name_clean, scales="free") + 
  theme(aspect.ratio = 1) + 
  scale_color_gradient(low = "yellow", high = "darkblue")

ggsave("F8_dcmr_latlong.png", plot = p8, height=8, width=8) 

