library(dplyr)
library(tidyr)
library(lubridate)

# Read data
nest <- read.csv("data/raw/LtppNestMonitoringLog_01_21_2026.csv", stringsAsFactors = FALSE)

# Filter to specified sites
sites <- c("Cottonwood Ranch", "Dyer", "Kearney Broadfoot South", "Leaman", 
           "OSG Lexington", "Newark East", "Newark West", "NPPD Lexington", 
           "Follmer", "Blue Hole")

# filtering nest sites and technique
nest_data <- nest %>%
  dplyr::filter(Site %in% sites) %>%  # Filtering to selected sites
  dplyr::mutate(
    Visit.Date = as.Date(Visit.Date), # Modifying dates to be date format
    technique = dplyr::case_when(     # Make new column called technique
      grepl("^I-", Nest) ~ "inside",  # Inside technique for inside monitoring - found by "I" in nests
      grepl("^O-", Nest) ~ "outside", # Same for Outside
      TRUE ~ "outside"                # Pre-2012 nests default to outside
    )
  )

# Create beta regression dataset - one row per nest
beta_data <- nest_data %>%
  dplyr::arrange(Nest, Visit.Date) %>%
  dplyr::group_by(Nest) %>%
  dplyr::mutate(
    visit_exposure = c(0, as.numeric(diff(Visit.Date)))
  ) %>%
  dplyr::summarise(
    Species        = dplyr::first(Species),
    Site           = dplyr::first(Site),
    technique      = dplyr::first(technique),
    first_date     = min(Visit.Date),
    last_date      = max(Visit.Date),
    julian_day     = lubridate::yday(min(Visit.Date)),
    exposure       = sum(visit_exposure, na.rm = TRUE),
    final_status_raw = dplyr::last(Chick.Status),
    max_15day      = max(X..15.Day, na.rm = TRUE),
    max_lt21_pp28  = max(LT.21....PP.28., na.rm = TRUE),
    max_chicks     = max(X..15.Day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    final_status = dplyr::case_when(
      Species == "PIPL" & exposure > 27 ~ "Fledged", # rule for plovers
      Species == "LETE" & exposure > 20 ~ "Fledged", # rule for terns
      TRUE ~ final_status_raw
    ),
    has_success = final_status == "Fledged"
  ) %>%
  dplyr::filter(has_success) %>%
  dplyr::mutate(proportion = max_lt21_pp28 / max_15day) |> 
  dplyr::mutate(prop_flag = case_when(
    proportion > 1 ~ "Above 1",
    proportion < 0 ~ "Negative",
    TRUE ~ "Good"
  )) %>%  
  dplyr::mutate(
    proportion = ifelse(proportion > 1, 1, proportion),
    proportion = ifelse(proportion < 0, 0, proportion),
    proportion = ifelse(is.na(proportion) | is.infinite(proportion), 0, proportion)
  ) %>%
  dplyr::select(Nest, Species, Site, technique, first_date, last_date, julian_day, exposure, max_chicks, proportion, prop_flag)

# Save dataset
write.csv(beta_data, "data/zoib/zoib_data.csv", row.names = FALSE)

