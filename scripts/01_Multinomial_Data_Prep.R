

#==============================================================================#
  
  # Multinomial Logistic Exposure Data Prep

#==============================================================================#  

# Libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Read the data - RAW Nest Monitoring Log from the PRIPP Species Database
nest <- read.csv("data/raw/LtppNestMonitoringLog_01_21_2026.csv", stringsAsFactors = FALSE)

# Filter to specified sites
sites <- c(
  "Cottonwood Ranch", "Dyer", "Kearney Broadfoot South", "Leaman",
  "OSG Lexington", "Newark East", "Newark West", "NPPD Lexington",
  "Follmer", "Blue Hole"
)

# Filter nest information
nest_data <- nest %>%
  dplyr::filter(lubridate::year(Visit.Date) >= 2010) %>% 
  dplyr::filter(Site %in% sites) %>%  # Filtering to selected sites
  dplyr::mutate(
    Visit.Date = as.Date(Visit.Date), # Modifying dates to be date format
    technique = dplyr::case_when(     # Make new column called technique
      grepl("^I-", Nest) ~ "inside",  # Inside technique for inside monitoring - found by "I" in nests
      grepl("^O-", Nest) ~ "outside", # Same for Outside
      TRUE ~ "outside"                # Pre-2012 nests default to outside
    )
  )

#==============================================================================#
# Adding in treatment information

library(tibble)

site_mgmt <- tribble(
  ~Site,                  ~Year, ~exterior_fence, ~entry_fence, ~interior_fence, ~lights,
  "Cottonwood Ranch",      2020,  0,               1,            0,               0,
  "Cottonwood Ranch",      2021,  0,               1,            0,               0,
  "Cottonwood Ranch",      2022,  0,               1,            0,               0,
  "Cottonwood Ranch",      2023,  0,               1,            0,               0,
  "Cottonwood Ranch",      2024,  0,               1,            0,               0,
  "Cottonwood Ranch",      2025,  0,               1,            0,               0,
  
  "Dyer",                  2020,  0,               1,            0,               0,
  "Dyer",                  2021,  0,               1,            0,               0,
  "Dyer",                  2022,  0,               1,            0,               0,
  "Dyer",                  2023,  0,               1,            0,               0,
  "Dyer",                  2024,  0,               1,            0,               0,
  "Dyer",                  2025,  0,               1,            0,               0,
  
  "Kearney Broadfoot South",2020,  0,               1,            1,               0,
  "Kearney Broadfoot South",2021,  0,               1,            1,               1,
  "Kearney Broadfoot South",2022,  0,               1,            1,               1,
  "Kearney Broadfoot South",2023,  0,               1,            1,               1,
  "Kearney Broadfoot South",2024,  0,               1,            1,               1,
  "Kearney Broadfoot South",2025,  0,               1,            1,               1,
  
  "Leaman",                2020,  0,               1,            0,               0,
  "Leaman",                2021,  0,               1,            0,               1,
  "Leaman",                2022,  0,               1,            0,               1,
  "Leaman",                2023,  0,               1,            0,               1,
  "Leaman",                2024,  0,               1,            0,               1,
  "Leaman",                2025,  0,               1,            0,               1,
  
  "OSG Lexington",         2020,  0,               1,            0,               0,
  "OSG Lexington",         2021,  0,               1,            0,               0,
  "OSG Lexington",         2022,  0,               1,            0,               0,
  "OSG Lexington",         2023,  0,               1,            0,               0,
  "OSG Lexington",         2024,  0,               1,            0,               0,
  "OSG Lexington",         2025,  0,               1,            0,               0,
  
  "Newark East",            2020,  0,               1,            0,               0,
  "Newark East",            2021,  0,               1,            0,               0,
  "Newark East",            2022,  0,               1,            0,               0,
  "Newark East",            2023,  0,               1,            0,               0,
  "Newark East",            2024,  0,               1,            0,               0,
  "Newark East",            2025,  0,               1,            0,               0,
  
  "Newark West",            2020,  1,               1,            0,               1,
  "Newark West",            2021,  1,               1,            0,               1,
  "Newark West",            2022,  1,               1,            0,               1,
  "Newark West",            2023,  1,               1,            0,               1,
  "Newark West",            2024,  1,               1,            0,               1,
  "Newark West",            2025,  1,               1,            0,               0,
  
  "NPPD Lexington",         2020,  0,               1,            0,               0,
  "NPPD Lexington",         2021,  0,               1,            0,               0,
  "NPPD Lexington",         2022,  0,               1,            0,               0,
  "NPPD Lexington",         2023,  0,               1,            0,               0,
  "NPPD Lexington",         2024,  0,               1,            0,               0,
  "NPPD Lexington",         2025,  0,               1,            0,               0,
  
  "Follmer",                2020,  0,               0,            0,               0,
  "Follmer",                2021,  0,               0,            0,               0,
  "Follmer",                2022,  0,               0,            0,               0,
  "Follmer",                2023,  0,               0,            0,               0,
  "Follmer",                2024,  0,               0,            0,               0,
  "Follmer",                2025,  0,               1,            0,               0,
  
  "Blue Hole",              2020,  0,               1,            1,               1,
  "Blue Hole",              2021,  0,               1,            0,               0,
  "Blue Hole",              2022,  0,               0,            0,               0,
  "Blue Hole",              2023,  0,               0,            0,               0,
  "Blue Hole",              2024,  0,               0,            0,               0,
  "Blue Hole",              2025,  0,               0,            0,               0
)



#==============================================================================#
# Nests

# Determine stage (nest vs chick)
nest_fates <- nest_data %>%
  dplyr::arrange(Nest, Visit.Date) %>% # Organing by nest, then date
  dplyr::group_by(Nest) %>%            # Grouping by nests
  dplyr::mutate(
    stage = dplyr::case_when(          # Assigning chick outcomes to chick status for nests
      Chick.Status %in% c(
        "Live Chicks", "Fledged", "Failed-Predation", "Failed-Abandoned",
        "Failed-Weather", "Failed-Flooded", "Failed-Unknown", "Unknown Outcome"
      ) ~ "chick",
      TRUE ~ "nest"                   # The rest are nest
    )
  ) %>%
  dplyr::ungroup()

# Create nest dataset (first observation to hatch or failure)
nest_stage_data <- nest_fates %>%
  dplyr::filter(stage == "nest") %>%                                # filtering to nests only
  dplyr::arrange(Nest, Visit.Date) %>%                              # arranging by nest then visit date
  dplyr::group_by(Nest) %>%                                         # grouping by nest
  dplyr::mutate( 
    exposure = c(0, as.numeric(diff(Visit.Date))),                  # calculating the exposure 
    total_exposure = cumsum(exposure),                              # sum the exposure
    is_last = dplyr::row_number() == dplyr::n(),                    

    outcome = dplyr::case_when(                                     # Assigning the status of outcome
      is_last & Nest.Status == "Successful Nest"  ~ "hatch",        
      is_last & Nest.Status == "Failed-Abandoned" ~ "abandoned",
      is_last & Nest.Status == "Failed-Flooded"   ~ "flooded",
      is_last & Nest.Status == "Failed-Predation" ~ "predation",
      is_last & Nest.Status == "Failed-Weather"   ~ "weather",
      is_last & Nest.Status == "Failed-Unknown"   ~ "unknown",
      is_last & Nest.Status == "Unknown Outcome"  ~ "unknown",
      TRUE ~ "censored"  # Active Nest falls through to censored
    ),

    # Apply species-specific rules for UNKNOWN outcomes only (last row only)
    outcome = dplyr::case_when(
      is_last & outcome == "unknown" & Species == "PIPL" & total_exposure > 27 ~ "hatch", # Rule for Plovers outcome
      is_last & outcome == "unknown" & Species == "LETE" & total_exposure > 20 ~ "hatch", # Rule for Terns outcome
      TRUE ~ outcome # If not meeting rule, keep outcome
    )
  ) %>%
  dplyr::ungroup()

# Create one-hot encoding for nests + join site-year management
nest_final <- nest_stage_data %>%
  dplyr::mutate(
    julian_day = lubridate::yday(Visit.Date),
    Year = lubridate::year(Visit.Date),
    s = ifelse(outcome == "hatch", 1, 0),
    p = ifelse(outcome == "predation", 1, 0),
    a = ifelse(outcome == "abandoned", 1, 0),
    w = ifelse(outcome == "weather", 1, 0),
    f = ifelse(outcome == "flooded", 1, 0),
    u = ifelse(outcome == "unknown", 1, 0)
  ) %>%
  dplyr::left_join(site_mgmt, by = c("Site", "Year")) %>%
  dplyr::mutate(
    dplyr::across(
      c(exterior_fence, entry_fence, interior_fence, lights),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  dplyr::select(
    Nest, Species, Site, technique, Visit.Date, julian_day, exposure,
    exterior_fence, entry_fence, interior_fence, lights,
    s, p, a, w, f, u
  )


#==============================================================================#
# Chicks

# Create chick dataset (hatch to fledge or failure)
chick_data <- nest_fates %>%
  dplyr::filter(stage == "chick") %>%              # Chick stage
  dplyr::arrange(Nest, Visit.Date) %>%             # arrange by nest id then data
  dplyr::group_by(Nest) %>%
  dplyr::mutate(
    exposure = c(0, as.numeric(diff(Visit.Date))), # Exposure
    total_exposure = cumsum(exposure),             # Total exposure
    is_last = dplyr::row_number() == dplyr::n(),

    outcome = dplyr::case_when(
      is_last & Chick.Status == "Fledged"          ~ "success", # Assigning the status of outcome
      is_last & Chick.Status == "Failed-Predation" ~ "predation",
      is_last & Chick.Status == "Failed-Abandoned" ~ "abandoned",
      is_last & Chick.Status == "Failed-Weather"   ~ "weather",
      is_last & Chick.Status == "Failed-Flooded"   ~ "flooded",
      is_last & Chick.Status == "Failed-Unknown"   ~ "unknown",
      is_last & Chick.Status == "Unknown Outcome"  ~ "unknown",
      TRUE ~ "censored"  # Live Chicks falls through to censored
    ),

    # Apply species-specific rules for UNKNOWN outcomes only (last row only)
    outcome = dplyr::case_when(
      is_last & outcome == "unknown" & Species == "PIPL" & total_exposure > 27 ~ "success", # rule for Plovers
      is_last & outcome == "unknown" & Species == "LETE" & total_exposure > 20 ~ "success", # Rule for Terns
      TRUE ~ outcome # Keep the outcome if rule isnt met
    )
  ) %>%
  dplyr::ungroup()

# Create one-hot encoding for chicks + join site-year management
chick_final <- chick_data %>%
  dplyr::mutate(
    julian_day = lubridate::yday(Visit.Date),
    Year = lubridate::year(Visit.Date),
    s = ifelse(outcome == "success", 1, 0),
    p = ifelse(outcome == "predation", 1, 0),
    a = ifelse(outcome == "abandoned", 1, 0),
    w = ifelse(outcome == "weather", 1, 0),
    f = ifelse(outcome == "flooded", 1, 0),
    u = ifelse(outcome == "unknown", 1, 0)
  ) %>%
  dplyr::left_join(site_mgmt, by = c("Site", "Year")) %>%
  dplyr::mutate(
    dplyr::across(
      c(exterior_fence, entry_fence, interior_fence, lights),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  dplyr::select(
    Nest, Species, Site, technique, Visit.Date, julian_day, exposure,
    exterior_fence, entry_fence, interior_fence, lights,
    s, p, a, w, f, u
  )




# Save datasets
write.csv(nest_final, "data/multinomial/nest_exposure_data.csv", row.names = FALSE)
write.csv(chick_final, "data/multinomial/chick_exposure_data.csv", row.names = FALSE)

