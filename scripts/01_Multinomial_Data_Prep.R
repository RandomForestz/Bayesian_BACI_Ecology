

#==============================================================================#
  
  # 01 Multinomial Logistic Exposure Data Prep

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

site_mgmt <- read.csv("data/management/predator_mgmt.csv")

#==============================================================================#
# Adding in climate

clim <- read.csv("data/climate/prism.csv")

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
    ),
    ever_chick = any(stage == "chick"),
  ) %>%
  dplyr::ungroup()

# Create nest dataset (first observation to hatch or failure)
nest_stage_data <- nest_fates %>%
  filter(stage == "nest") %>%
  arrange(Nest, Visit.Date) %>%
  group_by(Nest) %>%
  mutate(
    exposure = c(0, as.numeric(diff(Visit.Date))),
    total_exposure = cumsum(exposure),
    is_last = row_number() == n(),
    Year = year(Visit.Date),
    outcome = case_when(
      
      # FIRST: if this nest ever transitions to chick stage,
      # then the last nest-stage interval = hatch
      is_last & ever_chick ~ "hatch",
      
      # Otherwise evaluate real nest failures
      is_last & Nest.Status == "Failed-Abandoned" ~ "abandoned",
      is_last & Nest.Status == "Failed-Flooded"   ~ "flooded",
      is_last & Nest.Status == "Failed-Predation" ~ "predation",
      is_last & Nest.Status == "Failed-Weather"   ~ "weather",
      is_last & Nest.Status == "Failed-Unknown"   ~ "unknown",
      is_last & Nest.Status == "Unknown Outcome"  ~ "unknown",
      
      TRUE ~ "censored"
    )
  ) %>%
  ungroup()

# Create one-hot encoding for nests + join site-year management
nest_final <- nest_stage_data %>%
  mutate(
    julian_day = lubridate::yday(Visit.Date),
    Year = lubridate::year(Visit.Date),
    s = ifelse(outcome == "hatch", 1, 0),
    p = ifelse(outcome == "predation", 1, 0),
    a = ifelse(outcome == "abandoned", 1, 0),
    w = ifelse(outcome == "weather", 1, 0),
    f = ifelse(outcome == "flooded", 1, 0),
    u = ifelse(outcome == "unknown", 1, 0)
  ) %>%
  left_join(site_mgmt, by = c("Site", "Year")) %>%
  mutate(
    across(
      c(exterior_fence, entry_fence, interior_fence, lights),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  select(
    Nest, Species, Site, Year, technique, Visit.Date, julian_day, exposure,
    exterior_fence, entry_fence, interior_fence, lights,
    s, p, a, w, f, u
  ) %>%
      left_join(clim, by = c("Site", "Year")) %>%
      mutate(
        across(
          c(ppt, tmin, tmean, tmax, dew, vpdmin, vpdmax),
          ~ tidyr::replace_na(.x, 0)
        ))
      

#==============================================================================#
# Chicks

# Create chick dataset (hatch to fledge or failure)
chick_data <- nest_fates %>%
  filter(stage == "chick") %>%
  arrange(Nest, Visit.Date) %>%
  group_by(Nest) %>%
  mutate(
    exposure = c(0, as.numeric(diff(Visit.Date))),
    total_exposure = cumsum(exposure),
    is_last = row_number() == n(),
    Year = year(Visit.Date),
    outcome = case_when(
      
      # Only evaluate event on last interval
      is_last & Chick.Status == "Fledged"          ~ "success",
      is_last & Chick.Status == "Failed-Predation" ~ "predation",
      is_last & Chick.Status == "Failed-Abandoned" ~ "abandoned",
      is_last & Chick.Status == "Failed-Weather"   ~ "weather",
      is_last & Chick.Status == "Failed-Flooded"   ~ "flooded",
      is_last & Chick.Status == "Failed-Unknown"   ~ "unknown",
      is_last & Chick.Status == "Unknown Outcome"  ~ "unknown",
      
      TRUE ~ "censored"
    ),
    
    # Species-specific override for UNKNOWN on last interval only
    outcome = case_when(
      is_last & outcome == "unknown" & Species == "PIPL" & total_exposure > 27 ~ "success",
      is_last & outcome == "unknown" & Species == "LETE" & total_exposure > 20 ~ "success",
      TRUE ~ outcome
    )
  ) %>%
  ungroup()

# Create one-hot encoding for chicks + join site-year management
chick_final <- chick_data %>%
  mutate(
    julian_day = lubridate::yday(Visit.Date),
    Year = lubridate::year(Visit.Date),
    s = ifelse(outcome == "success", 1, 0),
    p = ifelse(outcome == "predation", 1, 0),
    a = ifelse(outcome == "abandoned", 1, 0),
    w = ifelse(outcome == "weather", 1, 0),
    f = ifelse(outcome == "flooded", 1, 0),
    u = ifelse(outcome == "unknown", 1, 0)
  ) %>%
  left_join(site_mgmt, by = c("Site", "Year")) %>%
  mutate(
    across(
      c(exterior_fence, entry_fence, interior_fence, lights),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  select(
    Nest, Species, Site, Year, technique, Visit.Date, julian_day, exposure,
    exterior_fence, entry_fence, interior_fence, lights,
    s, p, a, w, f, u
  ) %>%
  left_join(clim, by = c("Site", "Year")) %>%
  mutate(
    across(
      c(ppt, tmin, tmean, tmax, dew, vpdmin, vpdmax),
      ~ tidyr::replace_na(.x, 0)
    ))


# Save datasets
write.csv(nest_final, "data/multinomial/01_nest_exposure_data.csv", row.names = FALSE)
write.csv(chick_final, "data/multinomial/01_chick_exposure_data.csv", row.names = FALSE)

