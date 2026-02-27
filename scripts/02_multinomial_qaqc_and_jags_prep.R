

#==============================================================================#

# 02 Multinomial Logistic QA/QC & JAGS Prep

#==============================================================================#  
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)


# Load the interval-level exposure datasets (already one-hot)
nest_final  <- readr::read_csv("data/multinomial/01_nest_exposure_data.csv",  show_col_types = FALSE)
chick_final <- readr::read_csv("data/multinomial/01_chick_exposure_data.csv", show_col_types = FALSE)


# Filter out fully-censored groups (no terminal event)
nest_final <- nest_final %>%
  group_by(Nest) %>%
  mutate(fate_sum = s + p + a + w + f + u) %>%
  filter(any(fate_sum == 1)) %>%
  select(-fate_sum) %>%
  ungroup()

chick_final <- chick_final %>%
  group_by(Nest) %>%
  mutate(fate_sum = s + p + a + w + f + u) %>%
  filter(any(fate_sum == 1)) %>%
  select(-fate_sum) %>%
  ungroup()


# QA/QC check: last interval must be event, previous must be censored
check_nests <- nest_final %>%
  arrange(Nest, Visit.Date) %>%
  group_by(Nest) %>%
  mutate(
    fate_sum = s + p + a + w + f + u,
    is_last = row_number() == n()
  ) %>%
  summarise(
    last_fate = fate_sum[is_last],
    prev_fate_sum = sum(fate_sum[!is_last]),
    .groups = "drop"
  )

summary(check_nests$last_fate)
summary(check_nests$prev_fate_sum)

# check bad nests
bad_nests <- check_nests %>%
  filter(last_fate == 0)
bad_nests

# If you want to inspect the bad nestS (only if it exists)
nest_final %>%
  filter(Nest == "INPUT NEST ID HERE") %>%
  arrange(Visit.Date)


# Save
write.csv(
  nest_final,
  "data/multinomial/02_nest_exposure_data_clean.csv",
  row.names = FALSE
)

write.csv(
  chick_final,
  "data/multinomial/02_chick_exposure_data_clean.csv",
  row.names = FALSE
)