library(tidyverse)

# Read in main_file with lsoa variable before
# Matching by LSOA (neighbourhood)

# Create date variable
main_file$interview_date <- as.Date(paste(main_file$i_istrtdaty, main_file$i_istrtdatm, main_file$i_istrtdatd, sep = "-"), format="%Y-%m-%d")

# Matching to local elections
# Note need to match back by date because of by-elections
# There will be many LSOA matches in y df (multiple elections + upper/lower tier councils), therefore rows duplicated

combined_results <- read.csv("combined_le_results.csv")
combined_results_minus_borough <- read.csv("combined_le_results_minus_borough.csv")

main_file <- main_file %>% arrange(lsoa11, interview_date)
combined_results <- combined_results %>% arrange(lsoa11, election_date)
combined_results_minus_borough <- combined_results_minus_borough %>% arrange(lsoa11, election_date)

main_file$interview_date <- as.Date(main_file$interview_date)
combined_results$election_date <- as.Date(combined_results$election_date)
combined_results_minus_borough$election_date <- as.Date(combined_results_minus_borough$election_date)

# Join and filter for main dataframe
main_file <- main_file %>%
  left_join(combined_results, by = "lsoa11")  %>%
  filter(interview_date > election_date) %>%
  group_by(lsoa11, interview_date) %>%
  filter(election_date == max(election_date)) %>% # Get the latest (max) election_date before interview_date for each group
  ungroup()

# For excluding borough elections
append_string <- "_borough"
exclude_column <- "lsoa11"

# Append string to every column except the specified one
combined_results_minus_borough <- combined_results_minus_borough %>%
  mutate(across(all_of(names(combined_results_minus_borough)[names(combined_results_minus_borough) != exclude_column]), ~paste0(., append_string)))

# Join and filter again
main_file <- main_file %>%
  left_join(combined_results_minus_borough, by = "lsoa11")  %>%
  filter(interview_date > election_date) %>%
  group_by(lsoa11, interview_date) %>%
  filter(election_date == max(election_date)) %>% # Get the latest (max) election_date before interview_date for each group
  ungroup()
