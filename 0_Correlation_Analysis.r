# Load necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(purrr)

############################################################
# Helper Function: Compute Correlation Matrix P-values
############################################################

# Function to compute correlation matrix p-values using Pearson's correlation
cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  diag(p_mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "pearson")
      p_mat[i, j] <- p_mat[j, i] <- tmp$p.value
    }
  }
  colnames(p_mat) <- rownames(p_mat) <- colnames(mat)
  return(p_mat)
}

############################################################
# Function to detrend data
############################################################

# Function to detrend each column in a data frame
detrend_data <- function(df, time_var = "Time", exclude_vars = NULL) {
  df_detrended <- df
  vars_to_detrend <- setdiff(names(df), c(time_var, exclude_vars))
  for (var in vars_to_detrend) {
    # Fit linear model of var ~ Time
    lm_fit <- lm(df[[var]] ~ df[[time_var]], data = df)
    # Store residuals as detrended variable
    df_detrended[[var]] <- residuals(lm_fit)
  }
  return(df_detrended)
}

############################################################
# Step 1: Read and Process Climate Indices Data
############################################################

# List of climate indices and their file names
indices_files <- list(
  AMO = "amo_Monthly_unsmoothed.txt",
  NAO = "nao_Monthly.txt",
  SOI = "soi_Monthly.txt",
  # PDO = "pdo_Monthly.txt",
  # PNA = "pna_Monthly.txt",
  AO = "ao_Monthly.txt",
  ONI = "oni_Monthly.txt",
  Nino = "Nino34_Monthly.txt"
)

# Read and reshape all indices to long format
indices_long_list <- lapply(names(indices_files), function(index_name) {
  data <- read.table(indices_files[[index_name]], header = FALSE)
  colnames(data) <- c("Year", paste0("M", 1:12))
  data_long <- data %>%
    pivot_longer(
      cols = starts_with("M"),
      names_to = "Month",
      names_prefix = "M",
      values_to = index_name
    ) %>%
    mutate(Month = as.integer(Month))
  return(data_long)
})

# Combine all indices data
indices_monthly <- reduce(
  indices_long_list,
  full_join,
  by = c("Year", "Month")
)

############################################################
# Step 2: Calculate Moving 3-Month Average Indices for Acceptable Seasons
############################################################

# Define acceptable seasons and their months
seasons_list <- list(
  "JAS_1" = list(months = c(7,8,9), year_adjustment = c(-1, -1, -1)),
  "ASO_1" = list(months = c(8,9,10), year_adjustment = c(-1, -1, -1)),
  "SON_1" = list(months = c(9,10,11), year_adjustment = c(-1, -1, -1)),
  "OND_1" = list(months = c(10,11,12), year_adjustment = c(-1, -1, -1)),
  "NDJ_1" = list(months = c(11,12,1), year_adjustment = c(-1, -1, 0)),
  "DJF_1" = list(months = c(12,1,2), year_adjustment = c(-1, 0, 0)),
  "JFM_0" = list(months = c(1,2,3), year_adjustment = c(0, 0, 0)),
  "FMA_0" = list(months = c(2,3,4), year_adjustment = c(0, 0, 0)),
  "MAM_0" = list(months = c(3,4,5), year_adjustment = c(0, 0, 0))
)

# Prepare a list to store indices_seasonal data
indices_seasonal_list <- list()

# Get the years for which we have indices data
years <- unique(indices_monthly$Year)
max_year_indices <- max(indices_monthly$Year)

# For years from 1979 to 2020
for (year in 1979:max_year_indices) {
  # For each season
  for (season_name in names(seasons_list)) {
    season_info <- seasons_list[[season_name]]
    months <- season_info$months
    year_adjustments <- season_info$year_adjustment
    
    # Adjusted years for the months
    years_for_months <- year + year_adjustments
    
    # Create a data frame with the months and adjusted years
    months_years <- data.frame(
      Month = months,
      Year = years_for_months
    )
    
    # Merge with indices_monthly to get the indices for these months
    indices_for_season <- indices_monthly %>%
      filter((Year %in% months_years$Year) & (Month %in% months_years$Month))
    
    # Merge with months_years to ensure correct matching
    indices_for_season <- merge(months_years, indices_for_season, by = c("Year", "Month"), all.x = TRUE)
    
    # Compute the mean of each index
    season_mean <- indices_for_season %>%
      summarise(across(AMO:Nino, ~ mean(.x, na.rm = TRUE)))
    
    # Add season name and year
    season_mean <- cbind(Year = year, Season = season_name, season_mean)
    
    # Append to indices_seasonal_list
    indices_seasonal_list <- append(indices_seasonal_list, list(season_mean))
  }
}

# Combine all the data frames into one
indices_seasonal <- do.call(rbind, indices_seasonal_list)

# Reshape indices_seasonal to wide format
indices_seasonal_wide <- indices_seasonal %>%
  pivot_longer(cols = AMO:Nino, names_to = "Index", values_to = "Value") %>%
  unite("Index_Season", Index, Season, sep = "_") %>%
  pivot_wider(names_from = Index_Season, values_from = Value)

############################################################
# Step 3: Initialize Lists to Store Results
############################################################

# Initialize a list to store correlation tables
cor_tables_list <- list()

############################################################
# Step 4: Loop Over Each River in summer_rivers_results
############################################################

# Assuming summer_rivers_results is already loaded in the environment
# Loop over each river
for (river_name in names(summer_rivers_results)) {
  message(paste("Processing river:", river_name))
  
  river_events_data <- summer_rivers_results[[river_name]]
  
  if ("historical_Historical" %in% names(river_events_data)) {
    events_all <- river_events_data[["historical_Historical"]]
  } else {
    message(paste("No historical data for river:", river_name))
    next
  }
  
  if (is.null(events_all) || nrow(events_all) == 0) {
    message(paste("No events data available for river:", river_name))
    next
  }
  
  # Convert date columns to Date format (assuming 'date_peak' is in numeric days since origin)
  origin_date <- as.Date("1970-01-01")  # Adjust if different origin
  events_all <- events_all %>%
    mutate(
      date_peak = as.Date(date_peak, origin = origin_date),
      year = year(date_peak)
    )
  
  # Filter events between 1979 and 2020
  events_all <- events_all %>%
    filter(year >= 1979 & year <= 2020)
  
  ############################################################
  # Step 5: Compute Heatwave Frequency in Summer (JJAS)
  ############################################################
  
  # Filter events that occurred in summer months (June, July, August, September)
  summer_events <- events_all %>%
    filter(month(date_peak) %in% c(6, 7, 8, 9))
  
  # Compute annual heatwave frequency during summer
  annual_frequency <- summer_events %>%
    group_by(year) %>%
    summarise(
      Frequency = n(),  # Number of events
      .groups = "drop"
    )
  
  # Ensure all years between 1979 and 2020 are represented
  all_years <- data.frame(year = 1979:2020)
  annual_frequency <- all_years %>%
    left_join(annual_frequency, by = "year") %>%
    mutate(Frequency = ifelse(is.na(Frequency), 0, Frequency))
  
  # Add Time (year) as a covariate
  annual_frequency <- annual_frequency %>%
    mutate(Time = year)
  
  # Detrend heatwave frequency
  annual_frequency_detrended <- detrend_data(annual_frequency, time_var = "Time", exclude_vars = "year")
  
  ############################################################
  # Step 6: Prepare Data for Correlation Analysis
  ############################################################
  
  # Merge annual frequency with seasonal indices
  correlation_data <- annual_frequency_detrended %>%
    left_join(indices_seasonal_wide, by = c("year" = "Year"))
  
  # Remove 'Time' and 'year' from variables used for correlation
  variables_for_correlation <- correlation_data %>%
    select(-Time, -year)
  
  # Check for any NA values and handle them
  variables_for_correlation <- variables_for_correlation %>%
    drop_na()
  
  # Ensure there is enough data to compute correlation
  if (nrow(variables_for_correlation) < 3) {
    message(paste("Not enough data for correlation analysis for river:", river_name))
    next
  }
  
  ############################################################
  # Step 7: Perform Correlation Analysis
  
  # Compute correlation matrix and p-values using Pearson's correlation
  cor_matrix <- cor(variables_for_correlation, use = "pairwise.complete.obs", method = "pearson")
  cor_pvalues <- cor.mtest(variables_for_correlation)
  
  # Extract correlations between Frequency and indices
  indices_names <- names(variables_for_correlation)[-which(names(variables_for_correlation) == "Frequency")]
  cor_results <- data.frame(
    River = river_name,
    Index_Season = indices_names,
    Correlation = cor_matrix["Frequency", indices_names],
    P_value = cor_pvalues["Frequency", indices_names],
    stringsAsFactors = FALSE
  )
  
  # Format correlation coefficients
  cor_results$Correlation_formatted <- sprintf("%.2f", cor_results$Correlation)
  
  # Apply bold formatting to significant correlations (p < 0.1)
  significant_indices <- which(!is.na(cor_results$P_value) & cor_results$P_value < 0.1)
  cor_results$Correlation_formatted[significant_indices] <- paste0("**", cor_results$Correlation_formatted[significant_indices], "**")
  
  # Store the correlation table for this river
  cor_tables_list[[river_name]] <- cor_results
}

# Combine all correlation tables into one data frame
combined_cor_table <- bind_rows(cor_tables_list)
