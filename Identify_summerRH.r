# Load necessary libraries
library(dplyr)
library(readxl)
library(heatwaveR)
library(ggplot2)
library(ggpubr)
library(trend)
library(Kendall)
library(tidyr)      # For data manipulation
library(lubridate)  # For date functions
library(stringr)    # For string manipulation
library(rlang)      # For tidy evaluation using !!sym()
library(ggnewscale) # For multiple fill scales

############################################################
# Configuration
############################################################


# Define the list of river folders
river_folders <- c('0-AuxMelezes_DataGrass', 
  '0-Sheepscot_DataGrass', '0-Narragagus_DataGrass', '0-Ducktrap_DataGrass',
  '0-Restigouche_DataGrass',
  '0-MiramichiNW_DataGrass',
  '0-Miramichi_DataGrass', 
  '0-Matapedia_DataGrass',  '0-Upsalquitch_DataGrass',
  '0-Dartmouth_DataGrass',
  '0-SteAnne_DataGrass',
  '0-Gilbert_DataGrass', 
  '0-Gouffre_DataGrass', '0-Nouvelle_DataGrass', 
  '0-Margaree_DataGrass', '0-Havre_DataGrass', '0-Sackville_DataGrass',
  '0-Moisie_DataGrass', 
  '0-Natash_DataGrass', '0-Godbout_DataGrass',
  '0-Ouelle_DataGrass', '0-SteMarg_DataGrass',
  '0-Conne_DataGrass', '0-Highland_DataGrass', '0-StLewis_DataGrass', '0-Reid_DataGrass',
  '0-Huile_DataGrass', '0-Jupiter_DataGrass',
  '0-Bonaventure_DataGrass',  '0-Casca_DataGrass',  '0-Pcasca_DataGrass',  '0-Wilmot_DataGrass',
  '0-Carruthers_DataGrass','0-West_DataGrass', '0-Bear_DataGrass'
)


# Define model fields
model_fields <- c(
  "M_CanESM5", "M_CMCC_ESM2", "M_MPI_ESM1_2_HR", 
  "M_MPI_ESM1_2_LR", "M_NorESM2_LM", "M_NorESM2_MM"
)

# Define the scenarios (future scenarios only)
scenarios <- c("ssp370", "ssp585")

# Define the base path
base_path <- "C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/0-Cequeau_Cal"

# Define a fixed climatology period to be used for all analyses
fixed_climatology_start <- as.Date("1980-01-01")
fixed_climatology_end <- as.Date("2010-12-31")

# Define periods for event detection
periods <- list(
  Historical = list(
    start = as.Date("1979-01-01"), 
    end = as.Date("2020-12-31"),
    name = "Historical"
  ),
  Future1 = list(
    start = as.Date("2021-01-01"), 
    end = as.Date("2060-12-31"),
    name = "Future1"
  ),
  Future2 = list(
    start = as.Date("2061-01-01"), 
    end = as.Date("2099-12-31"),
    name = "Future2"
  )
)

# Define default and specific threshold values for rivers
default_threshold <- 20
threshold_adjustments <- list(
  '0-Carruthers_DataGrass' = 16,
  '0-Nouvelle_DataGrass' = 18,
  '0-Reid_DataGrass' = 19,
  '0-West_DataGrass' = 16
)

############################################################
# Helper Functions
############################################################

# Function to read model data
read_model_data <- function(model_folder, river_name, scenario) {
  excel_filename <- file.path(model_folder, paste0('globo_rhw_', river_name, '_', scenario, '.xlsx'))
  
  if (!file.exists(excel_filename)) {
    warning(paste("File", excel_filename, "does not exist. Skipping model:", basename(model_folder)))
    return(NULL)
  }
  
  # Read the Excel file
  df <- tryCatch(
    read_excel(excel_filename),
    error = function(e) {
      warning(paste("Error reading", excel_filename, ":", e$message))
      return(NULL)
    }
  )
  
  if (is.null(df)) return(NULL)
  
  # Ensure the date column is formatted correctly and rename temperature column
  df <- df %>%
    mutate(t = as.Date(t, format = "%Y-%m-%d")) %>%
    rename(temp = 2) %>%  # Assumes the second column is temperature
    select(t, temp)
  
  return(df)
}

# Function to adjust threshold based on river
adjust_threshold <- function(river_folder, default_threshold, threshold_adjustments) {
  if (river_folder %in% names(threshold_adjustments)) {
    return(threshold_adjustments[[river_folder]])
  } else {
    return(default_threshold)
  }
}

############################################################
# Main Processing Loop
############################################################

# Initialize an empty list to store all events
all_rivers_results <- list()

for (river_folder in river_folders) {
  river_name <- gsub("0-", "", gsub("_DataGrass", "", river_folder))
  river_path <- file.path(base_path, river_folder, paste0('0-Pyceq_', river_name))
  
  # Set threshold value for the current river
  threshold_value <- adjust_threshold(river_folder, default_threshold, threshold_adjustments)
  
  message(paste("Processing river:", river_name, "with threshold:", threshold_value))
  
  # Initialize a list to hold events data for the current river
  river_events_data <- list()
  
  # Loop over each scenario
  for (scenario in scenarios) {
    message(paste("  Scenario:", scenario))
    
    model_data_list <- list()
    
    # Loop over each model
    for (model_field in model_fields) {
      model_folder <- file.path(river_path, model_field)
      df <- read_model_data(model_folder, river_name, scenario)
      
      if (!is.null(df)) {
        df$model <- model_field
        model_data_list[[model_field]] <- df
      }
    }
    
    # Check if any model data was successfully read
    if (length(model_data_list) == 0) {
      warning(paste("    No valid model data found for river:", river_name, "and scenario:", scenario))
      # Assign NA trends for all applicable periods
      applicable_periods <- if (scenario %in% scenarios) c("Future1", "Future2") else "Historical"
      for (period_name in applicable_periods) {
        key <- paste0(scenario, "_", period_name)
        river_events_data[[key]] <- NULL  # No events
        message(paste("    No events detected for scenario:", scenario, "and period:", period_name))
      }
      next
    }
    
    # Combine data from all models and calculate daily mean temperature
    combined_data <- bind_rows(model_data_list) %>%
      group_by(t) %>%
      summarise(temp = mean(temp, na.rm = TRUE)) %>%
      ungroup()
    
    # Apply ts2clm() to the entire time series with fixed climatology period
    clm_obj <- tryCatch(
      ts2clm(
        data = combined_data,
        y = temp,
        climatologyPeriod = c("1980-01-01", "2010-12-31"),  # Fixed climatology period
        pctile = 90
      ),
      error = function(e) {
        warning(paste("    ts2clm failed for river:", river_name, "and scenario:", scenario, "-", e$message))
        return(NULL)
      }
    )
    
    if (is.null(clm_obj)) {
      # Assign NA trends for all applicable periods
      applicable_periods <- if (scenario %in% scenarios) c("Future1", "Future2") else "Historical"
      for (period_name in applicable_periods) {
        key <- paste0(scenario, "_", period_name)
        river_events_data[[key]] <- NULL  # No events
        message(paste("    No events detected for scenario:", scenario, "and period:", period_name))
      }
      next
    }
    
    # Calculate exceedance threshold based on fixed climatology
    tMin_exc <- tryCatch(
      exceedance(
        data = combined_data,
        y = temp,
        threshold = threshold_value,
      )$threshold,
      error = function(e) {
        warning(paste("    exceedance calculation failed for river:", river_name, "and scenario:", scenario, "-", e$message))
        return(NULL)
      }
    )
    
    if (is.null(tMin_exc)) {
      # Assign NA trends for all applicable periods
      applicable_periods <- if (scenario %in% scenarios) c("Future1", "Future2") else "Historical"
      for (period_name in applicable_periods) {
        key <- paste0(scenario, "_", period_name)
        river_events_data[[key]] <- NULL  # No events
        message(paste("    No events detected for scenario:", scenario, "and period:", period_name))
      }
      next
    }
    
    # Detect heatwave events across the entire time series
    events <- tryCatch(
      detect_event(
        data = clm_obj,
        y = combined_data$temp,
        minDuration = 5,
        maxGap = 2,
        categories = TRUE,
        climatology = TRUE,
        S = FALSE
        # threshClim2 = tMin_exc$exceedance,
        # minDuration2 = 2,
        # maxGap2 = 0
      ),
      error = function(e) {
        warning(paste("    Event detection failed for river:", river_name, "and scenario:", scenario, "-", e$message))
        return(NULL)
      }
    )
    
    # Extract the event data
    if (!is.null(events$event) && nrow(events$event) > 0) {
      # Assign scenario and river to the detected events
      events$event <- events$event %>%
        mutate(
          river = river_name
        )
      
      # Assign period based on 'date_start' date
      events$event <- events$event %>%
        mutate(
          period = case_when(
            date_start >= periods$Historical$start & date_start <= periods$Historical$end ~ "Historical",
            date_start >= periods$Future1$start & date_start <= periods$Future1$end ~ "Future1",
            date_start >= periods$Future2$start & date_start <= periods$Future2$end ~ "Future2",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(period))  # Remove events that don't fall into any period
      
      if (nrow(events$event) > 0) {
        # Split events by period and assign correct scenario
        events_split <- split(events$event, events$event$period)
        
        for (period_name in names(events_split)) {
          if (period_name == "Historical") {
            # For Historical period, assign 'historical' scenario and unique key
            events_split[[period_name]]$scenario <- "historical"
            key <- paste0("historical", "_", period_name)
          } else {
            # For Future periods, assign the current scenario and respective key
            events_split[[period_name]]$scenario <- scenario
            key <- paste0(scenario, "_", period_name)
          }
          
          # Store the events data under the appropriate key
          river_events_data[[key]] <- events_split[[period_name]]
          message(paste("    Detected", nrow(events_split[[period_name]]), "events for scenario:", events_split[[period_name]]$scenario[1], "and period:", period_name))
        }
      } else {
        # No events detected within defined periods
        applicable_periods <- if (scenario %in% scenarios) c("Future1", "Future2") else "Historical"
        for (period_name in applicable_periods) {
          key <- paste0(scenario, "_", period_name)
          river_events_data[[key]] <- NULL  # No events
          message(paste("    No events detected within defined periods for scenario:", scenario, "and period:", period_name))
        }
      }
    } else {
      # No heatwave events detected for this scenario
      applicable_periods <- if (scenario %in% scenarios) c("Future1", "Future2") else "Historical"
      for (period_name in applicable_periods) {
        key <- paste0(scenario, "_", period_name)
        river_events_data[[key]] <- NULL  # No events
        message(paste("    No heatwave events detected for river:", river_name, "and scenario:", scenario, "and period:", period_name))
      }
    }
  }
  
  # Store the events data for the river
  all_rivers_results[[river_name]] <- river_events_data
}
