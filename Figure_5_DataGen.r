library(dplyr)
library(ggpubr)
library(heatwaveR)
library(readxl)
library(tools)
library(lubridate)

# Define the list of river folders
river_folders <- c(
   '0-Restigouche_DataGrass',  '0-Miramichi_DataGrass',  '0-MiramichiNW_DataGrass', 
   '0-Matapedia_DataGrass',  '0-Upsalquitch_DataGrass',
    '0-AuxMelezes_DataGrass', '0-Bear_DataGrass', '0-Bonaventure_DataGrass',
    '0-Casca_DataGrass', '0-Conne_DataGrass', '0-Dartmouth_DataGrass',
    '0-Ducktrap_DataGrass', '0-Gilbert_DataGrass', '0-Godbout_DataGrass',
    '0-Gouffre_DataGrass', '0-Havre_DataGrass', '0-Highland_DataGrass',
    '0-Huile_DataGrass', '0-Jupiter_DataGrass', '0-Margaree_DataGrass',
    '0-Moisie_DataGrass', '0-Narragagus_DataGrass',
    '0-Natash_DataGrass', 
    '0-Ouelle_DataGrass',
    '0-Pcasca_DataGrass', 
    '0-Sackville_DataGrass', '0-Sheepscot_DataGrass', '0-SteAnne_DataGrass',
    '0-SteMarg_DataGrass', '0-StLewis_DataGrass', 
    '0-Wilmot_DataGrass', '0-Carruthers_DataGrass', '0-Nouvelle_DataGrass', '0-Reid_DataGrass', '0-West_DataGrass'
)

# Define model fields
model_fields <- c("M_CanESM5", "M_CMCC_ESM2", "M_MPI_ESM1_2_HR", "M_MPI_ESM1_2_LR", "M_NorESM2_LM", "M_NorESM2_MM")

# Define the scenarios
scenarios <- c("ssp370", "ssp585")

# Define the base path
base_path <- "C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/0-Cequeau_Cal"

# Loop over all river folders
for (river_folder in river_folders) {
  river_name <- gsub("0-", "", gsub("_DataGrass", "", river_folder))
  river_path <- file.path(base_path, river_folder, paste0('0-Pyceq_', river_name))


  # Set default threshold
  threshold_value <- 20
  
  # Adjust threshold for specific rivers
  if (river_folder == '0-Carruthers_DataGrass') {
      threshold_value <- 16
  } else if (river_folder == '0-Nouvelle_DataGrass') {
      threshold_value <- 18
  } else if (river_folder == '0-Reid_DataGrass') {
      threshold_value <- 19
  } else if (river_folder == '0-West_DataGrass') {
      threshold_value <- 16
  }
  
  # Loop over each model
  for (model_field in model_fields) {
    model_folder <- file.path(river_path, model_field)
    
    # Loop over each scenario
    for (scenario in scenarios) {
      # Create the Excel filename
      excel_filename <- file.path(model_folder, paste0('globo_rhw_', river_name, '_', scenario, '.xlsx'))
      
      # Check if the Excel file exists
      if (!file.exists(excel_filename)) {
        warning(paste("File", excel_filename, "does not exist. Skipping."))
        next
      }
      
      # Read the Excel file
      df <- read_excel(excel_filename)
      
      # Convert 't' to Date format
      df <- df %>%
        mutate(t = as.Date(t, format = "%Y-%m-%d"))
      
      # Convert tibble to data.frame
      df <- as.data.frame(df)
      
      # The tMax threshold
      tMax_clim <- ts2clm(data = df, y = temp, climatologyPeriod = c("1980-01-01", "2010-12-31"), pctile = 90)

      #Without Tmin flat
      # events <- detect_event(data = tMax_clim, y = temp, categories = TRUE, climatology = TRUE, S=FALSE)

      # # Or with Tmin :
      # # The tMin exceedance
      tMin_exc <- exceedance(data = df, y = temp, threshold =threshold_value)$threshold
      
      # #Detect events
      events <- detect_event(data = tMax_clim, y = temp, minDuration = 5, maxGap = 2, categories = TRUE, climatology = TRUE, S=FALSE)
                      #  threshClim2 = tMin_exc$exceedance, minDuration2 = 2, maxGap2 = 0)

      # Convert date columns to Date format
      events$event <- events$event %>%
        mutate(
          date_start = as.Date(date_start, origin = "1970-01-01"),
          date_peak = as.Date(date_peak, origin = "1970-01-01"),
          date_end = as.Date(date_end, origin = "1970-01-01")
        )

      # Filter events to include only those where date_peak is in June to September
      events$event <- events$event %>%
        filter(month(date_peak) %in% 6:9)


      # Extract the event data
      event_data <- events$event
      
      # Create a full sequence of years based on the data
      all_years <- seq(min(as.integer(format(df$t, "%Y"))), max(as.integer(format(df$t, "%Y"))))
      
      # Calculate yearly statistics with full years
      yearly_stats <- event_data %>%
        mutate(year = as.integer(format(date_start, "%Y"))) %>%
        group_by(year) %>%
        summarize(
          ann_mean = mean(intensity_mean_relThresh, na.rm = TRUE),
          ann_av_dur = mean(duration, na.rm = TRUE),
          ann_dur = sum(duration, na.rm = TRUE),
          ann_mod = sum(p_moderate, na.rm = TRUE),
          ann_str = sum(p_strong, na.rm = TRUE),
          ann_ex = sum(p_extreme, na.rm = TRUE),
          ann_sev = sum(p_severe, na.rm = TRUE),
          ann_winter = sum(season == "Winter", na.rm = TRUE),
          ann_spring = sum(season == "Spring", na.rm = TRUE),
          ann_summer = sum(season == "Summer", na.rm = TRUE),
          ann_fall = sum(season == "Fall", na.rm = TRUE),
          ann_winter_spring = sum(season == "Winter/Spring", na.rm = TRUE),
          ann_spring_summer = sum(season == "Spring/Summer", na.rm = TRUE),
          ann_summer_fall = sum(season == "Summer/Fall", na.rm = TRUE),
          ann_fall_winter = sum(season == "Fall/Winter", na.rm = TRUE),
          ann_winter_summer = sum(season == "Winter-Summer", na.rm = TRUE),
          ann_spring_fall = sum(season == "Spring-Fall", na.rm = TRUE),
          ann_summer_winter = sum(season == "Summer-Winter", na.rm = TRUE),
          ann_fall_spring = sum(season == "Fall-Spring", na.rm = TRUE),
          ann_year_round = sum(season == "Year-round", na.rm = TRUE),
          ann_cum_intensity = mean(intensity_cumulative_relThresh, na.rm = TRUE)
        ) %>%
        right_join(data.frame(year = all_years), by = "year") %>%
        mutate(
          ann_mean = ifelse(is.na(ann_mean), NA, ann_mean),
          ann_av_dur = ifelse(is.na(ann_av_dur), 0, ann_av_dur),
          ann_dur = ifelse(is.na(ann_dur), 0, ann_dur),
          ann_mod = ifelse(is.na(ann_mod), 0, ann_mod),
          ann_str = ifelse(is.na(ann_str), 0, ann_str),
          ann_ex = ifelse(is.na(ann_ex), 0, ann_ex),
          ann_sev = ifelse(is.na(ann_sev), 0, ann_sev),
          ann_winter = ifelse(is.na(ann_winter), 0, ann_winter),
          ann_spring = ifelse(is.na(ann_spring), 0, ann_spring),
          ann_summer = ifelse(is.na(ann_summer), 0, ann_summer),
          ann_fall = ifelse(is.na(ann_fall), 0, ann_fall),
          ann_winter_spring = ifelse(is.na(ann_winter_spring), 0, ann_winter_spring),
          ann_spring_summer = ifelse(is.na(ann_spring_summer), 0, ann_spring_summer),
          ann_summer_fall = ifelse(is.na(ann_summer_fall), 0, ann_summer_fall),
          ann_fall_winter = ifelse(is.na(ann_fall_winter), 0, ann_fall_winter),
          ann_winter_summer = ifelse(is.na(ann_winter_summer), 0, ann_winter_summer),
          ann_spring_fall = ifelse(is.na(ann_spring_fall), 0, ann_spring_fall),
          ann_summer_winter = ifelse(is.na(ann_summer_winter), 0, ann_summer_winter),
          ann_fall_spring = ifelse(is.na(ann_fall_spring), 0, ann_fall_spring),
          ann_year_round = ifelse(is.na(ann_year_round), 0, ann_year_round)
        ) %>%
        arrange(year) %>%
        ungroup()
      
      # Create the output CSV filename
      csv_filename <- file.path(model_folder, paste0('globo_rhw_', river_name, '_', scenario, '.csv'))
      
      # Write the yearly statistics to a CSV file
      write.csv(yearly_stats, csv_filename, row.names = FALSE)


      message(paste("Processed", csv_filename))
    }
  }
}




## Part 2

# Define model fields
model_fields <- c("M_CanESM5", "M_CMCC_ESM2", "M_MPI_ESM1_2_HR", "M_MPI_ESM1_2_LR", "M_NorESM2_LM", "M_NorESM2_MM")

# Define the base paths
base_path <- "C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/0-Cequeau_Cal"
output_path <- "C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode"

# Ensure the output directory exists
dir_create(output_path)

# Loop over each model to create directories and move files
for (model_field in model_fields) {
  # Create a folder for the model if it doesn't exist
  model_folder_path <- file.path(output_path, model_field)
  dir_create(model_folder_path)
  
  # Loop over all river folders to move CSV files
  for (river_folder in river_folders) {
    river_name <- gsub("0-", "", gsub("_DataGrass", "", river_folder))
    river_path <- file.path(base_path, river_folder, paste0('0-Pyceq_', river_name))
    
    # Loop over each scenario
    scenarios <- c("ssp370", "ssp585")
    for (scenario in scenarios) {
      # Define the source CSV filename
      csv_filename <- file.path(river_path, model_field, paste0('globo_rhw_', river_name, '_', scenario, '.csv'))
      
      # Check if the CSV file exists
      if (file_exists(csv_filename)) {
        # Define the destination path in the model folder
        dest_filename <- file.path(model_folder_path, paste0('globo_rhw_', river_name, '_', scenario, '.csv'))
        
        # Move the file to the model folder
        file_move(csv_filename, dest_filename)
        
        message(paste("Moved", csv_filename, "to", dest_filename))
      } else {
        warning(paste("File", csv_filename, "does not exist. Skipping."))
      }
    }
  }
}


## Part3
## River Name Extraction: The script extracts the river name from the CSV filenames.
# 'ID Assignment:
# It checks if the river name has already been assigned an ID.
# If not, it assigns a new ID and increments the id_counter.
# The river_id_map list stores the mapping of river names to their assigned IDs.
# Consistent Naming: When renaming the files, the script uses the same ID for files corresponding to the same river, 
# regardless of the scenario (ssp370 or ssp585).'

library(fs)

# Define model fields
model_fields <- c("M_CanESM5", "M_CMCC_ESM2", "M_MPI_ESM1_2_HR", "M_MPI_ESM1_2_LR", "M_NorESM2_LM", "M_NorESM2_MM")

# Define the base output path
output_path <- "C:/Users/Utilisateur/OneDrive - INRS/Documents/ilias/Woolway_Nature_MatlabCode"

# Initialize a list to store the ID assignments for each river
river_id_map <- list()

# Initialize the ID counter
id_counter <- 1

# Loop over each model folder
for (model_field in model_fields) {
  # Get the model folder path
  model_folder_path <- file.path(output_path, model_field)
  
  # List all CSV files in the model folder
  csv_files <- dir_ls(model_folder_path, glob = "*.csv")
  
  # Loop over each CSV file
  for (csv_file in csv_files) {
    # Extract the river name from the filename
    river_name <- sub("globo_rhw_(.*?)_ssp.*\\.csv", "\\1", basename(csv_file))
    
    # Check if this river has already been assigned an ID
    if (!river_name %in% names(river_id_map)) {
      # Assign a new ID to this river
      river_id_map[[river_name]] <- id_counter
      id_counter <- id_counter + 1
    }
    
    # Retrieve the assigned ID for this river
    river_id <- river_id_map[[river_name]]
    
    # Extract the scenario from the filename (e.g., 'ssp370' or 'ssp585')
    scenario <- ifelse(grepl("ssp370", csv_file), "ssp370", "ssp585")
    
    # Create the new filename with the ID
    new_filename <- paste0("globo_rhw_", river_id, "_", scenario, ".csv")
    new_file_path <- file.path(model_folder_path, new_filename)
    
    # Rename the file
    file_move(csv_file, new_file_path)
    
    # Print a message to indicate the renaming
    message(paste("Renamed", csv_file, "to", new_file_path))
  }
}

# Print the mapping of river names to IDs for reference
print(river_id_map)


# $AuxMelezes
# [1] 1

# $Bear
# [1] 2

# $Bonaventure
# [1] 3

# $Carruthers
# [1] 4

# $Casca
# [1] 5

# $Conne
# [1] 6

# $Dartmouth
# [1] 7

# $Ducktrap
# [1] 8

# $Gilbert
# [1] 9

# $Godbout
# [1] 10

# $Gouffre
# [1] 11

# $Havre
# [1] 12

# $Highland
# [1] 13

# $Huile
# [1] 14

# $Jupiter
# [1] 15

# $Margaree
# [1] 16

# $Matapedia
# [1] 17

# $MiramichiNW
# [1] 18

# $Miramichi
# [1] 19

# $Moisie
# [1] 20

# $Narragagus
# [1] 21

# $Natash
# [1] 22

# $Nouvelle
# [1] 23

# $Ouelle
# [1] 24

# $Pcasca
# [1] 25

# $Reid
# [1] 26

# $Restigouche
# [1] 27

# $Sackville
# [1] 28

# $Sheepscot
# [1] 29

# $SteAnne
# [1] 30

# $SteMarg
# [1] 31

# $StLewis
# [1] 32

# $Upsalquitch
# [1] 33

# $West
# [1] 34

# $Wilmot
# [1] 35

