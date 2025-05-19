# Load Necessary Libraries
library(dplyr)
library(ggplot2)
library(gamlss)
library(gamlss.dist)
library(lubridate)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(readr)
library(readxl)
library(openxlsx)
library(viridis)
library(plotly)
library(stringr)
library(cowplot)
library(htmlwidgets)
library(reshape2)
library(purrr)
library(RColorBrewer)

############################################################
# Load Correlation Results (Assuming 'combined_cor_table' is available)
############################################################

############################################################
# Frequency Modeling Based on Significant Indices
############################################################

# Initialize lists to store results and plots
model_info_df_freq <- data.frame()
plot_list_freq_2d <- list()
plot_list_freq_3d <- list()
plot_list_freq_line <- list()

all_indices <- unique(combined_cor_table$Index_Season)

############################################################# Stationary models #########################################'

# First, get the names of the plots
plot_names <- names(plot_list_freq_line)

# Initialize data frame to store model information
model_info_df_freq <- data.frame()

# Loop over each river in 'summer_rivers_results'
for (river_name in names(summer_rivers_results)) {
  message(paste("Processing river:", river_name))
  river_events_data <- summer_rivers_results[[river_name]]
  
  # Extract historical events data
  events_historical <- river_events_data[["historical_Historical"]]
  
  # Proceed only if events_historical is not NULL and has rows
  if (!is.null(events_historical) && nrow(events_historical) > 0) {
    events_historical$scenario <- "historical"
  } else {
    # If no historical data, skip to next river
    message(paste("No historical events data available for river:", river_name))
    next
  }
  
  # Combine historical data
  events_data <- events_historical
  
  # Proceed only if events_data has rows
  if (nrow(events_data) > 0) {
    # Convert date columns to Date format
    origin_date <- as.Date("1970-01-01")  # Adjust if different origin
    events_data <- events_data %>%
      mutate(
        date_peak = as.Date(date_peak, origin = origin_date),
        date_start = as.Date(date_start, origin = origin_date),
        date_end = as.Date(date_end, origin = origin_date)
      )
    
    # Add year column
    events_data <- events_data %>% mutate(year = year(date_start))
    
    # Filter events that occurred in summer months (June, July, August, September)
    summer_events <- events_data %>%
      filter(month(date_peak) %in% c(6, 7, 8, 9))
    
    # Calculate frequency per year
    frequency_data <- summer_events %>%
      group_by(year) %>%
      summarise(frequency = n(), .groups = 'drop')
    
    # Define the full range of years (1979-2020)
    full_years <- data.frame(year = 1979:2020)
    frequency_data <- full_years %>%
      left_join(frequency_data, by = "year") %>%
      mutate(frequency = ifelse(is.na(frequency), 0, frequency))
    
    # Remove rows with NA in frequency
    modeling_data <- frequency_data %>%
      filter(!is.na(frequency))
    
    # Check if there are enough data points
    if (nrow(modeling_data) < 1) {
      message(paste("  Not enough data after removing NAs for river:", river_name))
      next
    }
    
    # Fit the stationary model
    current_formula <- as.formula("frequency ~ 1")
    current_model_name <- "Stationary Model"
    
    current_model <- tryCatch(
      gamlss(
        formula = current_formula,
        family = PO(mu.link = "log"),
        data = modeling_data,
        control = gamlss.control(trace = FALSE)
      ),
      error = function(e) {
        message(paste("  Error fitting stationary model for river", river_name, "-", e$message))
        return(NULL)
      }
    )
    
    if (is.null(current_model)) {
      message(paste("  Failed to fit stationary model for river:", river_name))
      next
    }
    
    # Collect model information
    model_info_river <- data.frame(
      River = river_name,
      Covariates = "None",
      Model_Name = current_model_name,
      AIC = AIC(current_model),
      Formula = paste(deparse(current_formula), collapse = ""),
      Best_Model = "Yes",
      stringsAsFactors = FALSE
    )
    
    # Append model information to the main data frame
    model_info_df_freq <- rbind(model_info_df_freq, model_info_river)
    
  } else {
    message(paste("No events data available for river:", river_name))
    next
  }
} # End of river loop








########################################################## Multi covariate : Non stationary model  ##################################################33

########################### Climate Index + Time ###############################33
# Load Necessary Libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

plot_list_freq_3d <- list()

# Load 'indices_seasonal_wide' data (ensure it includes years up to 2024)
# Assuming 'indices_seasonal_wide' is loaded and available

############## Selected rivers and their corresponding best models FOR Single index 2 covariate##############: 
selected_rivers <- c("Dartmouth", "Carruthers", "Restigouche", "Huile", "Gilbert", "Highland",  "Ducktrap", "Miramichi", "Wilmot"
                     )#, "Bonaventure", "Casca")   
best_models <- list(
  "Dartmouth" = list(
    covariates = c("Time", "AO_ASO_1"),
    formula = frequency ~ Time + AO_ASO_1
  ),
  "Carruthers" = list(
    covariates = c("Time", "AO_ASO_1"),
    formula = frequency ~ Time + AO_ASO_1
  ),
  "Restigouche" = list(
    covariates = c("Time", "AO_ASO_1"),
    formula = frequency ~ Time + AO_ASO_1
  ),
  "Huile" = list(
    covariates = c("Time", "AO_ASO_1"),
    formula = frequency ~ Time + AO_ASO_1
  ),
  "Gilbert" = list(
    covariates = c("Time", "AO_ASO_1"),
    formula = frequency ~ Time + AO_ASO_1
  ),
  "Highland" = list(
    covariates = c("Time", "AO_JAS_1"),
    formula = frequency ~ Time + AO_JAS_1
  ),
  "Ducktrap" = list(
    covariates = c("Time", "AO_JAS_1"),
    formula = frequency ~ Time + AO_JAS_1
  ),
  "Miramichi" = list(
    covariates = c("Time", "NAO_OND_1"),
    formula = frequency ~ Time + NAO_OND_1
  ),
  "Wilmot" = list(
    covariates = c("Time", "NAO_OND_1"),
    formula = frequency ~ Time + NAO_OND_1
  )
)


############## Selected rivers and their corresponding best models: Two climate indexes as covariate ##############: 
# selected_rivers <- c("Bonaventure", "Casca", "AuxMelezes", "Reid", "Godbout", "Gouffre")
# best_models <- list(
#   "Bonaventure" = list(
#     covariates = c("NAO_DJF_1", "AO_ASO_1"),
#     formula = frequency ~ NAO_DJF_1 + AO_ASO_1
#   ),
#   "Casca" = list(
#     covariates = c("NAO_DJF_1", "AO_ASO_1"),
#     formula = frequency ~ NAO_DJF_1 + AO_ASO_1
#   ),
#   "AuxMelezes" = list(
#     covariates = c("NAO_MAM_0", "AO_DJF_1"),
#     formula = frequency ~ NAO_MAM_0 + AO_DJF_1
#   ),
#   "Reid" = list(
#     covariates = c("NAO_MAM_0", "AO_JAS_1"),
#     formula = frequency ~ NAO_MAM_0 + AO_JAS_1
#   ),
#   "Godbout" = list(
#     covariates = c("NAO_ASO_1", "AO_JAS_1"),
#     formula = frequency ~ NAO_ASO_1 + AO_JAS_1
#   ),
#   "Gouffre" = list(
#     covariates = c("SOI_SON_1", "NAO_JAS_1"),
#     formula = frequency ~ SOI_SON_1 + NAO_JAS_1
#   )
#   )

# Loop over each selected river
for (river_name in selected_rivers) {
  message(paste("Processing river:", river_name))
  river_events_data <- summer_rivers_results[[river_name]]
  
  # Extract historical events data
  events_historical <- river_events_data[["historical_Historical"]]
  
  # Proceed only if events_historical is not NULL and has rows
  if (!is.null(events_historical) && nrow(events_historical) > 0) {
    events_historical$scenario <- "historical"
  } else {
    # If no historical data, skip to next river
    message(paste("No historical events data available for river:", river_name))
    next
  }
  
  # Combine historical data
  events_data <- events_historical
  
  # Proceed only if events_data has rows
  if (nrow(events_data) > 0) {
    # Convert date columns to Date format (adjust origin if necessary)
    origin_date <- as.Date("1970-01-01")  # Adjust if different origin
    events_data <- events_data %>%
      mutate(
        date_peak = as.Date(date_peak, origin = origin_date),
        date_start = as.Date(date_start, origin = origin_date),
        date_end = as.Date(date_end, origin = origin_date)
      )
    
    # Add year column
    events_data <- events_data %>% mutate(year = year(date_start))
    
    # Filter events that occurred in summer months (June, July, August, September)
    summer_events <- events_data %>%
      filter(month(date_peak) %in% c(6, 7, 8, 9))
    
    # Calculate frequency per year
    frequency_data <- summer_events %>%
      group_by(year) %>%
      summarise(frequency = n(), .groups = 'drop')
    
    # **Prepare Data for Model Fitting (1979-2020)**
    full_years_fit <- data.frame(year = 1979:2020)
    frequency_data_fit <- full_years_fit %>%
      left_join(frequency_data, by = "year") %>%
      mutate(frequency = ifelse(is.na(frequency), 0, frequency))
    
    frequency_data_fit <- frequency_data_fit %>%
      mutate(Time = as.integer(year))
    
    modeling_data_fit <- frequency_data_fit %>%
      select(year, frequency, Time) %>%
      left_join(indices_seasonal_wide, by = c("year" = "Year"))
    
    # **Prepare Data for Prediction (1979-2024)**
    full_years_all <- data.frame(year = 1979:2024)
    frequency_data_all <- full_years_all %>%
      left_join(frequency_data, by = "year") %>%
      mutate(frequency = ifelse(is.na(frequency), 0, frequency))
    
    frequency_data_all <- frequency_data_all %>%
      mutate(Time = as.integer(year))
    
    modeling_data_all <- frequency_data_all %>%
      select(year, frequency, Time) %>%
      left_join(indices_seasonal_wide, by = c("year" = "Year"))
    
    # **Remove Rows with NA in Frequency or Covariates**
    best_covariates <- best_models[[river_name]]$covariates
    vars_in_model <- unique(c("frequency", best_covariates))
    modeling_data_fit <- modeling_data_fit %>%
      select(all_of(vars_in_model)) %>%
      na.omit()
    
    # **Check Data Sufficiency**
    if (nrow(modeling_data_fit) < 10) {
      message(paste("  Not enough data after removing NAs for river:", river_name))
      next
    }
    
    # **Fit the Model**
    model_formula <- best_models[[river_name]]$formula
    best_model <- tryCatch(
      gamlss(
        formula = model_formula,
        family = PO(mu.link = "log"),
        data = modeling_data_fit,
        control = gamlss.control(trace = FALSE)
      ),
      error = function(e) {
        message(paste("  Error fitting model for river", river_name, "-", e$message))
        return(NULL)
      }
    )
    
    if (is.null(best_model)) {
      message(paste("No valid model found for river:", river_name))
      next
    }
    
    # **Plotting**
    message(paste("Plotting for river:", river_name))
    message(paste("Best model is:", deparse(model_formula), "with covariates:", paste(best_covariates, collapse = ", ")))
    
    # **Use Data up to 2020 for Covariate Ranges**
    modeling_data_plot <- modeling_data_fit  # Data up to 2020 used for plotting ranges
    
    # **Generate Sequences for Covariates**
    covariates <- best_covariates  # Use the same as in Code 1
    covariate_ranges <- lapply(covariates, function(covariate) {
      range(modeling_data_plot[[covariate]], na.rm = TRUE)
    })
    covariate_sequences <- mapply(function(covariate, covariate_range) {
      seq(covariate_range[1], covariate_range[2], length.out = 100)
    }, covariates, covariate_ranges, SIMPLIFY = FALSE)
    names(covariate_sequences) <- covariates
    
    # **Create Prediction Grid**
    prediction_grid <- expand.grid(covariate_sequences)
    prediction_grid <- na.omit(prediction_grid)
    
    # **Add Other Covariates at Mean Values**
    other_covariates <- setdiff(vars_in_model, c("frequency", covariates))
    for (oc in other_covariates) {
      prediction_grid[[oc]] <- mean(modeling_data_plot[[oc]], na.rm = TRUE)
    }
    
    # **Add Squared Terms if Necessary**
    formula_vars <- all.vars(model_formula)
    for (var in formula_vars) {
      if (grepl("^I\\(", var)) {
        var_inside <- sub("^I\\((.*)\\)$", "\\1", var)
        prediction_grid[[var]] <- with(prediction_grid, eval(parse(text = var_inside)))
      }
    }
    
    # **Predict Quantiles**
    prediction_grid$lambda_t <- predict(best_model, 
                                        newdata = prediction_grid, 
                                        type = "response")
    prediction_grid$quantile_p90 <- qPO(0.75, mu = prediction_grid$lambda_t)
    
    # **Handle NA/Infinite Values**
    prediction_grid$quantile_p90[!is.finite(prediction_grid$quantile_p90)] <- NA
    prediction_grid <- prediction_grid %>% filter(!is.na(quantile_p90))
    
    # **Prepare Data for Plotting**
    prediction_grid_plot <- prediction_grid %>%
      select(all_of(c(covariates, "quantile_p90")))
    
    # **Ensure Variables are Numeric**
    for (covariate in covariates) {
      prediction_grid_plot[[covariate]] <- as.numeric(prediction_grid_plot[[covariate]])
    }
    prediction_grid_plot$quantile_p90 <- as.numeric(prediction_grid_plot$quantile_p90)
    
    # **Check for Sufficient Variation**
    sufficient_variation <- all(sapply(covariates, function(covariate) {
      length(unique(prediction_grid_plot[[covariate]])) >= 2
    }))
    
    if (!sufficient_variation) {
      message(paste("  Insufficient variation in covariates for plotting for river:", 
                    river_name, "combination:", paste(covariates, collapse = ", ")))
      next
    }
    
    # **Convert Grid to Matrix for z-values**
    x_varN <- transform_covariate(covariates[1])#covariates[1]
    y_varN <- transform_covariate(covariates[2])#covariates[2]
    x_var <- covariates[1]
    y_var <- covariates[2]
    x_seq <- covariate_sequences[[x_var]]
    y_seq <- covariate_sequences[[y_var]]
    
    z_matrix <- matrix(prediction_grid_plot$quantile_p90, nrow = length(y_seq), ncol = length(x_seq), byrow = TRUE)
    
    # **Adjust z_range**
    z_range <- range(prediction_grid_plot$quantile_p90, na.rm = TRUE)
    z_range[1] <- floor(z_range[1])
    z_range[2] <- ceiling(z_range[2])
    
    # **Define Discrete Integer Levels**
    breaks <- seq(z_range[1], z_range[2], by = 1)
    breaks <- unique(breaks)
    
    if (length(breaks) <= 1) {
      message(paste("  Not enough variation in quantile_p90 for plotting 3D plot for river:", river_name))
      next
    }
    
    # **Initialize the 3D Plot**
    p3d <- plot_ly()
    
    # **Add Surfaces for Each Discrete Level**
    for (i in 1:length(breaks)) {
      z_level <- z_matrix
      if (i < length(breaks)) {
        z_level[z_matrix < breaks[i] | z_matrix >= breaks[i + 1]] <- NA  # Define step range
      } else {
        # For the last break, include all values >= breaks[i]
        z_level[z_matrix < breaks[i]] <- NA
      }
      
      # Set remaining values to the current break level
      z_level[!is.na(z_level)] <- breaks[i]
      
      # Add surface for each discrete level
      p3d <- p3d %>%
        add_surface(
          x = x_seq,
          y = y_seq,
          z = z_level,
          colorscale = "RdOrYl",
          cmin = z_range[1],
          cmax = z_range[2],
          showscale = (i == length(breaks)),  # Show color scale on the last layer only
          colorbar = list(
            title = list(text = "Frequency (event)", font = list(size = 12)),
            len = 0.5,
            y = 0.5,
            tickvals = breaks,
            ticktext = breaks  # Use integer breaks as tick labels
          ),
          opacity = 0.4,
          hoverinfo = "none"
        )
    }
    
    # **Prepare Observed Data for Plotting**
    modeling_data_observed <- modeling_data_fit %>% filter(frequency > 0)
    
    # **Add Vertical Lines for Observed Points**
    for (i in 1:nrow(modeling_data_observed)) {
      p3d <- p3d %>%
        add_trace(
          x = rep(modeling_data_observed[[x_var]][i], 2),
          y = rep(modeling_data_observed[[y_var]][i], 2),
          z = c(0, modeling_data_observed$frequency[i]),
          type = 'scatter3d',
          mode = 'lines',
          line = list(color = 'blue', width = 2),
          showlegend = FALSE,
          hoverinfo = 'none',
          opacity = 0.6
        )
    }
    
    # **Add Observed Points as Blue Markers**
    p3d <- p3d %>%
      add_markers(
        data = modeling_data_observed,
        x = modeling_data_observed[[x_var]],
        y = modeling_data_observed[[y_var]],
        z = modeling_data_observed$frequency,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = "blue", size = 5, opacity = 0.6),
        showlegend = FALSE
      )
    
    # **Generate Predictions for 2021-2024**
    prediction_years <- 2021:2024
    prediction_data_future <- modeling_data_all %>%
      filter(year %in% prediction_years) %>%
      select(all_of(vars_in_model))
    
    # **Remove Rows with NA in Covariates**
    prediction_data_future <- prediction_data_future %>%
      na.omit()
    
    # **Check if There is Data to Predict**
    if (nrow(prediction_data_future) > 0) {
      # **Add Squared Terms if Necessary**
      for (var in formula_vars) {
        if (grepl("^I\\(", var)) {
          var_inside <- sub("^I\\((.*)\\)$", "\\1", var)
          prediction_data_future[[var]] <- with(prediction_data_future, eval(parse(text = var_inside)))
        }
      }
      
      # Predict frequency quantiles
      prediction_data_future$lambda_t <- predict(best_model, 
                                                 newdata = prediction_data_future, 
                                                 type = "response")
      prediction_data_future$quantile_p90 <- qPO(0.75, mu = prediction_data_future$lambda_t)
      
      # **Add Predicted Points to the Plot**



          # **Add Vertical Lines for Observed Points**
      for (i in 1:nrow(prediction_data_future)) {
        p3d <- p3d %>%
          add_trace(
            x = rep(prediction_data_future[[x_var]][i], 2),
            y = rep(prediction_data_future[[y_var]][i], 2),
            z = c(0, prediction_data_future$quantile_p90[i]),
            type = 'scatter3d',
            mode = 'lines',
            line = list(color = 'red', width = 2),
            showlegend = FALSE,
            hoverinfo = 'none',
            opacity = 0.6
          )
      }
      p3d <- p3d %>%
        add_markers(
          data = prediction_data_future,
          x = prediction_data_future[[x_var]],
          y = prediction_data_future[[y_var]],
          z = prediction_data_future$quantile_p90,
          type = "scatter3d",
          mode = "markers",
          marker = list(color = "red", size = 5, opacity = 0.6),
          showlegend = FALSE
        )
    }
    
    # **Finalize Plot Layout**
    p3d <- p3d %>%
      layout(
        title = paste("River:", river_name),# "- Model:", deparse(model_formula)),
        scene = list(
          xaxis = list(title = x_varN),
          yaxis = list(title = y_varN),
          zaxis = list(
            title = "Frequency (event)",
            range = c(0, max(
              max(modeling_data_observed$frequency, na.rm = TRUE),
              max(z_matrix, na.rm = TRUE),
              ifelse(exists("prediction_data_future"), max(prediction_data_future$quantile_p90, na.rm = TRUE), 0)
            ))
          )
        )
      )
    
    # **Store the 3D Plot**
    plot_name <- paste0(river_name, "_", paste(covariates, collapse = "_"), "_Best_Model_3D")
    plot_list_freq_3d[[plot_name]] <- p3d
  } else {
    message(paste("No events data available for river:", river_name))
    next
  }
}
