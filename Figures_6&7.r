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




###############################################################################
# GAMLSS Poisson Frequency Model with Parametric 
# Bootstrap Confidence Intervals (Single Covariate Case)
###############################################################################

#-----------------------
# 1) Load Necessary Libraries
#-----------------------
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

#-----------------------
# 2) Load or Define Data
#-----------------------
# Make sure these objects are loaded:
#   summer_rivers_results, indices_seasonal_wide, combined_cor_table

#-----------------------
# 3) Frequency Modeling Based on Significant Indices
#-----------------------

# Initialize lists to store model information and plots
model_info_df_freq  <- data.frame()
plot_list_freq_2d   <- list()
plot_list_freq_3d   <- list()
plot_list_freq_line <- list()

# Define the rivers for which you want single-covariate line plots
selected_rivers <- c("Sheepscot", "Narragagus", "SteAnne", "Jupiter",
                     "Moisie", "StLewis", "Ouelle", "SteMarg", "Matapedia")


# Loop over each river in your results
for (river_name in names(summer_rivers_results)) {
  message(paste("Processing river:", river_name))
  river_events_data <- summer_rivers_results[[river_name]]
  
  # Extract historical events data (assuming key "historical_Historical")
  events_historical <- river_events_data[["historical_Historical"]]
  if (!is.null(events_historical) && nrow(events_historical) > 0) {
    events_historical$scenario <- "historical"
  } else {
    message(paste("No historical events data for river:", river_name))
    next
  }
  
  # Use only historical data for modeling
  events_data <- events_historical
  
  if (nrow(events_data) > 0) {
    # Convert date columns (adjust origin if necessary)
    origin_date <- as.Date("1970-01-01")
    events_data <- events_data %>%
      mutate(
        date_peak  = as.Date(date_peak,  origin = origin_date),
        date_start = as.Date(date_start, origin = origin_date),
        date_end   = as.Date(date_end,   origin = origin_date)
      )
    
    # Add a year column (using the start date)
    events_data <- events_data %>% mutate(year = year(date_start))
    
    # Filter to summer months (June–September)
    summer_events <- events_data %>% filter(month(date_peak) %in% c(6,7,8,9))
    
    # Calculate event frequency per year
    frequency_data <- summer_events %>%
      group_by(year) %>%
      summarise(frequency = n(), .groups = 'drop')
    
    # Create a full year sequence (e.g., 1979–2020) and merge
    full_years <- data.frame(year = 1979:2020)
    frequency_data <- full_years %>%
      left_join(frequency_data, by = "year") %>%
      mutate(frequency = ifelse(is.na(frequency), 0, frequency))
    
    # Add a time covariate (here, simply the integer year)
    frequency_data <- frequency_data %>% mutate(Time = as.integer(year))
    
    # Prepare the modeling data
    modeling_data <- frequency_data %>% select(year, frequency, Time)
    
    # Merge with seasonal indices (assumes "Year" is the key in indices_seasonal_wide)
    modeling_data <- modeling_data %>%
      left_join(indices_seasonal_wide, by = c("year" = "Year"))
    
    # Remove rows with missing frequency
    modeling_data <- modeling_data %>% filter(!is.na(frequency))
    
    # Extract significant indices from combined_cor_table for the current river
    cor_results_river <- combined_cor_table %>%
      filter(River == river_name) %>%
      filter(P_value < 0.1)
    
    if (nrow(cor_results_river) > 0) {
      cor_results_river <- cor_results_river %>%
        mutate(abs_correlation = abs(Correlation),
               Base_Index = sub("_.*", "", Index_Season)) %>%
        arrange(P_value)
      
      # Select the top index per base index
      top_indices_per_base <- cor_results_river %>%
        group_by(Base_Index) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(P_value)
      
      # Select the top two indices (if available)
      top_indices <- top_indices_per_base$Index_Season[1:min(2, nrow(top_indices_per_base))]
      top_indices <- top_indices[!is.na(top_indices)]
    } else {
      top_indices <- character(0)
    }
    
    # Use "Time" alone if no significant indices found
    if (length(top_indices) == 0) {
      possible_covariates <- c("Time")
    } else {
      possible_covariates <- unique(c("Time", top_indices))
    }
    possible_covariates <- possible_covariates[possible_covariates %in% names(modeling_data)]
    if (length(possible_covariates) == 0) {
      message(paste("No significant indices for river:", river_name))
      next
    }
    
    # Build candidate covariate combinations:
    covariate_combinations <- list()
    covariate_combinations[[1]] <- c()         # Stationary model
    covariate_combinations[[2]] <- c("Time")     # Time-only model
    
    if (length(top_indices) == 1) {
      idx1 <- top_indices[1]
      covariate_combinations[[length(covariate_combinations)+1]] <- c(idx1)
      covariate_combinations[[length(covariate_combinations)+1]] <- c("Time", idx1)
    } else if (length(top_indices) >= 2) {
      idx1 <- top_indices[1]
      idx2 <- top_indices[2]
      covariate_combinations[[length(covariate_combinations)+1]] <- c(idx1)
      covariate_combinations[[length(covariate_combinations)+1]] <- c("Time", idx1)
      covariate_combinations[[length(covariate_combinations)+1]] <- c(idx2)
      covariate_combinations[[length(covariate_combinations)+1]] <- c("Time", idx2)
      covariate_combinations[[length(covariate_combinations)+1]] <- c(idx1, idx2)
      covariate_combinations[[length(covariate_combinations)+1]] <- c("Time", idx1, idx2)
    }
    
    # Data frame to accumulate model info for this river
    model_info_river <- data.frame()
    
    # Helper function: generate formulas with linear and quadratic terms
    generate_model_formulas <- function(covs) {
      if (length(covs) == 0) {
        return(list(formulas = list(as.formula("frequency ~ 1")),
                    names    = "Stationary Model"))
      }
      n_cov <- length(covs)
      term_types <- expand.grid(rep(list(c(0, 1)), n_cov))
      formulas <- list()
      fnames   <- character()
      
      for (i in seq_len(nrow(term_types))) {
        terms <- c()
        parts <- c()
        for (j in seq_len(n_cov)) {
          cov_j <- covs[j]
          is_quad <- term_types[i, j]
          if (is_quad == 0) {
            terms <- c(terms, cov_j)
            parts <- c(parts, paste0(cov_j, " Linear"))
          } else {
            terms <- c(terms, cov_j, paste0("I(", cov_j, "^2)"))
            parts <- c(parts, paste0(cov_j, " Quadratic"))
          }
        }
        formula_str <- paste("frequency ~", paste(terms, collapse = " + "))
        formulas[[length(formulas) + 1]] <- as.formula(formula_str)
        fnames <- c(fnames, paste(parts, collapse = ", "))
      }
      list(formulas = formulas, names = fnames)
    }
    
    # Loop over candidate covariate combinations
    for (covariates in covariate_combinations) {
      covariates <- covariates[!is.na(covariates)]
      cov_label <- if (length(covariates) == 0) "None" else paste(covariates, collapse = " + ")
      message(paste("River:", river_name, "- Covariate Combination:", cov_label))
      
      # Subset data to remove missing covariate values
      modeling_data_current <- modeling_data
      if (length(covariates) > 0) {
        modeling_data_current <- modeling_data_current %>%
          filter(!if_any(all_of(covariates), is.na))
      }
      modeling_data_current <- modeling_data_current %>% filter(!is.na(frequency))
      
      if (length(covariates) == 0 && nrow(modeling_data_current) >= 1) {
        message("  Proceeding with stationary model. N=", nrow(modeling_data_current))
      } else if (nrow(modeling_data_current) < 10) {
        message("  Not enough data after NA removal for combination:", cov_label)
        next
      }
      
      # Generate candidate formulas
      forms_list <- generate_model_formulas(covariates)
      model_forms <- forms_list$formulas
      model_names <- forms_list$names
      
      models     <- list()
      aic_values <- numeric(length(model_forms))
      
      for (i in seq_along(model_forms)) {
        frm   <- model_forms[[i]]
        fname <- model_names[i]
        var_in_model <- all.vars(frm)
        var_in_model <- var_in_model[!(var_in_model %in% c("frequency"))]
        tmp_data <- modeling_data_current %>%
          dplyr::select(all_of(c("frequency", var_in_model))) %>%
          na.omit()
        
        if (nrow(tmp_data) < 10) {
          message(paste("  Not enough data for model:", fname))
          models[[i]] <- NULL
          aic_values[i] <- NA
          next
        }
        
        fit <- tryCatch(
          gamlss(
            formula = frm,
            family  = PO(mu.link = "log"),
            data    = tmp_data,
            control = gamlss.control(trace = FALSE)
          ),
          error = function(e) {
            message(paste("  Error fitting", fname, "-", e$message))
            return(NULL)
          }
        )
        
        if (is.null(fit)) {
          models[[i]] <- NULL
          aic_values[i] <- NA
          next
        }
        
        models[[i]]   <- fit
        aic_values[i] <- AIC(fit)
        
        model_info_river <- rbind(
          model_info_river,
          data.frame(
            River       = river_name,
            Covariates  = cov_label,
            Model_Name  = ifelse(length(covariates) == 0, "Stationary Model", fname),
            AIC         = aic_values[i],
            Formula     = paste(deparse(frm), collapse = ""),
            Best_Model  = "No",
            stringsAsFactors = FALSE
          )
        )
      }
      
      valid_idx <- which(!sapply(models, is.null))
      models      <- models[valid_idx]
      model_names <- model_names[valid_idx]
      model_forms <- model_forms[valid_idx]
      aic_values  <- aic_values[valid_idx]
      
      if (length(models) == 0) {
        message("  No valid models for combo:", cov_label)
        next
      }
      
      num_params <- sapply(models, function(m) m$df.fit)
      ord        <- order(num_params)
      models      <- models[ord]
      model_names <- model_names[ord]
      model_forms <- model_forms[ord]
      aic_values  <- aic_values[ord]
      num_params  <- num_params[ord]
      
      best_model_in_combination       <- models[[1]]
      best_model_name_in_combination  <- model_names[1]
      best_model_formula_in_combination <- paste(deparse(model_forms[[1]]), collapse = "")
      best_aic_in_combination         <- aic_values[1]
      
      if (length(models) > 1) {
        for (j in 2:length(models)) {
          curr_mod  <- models[[j]]
          prev_mod  <- models[[j - 1]]
          D         <- 2 * (as.numeric(logLik(curr_mod)) - as.numeric(logLik(prev_mod)))
          df_diff   <- curr_mod$df.fit - prev_mod$df.fit
          p_value   <- pchisq(D, df = df_diff, lower.tail = FALSE)
          message(paste("  Deviance Test between",
                        model_names[j], "and", model_names[j - 1],
                        ": D =", round(D, 2), ", df =", df_diff,
                        ", p-value =", round(p_value, 4)))
          if (!is.na(p_value) && p_value < 0.05) {
            best_model_in_combination         <- curr_mod
            best_model_name_in_combination    <- model_names[j]
            best_model_formula_in_combination <- paste(deparse(model_forms[[j]]), collapse = "")
            best_aic_in_combination           <- aic_values[j]
          } else {
            break
          }
        }
      }
      
      model_info_river$Best_Model[
        model_info_river$River == river_name &
          model_info_river$Covariates == cov_label &
          model_info_river$Model_Name == best_model_name_in_combination
      ] <- "Yes"
      
      #-----------------------------
      # Plotting for Single Covariate Cases (Line Plot with Bootstrap CI)
      #-----------------------------
      if (river_name %in% selected_rivers && length(covariates) == 1) {
        covariate <- covariates[1]
        
        # A. Create prediction data over the range of the covariate.
        #    Here we build a data frame with one column named after the covariate.
        cov_range <- range(modeling_data_current[[covariate]], na.rm = TRUE)
        cov_seq   <- seq(cov_range[1], cov_range[2], length.out = 100)
        prediction_data <- data.frame(temp = cov_seq)
        names(prediction_data)[1] <- covariate
        
        # B. For any other covariates in the best model, set them to their mean value.
        other_covs <- setdiff(all.vars(formula(best_model_in_combination)),
                              c("frequency", covariate))
        for (oc in other_covs) {
          prediction_data[[oc]] <- mean(modeling_data_current[[oc]], na.rm = TRUE)
        }
        
        # IMPORTANT: Ensure a dummy "frequency" column exists.
        if (!("frequency" %in% names(prediction_data))) {
          prediction_data$frequency <- NA
        }
        # Instead of hard-coding column names, use the variables from the fitted model.
        best_formula <- formula(best_model_in_combination)
        newdata_sub <- prediction_data[, intersect(names(prediction_data), all.vars(best_formula)), drop = FALSE]
        
        # C. Obtain predicted mean and quantiles from the best model.
        prediction_data$lambda_t    <- predict(best_model_in_combination,
                                               newdata = newdata_sub,
                                               type = "response")
        prediction_data$quantile_p90 <- qPO(0.75, mu = prediction_data$lambda_t)
        prediction_data$quantile_p50 <- qPO(0.50, mu = prediction_data$lambda_t)
        
        modeling_data_plot <- modeling_data_current %>% filter(frequency >= 0)
        
        # D. Parametric Bootstrap for Confidence Intervals.
        var_in_best  <- all.vars(best_formula)
        model_data_for_fit <- modeling_data_current %>%
          dplyr::select(any_of(var_in_best)) %>%
          na.omit()
        
        mu_hat <- predict(best_model_in_combination,
                          newdata = model_data_for_fit,
                          type = "response")
        
        n_boot <- 200  # Increase if desired.
        # Initialize matrices to store predictions from each bootstrap replicate:
        boot_preds <- matrix(NA, nrow = n_boot, ncol = nrow(newdata_sub))  # For λ (mean) predictions
        boot_q50   <- matrix(NA, nrow = n_boot, ncol = nrow(newdata_sub))  # For the 50th quantile (qPO(0.50, mu))
        boot_q90   <- matrix(NA, nrow = n_boot, ncol = nrow(newdata_sub))  # For the 90th quantile (qPO(0.90, mu))

        set.seed(123)  # For reproducibility

        for (b in seq_len(n_boot)) {
          # Generate new responses using a Poisson distribution with means from mu_hat:
          Y_new <- rpois(length(mu_hat), lambda = mu_hat)
          
          # Create a new dataset using the original data (model_data_for_fit) but replacing the response:
          tmp_data <- model_data_for_fit
          tmp_data$frequency <- Y_new
          
          # Re-fit the model on the bootstrapped data:
          boot_model <- tryCatch(
            gamlss(
              formula = best_formula,
              family  = PO(mu.link = "log"),
              data    = tmp_data,
              control = gamlss.control(trace = FALSE)
            ),
            error = function(e) NULL
          )
          
          # If the model fit successfully, predict λ on the prediction grid:
          if (!is.null(boot_model)) {
            boot_preds[b, ] <- predict(boot_model, newdata = newdata_sub, type = "response")
            # Convert the bootstrapped λ predictions into quantiles:
            boot_q50[b, ]   <- qPO(0.50, mu = boot_preds[b, ])
            boot_q90[b, ]   <- qPO(0.75, mu = boot_preds[b, ])
          }
        }

        # Now, compute the bootstrap-based confidence intervals:
        # For λ (the mean):
        ci_lambda_lower <- apply(boot_preds, 2, quantile, probs = 0.025, na.rm = TRUE)
        ci_lambda_upper <- apply(boot_preds, 2, quantile, probs = 0.975, na.rm = TRUE)
        prediction_data$lambda_t_lower <- ci_lambda_lower
        prediction_data$lambda_t_upper <- ci_lambda_upper

        # For the 50th quantile:
        ci_q50_lower <- apply(boot_q50, 2, quantile, probs = 0.025, na.rm = TRUE)
        ci_q50_upper <- apply(boot_q50, 2, quantile, probs = 0.975, na.rm = TRUE)
        prediction_data$q50_lower <- ci_q50_lower
        prediction_data$q50_upper <- ci_q50_upper

        # For the 90th quantile:
        ci_q90_lower <- apply(boot_q90, 2, quantile, probs = 0.025, na.rm = TRUE)
        ci_q90_upper <- apply(boot_q90, 2, quantile, probs = 0.975, na.rm = TRUE)
        prediction_data$q90_lower <- ci_q90_lower
        prediction_data$q90_upper <- ci_q90_upper


        # Helper function to transform a covariate name
        transform_covariate <- function(name) {
          
          # This regex captures three groups: 
          # 1) Anything up to the first underscore,
          # 2) Anything between underscores,
          # 3) Either 0 or 1 at the end of the string.
          #
          # Examples:
          #   "AO_ASO_1" ->  x="AO", y="ASO", shift="1"
          #   "AO_ASO_0" ->  x="AO", y="ASO", shift="0"
          match_pattern <- "^(.*?)_(.*?)_(0|1)$"
          
          if (grepl(match_pattern, name)) {
            # Extract the groups
            groups <- sub(match_pattern, "\\1,\\2,\\3", name)
            parts <- strsplit(groups, ",")[[1]]
            
            x     <- parts[1]  # e.g., "AO"
            y     <- parts[2]  # e.g., "ASO"
            shift <- parts[3]  # e.g., "1" or "0"
            
            if (shift == "1") {
              # e.g., "AO_ASO_1" -> "AO(ASO⁻¹)"
              return(paste0(x, "(", y, "⁻¹)"))
            } else {
              # e.g., "AO_ASO_0" -> "AO(ASO)"
              return(paste0(x, "(", y, ")"))
            }
          } else {
            # If it doesn't match, return it unchanged
            return(name)
          }
        }

        
        # E. Create the line plot with a ribbon for the bootstrap CI.
        line_plot <- ggplot() +
          ## Ribbon for the predicted mean (λ) CI (in gray)
          # geom_ribbon(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]],
          #       ymin = lambda_t_lower,
          #       ymax = lambda_t_upper),
          #   alpha = 0.2,
          #   fill  = "gray50"
          # ) +
          ## Ribbon for the 90th quantile CI (in red)
          # geom_ribbon(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]],
          #       ymin = q90_lower,
          #       ymax = q90_upper),
          #   alpha = 0.05,
          #   fill  = "red"
          # ) +
          ## Dashed lines for 90% quantile CI
          # geom_line(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]], y = q90_lower, color = "CIs"),
          #   linetype = "dashed",
          #   color = "red",
          #   size=0.5
          # ) +
          # geom_line(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]], y = q90_upper, color = "CIs"),
          #   linetype = "dashed",
          #   color = "red",
          #   size=0.5
          # ) +
          ## Ribbon for the 50th quantile CI (in blue)
          # geom_ribbon(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]],
          #       ymin = q50_lower,
          #       ymax = q50_upper),
          #   alpha = 0.05,
          #   fill  = "blue"
          # ) +
          ## Dashed lines for 50% quantile CI
          # geom_line(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]], y = q50_lower, color = "CIs"),
          #   linetype = "dashed",
          #   color = "blue",
          #   size = 0.5
          # ) +
          # geom_line(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]], y = q50_upper, color = "CIs"),
          #   linetype = "dashed",
          #   color = "blue",
          #   size=0.5
          # ) +
          # Line for the point estimate of the 90th quantile
          geom_line(
            data = prediction_data,
            aes(x = .data[[covariate]], y = quantile_p90, color = "p = 0.75"),
            size = 1
          ) +
          # Line for the point estimate of the 50th quantile
          geom_line(
            data = prediction_data,
            aes(x = .data[[covariate]], y = quantile_p50, color = "p = 0.5"),
            size = 1
          ) +
          # # (Optional) Line for the predicted mean (λ) point estimate (dashed)
          # geom_line(
          #   data = prediction_data,
          #   aes(x = .data[[covariate]], y = lambda_t, color = "Mean"),
          #   size = 1,
          #   linetype = "dashed"
          # ) +
          # Observed data points
          geom_point(
            data = modeling_data_plot,
            aes(x = .data[[covariate]], y = frequency, color = "Observed Frequency"),
            alpha = 0.8,
            size = 1
          ) +
          labs(
            title = paste(river_name),
            x = transform_covariate(covariate),
            y = "Frequency (event)"
          ) +
          scale_color_manual(
            values = c("p = 0.75" = "red",
                      "p = 0.5" = "blue",
                      "Observed Frequency" = "black",
                      "CIs" = "black")
                      # ,
                      # "Mean" = "gray20")
          ) +
          theme_minimal() +
          theme(
            axis.text  = element_text(size = 10),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 12, hjust = 0.5),
            legend.title = element_blank(),
            legend.position = "bottom"
          )

        # Save the plot in the list and display it.
        plot_name <- paste0(river_name, "_", covariate, "_", best_model_name_in_combination, "_line")
        plot_list_freq_line[[plot_name]] <- line_plot
        print(line_plot)

      }
      
      # (If you have two- or three-covariate models, insert analogous plotting code here.)
      
    }  # End candidate covariate combination loop
    
    # Append the model information for this river
    model_info_df_freq <- rbind(model_info_df_freq, model_info_river)
    
  } else {
    message(paste("No events data available for river:", river_name))
    next
  }
}  # End river loop

#---------------------------------------------------
# 4) Save Model Information and Display Plots as Needed
#---------------------------------------------------
# Example: Save the model info to an Excel file (uncomment when ready)
# write.xlsx(model_info_df_freq, "Model_Info_Frequency_WithBootstrap.xlsx", rowNames = FALSE)

# Example: Combine and display selected single-covariate line plots using ggpubr
# library(ggpubr)
# selected_plot_names <- c("Sheepscot_Time_Stationary Model_line", "Moisie_Time Quadratic_line")
# selected_plots <- plot_list_freq_line[selected_plot_names]
# ggarrange(plotlist = selected_plots, ncol = 2, nrow = 2)

# These are the best model :
selected_plot_names <- c(
  "Sheepscot_AO_JAS_1_AO_JAS_1 Linear_line",
  "Narragagus_AO_JAS_1_AO_JAS_1 Linear_line",
  "SteAnne_AO_ASO_1_AO_ASO_1 Linear_line",
  "Jupiter_AO_ASO_1_AO_ASO_1 Linear_line",
  "Moisie_NAO_MAM_0_NAO_MAM_0 Linear_line",
  "StLewis_AO_SON_1_AO_SON_1 Linear_line",
  "Ouelle_SOI_SON_1_SOI_SON_1 Linear_line",
  "SteMarg_SOI_SON_1_SOI_SON_1 Linear_line",
  "Matapedia_AO_SON_1_AO_SON_1 Linear_line"
)

selected_rivers <- c("Sheepscot", "Narragagus", "SteAnne", "Jupiter",
                     "Moisie", "StLewis", "Ouelle", "SteMarg", "Matapedia")
# Filter the plots: we assume that the names of the plots in plot_list_freq_line 
# contain the river names (e.g., "Sheepscot_..."). We use grep to extract only those.
selected_plots <- plot_list_freq_line[grep(paste(selected_rivers, collapse="|"), names(plot_list_freq_line))][c(
  "Sheepscot_AO_JAS_1_AO_JAS_1 Linear_line",
  "Narragagus_AO_JAS_1_AO_JAS_1 Linear_line",
  "SteAnne_AO_ASO_1_AO_ASO_1 Linear_line",
  "Jupiter_AO_ASO_1_AO_ASO_1 Linear_line",
  "Moisie_NAO_MAM_0_NAO_MAM_0 Linear_line",
  "StLewis_AO_SON_1_AO_SON_1 Linear_line",
  "Ouelle_SOI_SON_1_SOI_SON_1 Linear_line",
  "SteMarg_SOI_SON_1_SOI_SON_1 Linear_line",
  "Matapedia_AO_SON_1_AO_SON_1 Linear_line")]

# Optionally, you can check the names of the selected plots:
print(names(selected_plots))

# Arrange the selected plots in a grid. Adjust ncol and nrow as desired.
combined_plot <- ggarrange(plotlist = selected_plots,
                           ncol = 2,   # e.g., two columns
                          #  ncol = 1,   # e.g., two columns
                           nrow = ceiling(length(selected_plots)/2),
                           common.legend = TRUE,
                           legend = "bottom")

# Optionally add an overall title
combined_plot <- annotate_figure(combined_plot,
                                #  top = text_grob("Single Teleconnection Covariate with 95% bootstrap confidence intervals", 
                                top = text_grob("Single Teleconnection Covariate", 
                                                 face = "bold", size = 14))

# Print the combined figure
print(combined_plot)






######################## Plot showing on x-axis the non-exceedance probabilities (0.5, 0.8, 0.9, 0.95, 0.98, 0.99) with associated CIs, and in y the
######################## quantiles (frequency of event) (y-axis). To do so, we need to fix the values of the covariates, so for example let's do it for 'SteMarg' river:
######################### StMarg best model here :  "SteMarg_SOI_SON_1_SOI_SON_1 Linear_line" by fixing the value of SOI_SON_1 to 0.5


###############################################################################
# Fit Two Models for Each River:
#   1) frequency ~ (teleconnection)  [blue lines/ribbon]
#   2) frequency ~ 1 (stationary)    [red lines/ribbon]
# Then compute Poisson quantiles at p-values = 50%,80%,90%,95%,98%,99%
# with parametric-bootstrap 95% CIs, and overlay them on a single plot.
###############################################################################


###############################################################################
# Quantile-Frequency Plot for Each River with Bootstrap CIs (with Observed Data)
# (Revised version: Subset data to only necessary columns to avoid NA issues,
#  and add observed frequency points to the plot.)
###############################################################################

# Load necessary libraries 
library(dplyr)
library(lubridate)
library(gamlss)
library(gamlss.dist)
library(ggplot2)
library(ggpubr)
library(scales)

# Helper function to transform a covariate name for plot labels
transform_covariate <- function(name) {
  # This regex captures three groups: 
  # 1) Anything up to the first underscore,
  # 2) Anything between underscores,
  # 3) Either 0 or 1 at the end of the string.
  match_pattern <- "^(.*?)_(.*?)_(0|1)$"
  if (grepl(match_pattern, name)) {
    groups <- sub(match_pattern, "\\1,\\2,\\3", name)
    parts <- strsplit(groups, ",")[[1]]
    x     <- parts[1]  # e.g., "AO"
    y     <- parts[2]  # e.g., "ASO"
    shift <- parts[3]  # e.g., "1" or "0"
    if (shift == "1") {
      return(paste0(x, "(", y, "⁻¹)"))
    } else {
      return(paste0(x, "(", y, ")"))
    }
  } else {
    return(name)
  }
}

# Best teleconnection model info for each river (from your earlier results)
best_model_info <- list(
  "Sheepscot"  = "AO_JAS_1",
  "Narragagus" = "AO_JAS_1",
  "SteAnne"    = "AO_ASO_1",
  "Jupiter"    = "AO_ASO_1",
  "Moisie"     = "NAO_MAM_0",
  "StLewis"    = "AO_SON_1",
  "Ouelle"     = "SOI_SON_1",
  "SteMarg"    = "SOI_SON_1"
)

# Fixed teleconnection values per river (Highfrequency values)
# fixed_index_values <- c(
#   "Sheepscot"  = -0.4,
#   "Narragagus" = -0.4,
#   "SteAnne"    = 0.5,
#   "Jupiter"    = 0.5,
#   "Moisie"     = -1,
#   "StLewis"    = 0.75,
#   "Ouelle"     = 2,
#   "SteMarg"    = 1.5
# )

# # For low frequency values
fixed_index_values <- c(
  "Sheepscot"  = 0.4,
  "Narragagus" = 0.4,
  "SteAnne"    = -0.6,
  "Jupiter"    = -0.4,
  "Moisie"     = 1,
  "StLewis"    = -0.75,
  "Ouelle"     = -2,
  "SteMarg"    = -2
)

p_values <- c(0.50, 0.75, 0.90, 0.95, 0.98, 0.99)  # Quantiles of interest
n_boot   <- 1000                                   # Number of bootstrap replicates

plot_list <- list()  # to store ggplots for each river

# Loop over each river
for (river_nm in names(best_model_info)) {
  covariate_nm <- best_model_info[[river_nm]]
  fixed_val    <- fixed_index_values[[river_nm]]  # teleconnection value to fix
  
  # 1) Build the data for this river
  events_df <- summer_rivers_results[[river_nm]][["historical_Historical"]]
  if (is.null(events_df) || nrow(events_df) == 0) {
    message("Skipping ", river_nm, ": no data")
    next
  }
  
  # Convert dates and add a year column; filter to summer months
  origin_date <- as.Date("1970-01-01")
  events_df <- events_df %>%
    mutate(
      date_peak  = as.Date(date_peak, origin = origin_date),
      date_start = as.Date(date_start, origin = origin_date),
      date_end   = as.Date(date_end,   origin = origin_date),
      year       = year(date_start)
    ) %>%
    filter(month(date_peak) %in% c(6,7,8,9))  # summer
  
  # Frequency per year
  freq_df <- events_df %>%
    group_by(year) %>%
    summarise(frequency = n(), .groups = "drop")
  
  # Merge with a full year range (1979–2020)
  full_yrs <- data.frame(year = 1979:2020)
  freq_df  <- full_yrs %>%
    left_join(freq_df, by = "year") %>%
    mutate(frequency = ifelse(is.na(frequency), 0, frequency))
  
  # Merge with seasonal indices (assumes indices_seasonal_wide is in your session)
  modeling_data <- freq_df %>%
    left_join(indices_seasonal_wide, by = c("year" = "Year"))
  
  # --- FIX: Ensure the teleconnection index column is numeric ---
  if (covariate_nm %in% names(modeling_data)) {
    modeling_data[[covariate_nm]] <- as.numeric(as.character(modeling_data[[covariate_nm]]))
  } else {
    message("For ", river_nm, ": Column ", covariate_nm, " not found; skipping.")
    next
  }
  # --------------------------------------------------------------------
  
  # Filter out rows missing frequency or the teleconnection index
  modeling_data <- modeling_data %>%
    filter(!is.na(frequency), !is.na(.data[[covariate_nm]]))
  
  # If there is very little variation in the teleconnection index, add jitter.
  if (length(unique(modeling_data[[covariate_nm]])) == 1) {
    message("Covariate ", covariate_nm, " for ", river_nm, " is constant. Adding jitter.")
    modeling_data[[covariate_nm]] <- modeling_data[[covariate_nm]] +
      rnorm(nrow(modeling_data), 0, 1e-6)
  }
  
  # IMPORTANT: Subset the data to only the columns needed for the model.
  modeling_data_fit <- modeling_data %>% 
    select(frequency, all_of(covariate_nm)) %>% 
    na.omit()
  
  if (nrow(modeling_data_fit) < 5) {
    message("Skipping ", river_nm, ": not enough data for fitting (n = ", nrow(modeling_data_fit), ").")
    next
  }
  
  ## Compute observed data points:
  # Here we compute an empirical non-exceedance probability for each observed frequency.
  df_obs <- modeling_data_fit %>% 
    arrange(frequency) %>% 
    mutate(p = (row_number() - 0.5) / n())
  
  ##--------------------------------------------------------------------------------
  ## (A) Fit the *Linear Covariate* model: frequency ~ [covariate]
  ##--------------------------------------------------------------------------------
  frm_linear <- as.formula(paste0("frequency ~ ", covariate_nm))
  fit_linear <- tryCatch(
    gamlss(frm_linear, data = modeling_data_fit, family = PO(mu.link = "log"),
           control = gamlss.control(trace = FALSE)),
    error = function(e) {
      message("Error in gamlss for ", river_nm, " with formula ", deparse(frm_linear), ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(fit_linear)) {
    message("Could not fit linear model for ", river_nm, "; skipping.")
    next
  }
  
  # Predict Poisson mean at the user-specified (fixed) teleconnection value
  fixed_df <- data.frame(x = fixed_val)
  names(fixed_df) <- covariate_nm
  lambda_lin <- predict(fit_linear, newdata = fixed_df, type = "response")
  
  # The point estimates of each Poisson quantile
  q_pt_lin <- qPO(p_values, mu = lambda_lin)
  
  # Parametric bootstrap for the linear (covariate) model
  model_data_linear <- model.frame(fit_linear)
  mu_orig_lin <- predict(fit_linear, newdata = model_data_linear, type = "response")
  
  boot_q_lin <- matrix(NA, nrow = n_boot, ncol = length(p_values))
  set.seed(123)
  for (b in seq_len(n_boot)) {
    Y_new <- rpois(length(mu_orig_lin), lambda = mu_orig_lin)
    tmp_data <- model_data_linear
    # Replace the response (assumed to be the first variable in the formula)
    resp_var <- all.vars(frm_linear)[1]  # "frequency"
    tmp_data[[resp_var]] <- Y_new
    
    boot_mod_lin <- tryCatch(
      gamlss(frm_linear, data = tmp_data, family = PO(mu.link = "log"),
             control = gamlss.control(trace = FALSE)),
      error = function(e) NULL
    )
    
    if (!is.null(boot_mod_lin)) {
      lam_b_lin <- predict(boot_mod_lin, newdata = fixed_df, type = "response")
      boot_q_lin[b, ] <- qPO(p_values, mu = lam_b_lin)
    }
  }
  # Keep only rows with at least one successful replicate
  valid_lin <- apply(boot_q_lin, 1, function(r) !all(is.na(r)))
  boot_q_lin <- boot_q_lin[valid_lin, , drop = FALSE]
  
  q_lin_lower <- apply(boot_q_lin, 2, quantile, probs = 0.025, na.rm = TRUE)
  q_lin_upper <- apply(boot_q_lin, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  df_lin <- data.frame(
    p       = p_values,
    q_est   = q_pt_lin,
    q_lower = q_lin_lower,
    q_upper = q_lin_upper
  )
  
  ##--------------------------------------------------------------------------------
  ## (B) Fit the *Stationary* model: frequency ~ 1
  ##--------------------------------------------------------------------------------
  # For the stationary model, we only need the "frequency" column.
  modeling_data_fit_stat <- modeling_data %>% 
    select(frequency) %>% 
    na.omit()
  
  frm_stat <- frequency ~ 1
  fit_stat <- tryCatch(
    gamlss(frm_stat, data = modeling_data_fit_stat, family = PO(mu.link = "log"),
           control = gamlss.control(trace = FALSE)),
    error = function(e) NULL
  )
  
  if (is.null(fit_stat)) {
    message("Could not fit stationary model for ", river_nm, ". Only plotting linear model.")
    fit_stat <- NULL
  }
  
  df_stat <- NULL
  if (!is.null(fit_stat)) {
    # For a stationary model the predicted mu is constant; we take the first value.
    mu_stat_all <- predict(fit_stat, type = "response")
    mu_stat <- mu_stat_all[1]
    
    q_pt_stat <- qPO(p_values, mu = mu_stat)
    
    model_data_stat <- model.frame(fit_stat)
    mu_orig_stat <- predict(fit_stat, newdata = model_data_stat, type = "response")
    
    boot_q_stat <- matrix(NA, nrow = n_boot, ncol = length(p_values))
    set.seed(123)
    for (b in seq_len(n_boot)) {
      Y_new <- rpois(length(mu_orig_stat), lambda = mu_orig_stat)
      tmp2 <- model_data_stat
      tmp2$frequency <- Y_new
      
      boot_mod_stat <- tryCatch(
        gamlss(frm_stat, data = tmp2, family = PO(mu.link = "log"),
               control = gamlss.control(trace = FALSE)),
        error = function(e) NULL
      )
      if (!is.null(boot_mod_stat)) {
        lam_b_stat <- predict(boot_mod_stat, type = "response")[1]
        boot_q_stat[b, ] <- qPO(p_values, mu = lam_b_stat)
      }
    }
    
    valid_stat <- apply(boot_q_stat, 1, function(r) !all(is.na(r)))
    boot_q_stat <- boot_q_stat[valid_stat, , drop = FALSE]
    q_stat_lower <- apply(boot_q_stat, 2, quantile, probs = 0.025, na.rm = TRUE)
    q_stat_upper <- apply(boot_q_stat, 2, quantile, probs = 0.975, na.rm = TRUE)
    
    df_stat <- data.frame(
      p       = p_values,
      q_est   = q_pt_stat,
      q_lower = q_stat_lower,
      q_upper = q_stat_upper
    )
  }
  
  ##--------------------------------------------------------------------------------
  ## (C) Build a Combined Plot for this River
  ##--------------------------------------------------------------------------------
  # We draw:
  # - Blue ribbon + dashed lines for the linear (nonstationary) model
  # - Red ribbon + dashed lines for the stationary model (if available)
  # - Observed data points (black circles)
  g <- ggplot(mapping = aes(x = p_values)) +
    # Linear model (nonstationary) ribbon and lines (blue)
    geom_ribbon(
      data = df_lin,
      aes(x = p, ymin = q_lower, ymax = q_upper),
      fill = "blue", alpha = 0.05
    ) +
    geom_line(
      data = df_lin,
      aes(x = p, y = q_est),
      color = "blue"
    ) +
    geom_line(
      data = df_lin,
      aes(x = p, y = q_lower),
      color = "blue", linetype = "dashed"
    ) +
    geom_line(
      data = df_lin,
      aes(x = p, y = q_upper),
      color = "blue", linetype = "dashed"
    ) +
    # Observed data points
    geom_point(
      data = df_obs,
      aes(x = p, y = frequency),
      color = "black", size = 2, alpha = 0.8
    )
  
  # Overlay stationary model (red) if available
  if (!is.null(df_stat)) {
    g <- g +
      geom_ribbon(
        data = df_stat,
        aes(x = p, ymin = q_lower, ymax = q_upper),
        fill = "red", alpha = 0.05
      ) +
      geom_line(
        data = df_stat,
        aes(x = p, y = q_est),
        color = "red"
      ) +
      geom_line(
        data = df_stat,
        aes(x = p, y = q_lower),
        color = "red", linetype = "dashed"
      ) +
      geom_line(
        data = df_stat,
        aes(x = p, y = q_upper),
        color = "red", linetype = "dashed"
      )
  }
  
  # Final plot styling
  g <- g +
    scale_x_continuous(
      trans  = scales::logit_trans(),
      breaks = p_values,
      labels = p_values,
      limits = c(0.5, 0.99)
    ) +
    labs(
      title = paste0(
        river_nm,
        "\nBlue = Nonstationary Model (", transform_covariate(covariate_nm), " = ", fixed_val, ")",
        if (!is.null(df_stat)) "\nRed = Stationary Model" else ""
      ),
      x = "Non-Exceedance Probability (p)",
      y = "Frequency (Event)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  plot_list[[river_nm]] <- g
}

# Finally, arrange all the plots in a grid (if any)
if (length(plot_list) > 0) {
  combined_plot <- ggarrange(plotlist = plot_list,
                             ncol = 2,
                             nrow = ceiling(length(plot_list) / 2))
  print(combined_plot)
} else {
  message("No valid fits produced any plots.")
}




