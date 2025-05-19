library(dplyr)
library(ggpubr)
library(heatwaveR)


# Load necessary libraries
library(dplyr)
library(readxl)

# Read the Excel file
df <- read_excel("globo_rhw_Miramichi_ssp370.xlsx")

# Rename the columns
# colnames(df) <- c("t", "Temp")

# Convert 't' to Date format
df <- df %>%
  mutate(t = as.Date(t, format = "%Y-%m-%d"))

# Check the first few rows of the final data frame
# head(df)

# Verify the structure
# str(df)

# Convert tibble to data.frame
df <- as.data.frame(df)

# Print the head of the data frame
head(df)

# The tMax threshold
tMax_clim <- ts2clm(data = df, y = temp, climatologyPeriod = c("1980-01-01", "2010-12-31"), pctile = 90)

# The tMin exceedance
tMin_exc <- exceedance(data = df, y = temp, threshold =20)$threshold # nolint #, minDuration = 2, maxGap = 0

# The 90th percentile threshold # nolint
events <- detect_event(data = tMax_clim, y = temp, minDuration = 5, maxGap = 2, categories = TRUE, climatology = TRUE, S=FALSE)

# The code to create a bubble plot for the heatwave results
bubble_plot <- ggplot(data = events$event, aes(x = date_peak, y = intensity_max_relThresh)) + 
  geom_point(aes(size = intensity_cumulative_relThresh), shape = 21, fill = "salmon", alpha = 0.8) + 
  labs(x = NULL, y = "Maximum Intensity [°C] ", size = "Cumulative Intensity [°C x days]") + 
  scale_size_continuous(range = c(1, 10)) + # nolint
  theme_bw() +
  theme(legend.position = c(0.3, 0.8),
        legend.box.background = element_rect(colour = "black"),
        plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", linewidth = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black"))

plot(ggarrange(event_line(events, y = temp, metric = 'intensity_max_relThresh',
                       start_date = "2024-05-01",
                       end_date = "2024-11-01")+ # nolint
                      theme_bw() +
                      theme(legend.position = c(0.8, 0.8),
                            legend.box.background = element_rect(colour = "black"),
                            plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", linewidth = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black")),
          event_line(events, y = temp, metric = 'intensity_max_relThresh', category = TRUE,
                       start_date = "2024-05-01",
                       end_date = "2024-11-01")+ # nolint
                      theme_bw() +
                      theme(legend.position = c(0.8, 0.7),
                            legend.box.background = element_rect(colour = "black"),
                            plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", linewidth = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black")),
          lolli_plot(events),
          bubble_plot,
          ncol = 2, nrow = 2, align = "hv",
          labels = c("a", "b", "c", "d") # Add labels to the plots
))
