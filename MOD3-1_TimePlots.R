# MODULE 3: BASIC VISUAL REPRESENTATIONS: T, RH and OTHER DERIVATIVES
# MODULE 3-1: TIME PLOTS
# =======================================

# OVERVIEW:
# This module uses the data generated in MOD1 and MOD2 in order to create time plots
# The plots created visualize data without additional interpretations. Aka, no risk related collections are performed.

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# 1. For custom time frame analysis: insert start date and end date of the analysis.

# IF NEED TO CREATE LIST TO DOCUMENT METHOD 
# **** CREATE LIST ... TO DOCUMENT METHOD ****
# None

# PROCESSING STEPS:
# 1. Initial Setup and File Verification
#    - Load required packages
#    - Verify Module 0 outputs exist

# 2. All data Analysis
#    - Add indoor climate data, weather extremes and fluctuations per lacation
#    - Format the table
#    - Save the plots

# 3. Annual analysis
#    TO-DO
#    - Work with full years only (get from MOD 2)
#    - Split in several years and save per year, per location

# 4. Create plot for specific time range (optional)
#    TO-DO
#    - Allow user to define a specific period for analysis.
#    - Work with hashtags to taggle on and off certain specific analysis: choose locations and data from a list.

# 5. Create an overlay of ASHRAE limits.
#    TO-DO
#    - Be able to choose your specific climate class
#    - Overlay on individual location charts
#    - Overlay on comparison charts
#    !!! Add the step by step monthly allowed changes (not yet part of MOD2)

# File Outputs:
# - [filename]_Loc[x]_[name]_TimePlot.png: Time plot per location (all data)

# STEP 1: FILE VERIFICATION AND INITIAL SETUP
# ------------------------------------

# Load required packages
required_packages <- c("ggplot2", "lubridate", "scales", "grid", "gridExtra")
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Verify prerequisites
if (!exists("dir_registry")) stop("Directory registry not found. Please run Module 0 first.")

# STEP 2: CREATE PLOTS WITH ALL DATA PER LOCATION
# ------------------------------------

# Function to create time series plot for climate data
create_climate_plot <- function(loc_data, loc_number, loc_name, dir_registry, prefix) {
  # Format dates
  date_range <- range(loc_data$DateTime, na.rm = TRUE)
  years_range <- paste(year(date_range[1]), year(date_range[2]), sep="-")
  
  # Create base plot
  p <- ggplot() +
    # Indoor climate data
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("Ti", loc_number)), 
                                   color = "Ti"), linewidth = 0.2, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("T24havg", loc_number)), 
                                   color = "T24havg"), linewidth = 0.2, alpha = 0.5, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("T7davg", loc_number)), 
                                   color = "T7davg"), linewidth = 0.2, alpha = 0.3, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RHi", loc_number)), 
                                   color = "RHi"), linewidth = 0.2, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RH24havg", loc_number)), 
                                   color = "RH24havg"), linewidth = 0.2, alpha = 0.5, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RH7davg", loc_number)), 
                                   color = "RH7davg"), linewidth = 0.2, alpha = 0.3, na.rm = TRUE) +
    geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("Dwpti", loc_number)), 
                                   color = "Dwpti"), linewidth = 0.2, na.rm = TRUE)
  
  # Add weather extremes
  if (exists("Weather_Heatwaves") && !is.null(Weather_Heatwaves) && nrow(Weather_Heatwaves) > 0) {
    p <- p + geom_rect(data = Weather_Heatwaves,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Heatwave"),
                       alpha = 0.1)
  }
  
  if (exists("Weather_Cold") && !is.null(Weather_Cold) && nrow(Weather_Cold) > 0) {
    p <- p + geom_rect(data = Weather_Cold,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Cold period"),
                       alpha = 0.1)
  }
  
  if (exists("Weather_DryPeriods") && !is.null(Weather_DryPeriods) && nrow(Weather_DryPeriods) > 0) {
    p <- p + geom_rect(data = Weather_DryPeriods,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Dry period"),
                       alpha = 0.1)
  }
  
  if (exists("Weather_WetPeriods") && !is.null(Weather_WetPeriods) && nrow(Weather_WetPeriods) > 0) {
    p <- p + geom_vline(data = Weather_WetPeriods,
                        aes(xintercept = as.POSIXct(DateTime), 
                            color = "Heavy precipitation"),
                        linetype = "dotted", linewidth = 0.5)
  }
  
  if (exists("Weather_HumidPeriods") && !is.null(Weather_HumidPeriods) && nrow(Weather_HumidPeriods) > 0) {
    p <- p + geom_rect(data = Weather_HumidPeriods,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Humid period"),
                       alpha = 0.1)
  }
  
  # Add fluctuations
  fluct_data <- get(paste0("Loc", loc_number, "_24hFluct"))
  p <- p +
    geom_line(data = fluct_data, aes(x = as.POSIXct(Date), y = T_fluct, 
                                     color = "T_fluct"), linewidth = 0.2, na.rm = TRUE) +
    geom_line(data = fluct_data, aes(x = as.POSIXct(Date), y = RH_fluct, 
                                     color = "RH_fluct"), linewidth = 0.2, na.rm = TRUE)
  
  # Set colors and legend
  p <- p +
    scale_color_manual(name = "Parameters",
                       values = c("Ti" = "darkred",
                                  "T24havg" = alpha("red", 0.5),
                                  "T7davg" = alpha("red", 0.3),
                                  "RHi" = "darkblue",
                                  "RH24havg" = alpha("blue", 0.5),
                                  "RH7davg" = alpha("blue", 0.3),
                                  "Dwpti" = "chartreuse3",
                                  "T_fluct" = "red",
                                  "RH_fluct" = "blue",
                                  "Heavy precipitation" = "darkblue"),
                       breaks = c("Ti", "T24havg", "T7davg",
                                  "RHi", "RH24havg", "RH7davg",
                                  "Dwpti", "T_fluct", "RH_fluct",
                                  "Heavy precipitation")) +
    scale_fill_manual(name = "Extreme Weather Phenomena",
                      values = c("Heatwave" = "magenta",
                                 "Cold period" = "cyan",
                                 "Dry period" = "yellow",
                                 "Humid period" = "darkblue"),
                      breaks = c("Heatwave",
                                 "Cold period", 
                                 "Dry period",
                                 "Humid period"))
  
  # Custom legend guide
  p <- p + guides(color = guide_legend(override.aes = list(
    linetype = c("solid", "solid", "solid",
                 "solid", "solid", "solid",
                 "solid",
                 "solid", "solid",
                 "dotted"),
    alpha = c(1, 0.5, 0.3,
              1, 0.5, 0.3,
              1,
              1, 1,
              1)
  )),
  fill = guide_legend(override.aes = list(alpha = 0.1)))
  
  # Plot appearance
  p <- p +
    labs(title = paste("Location", loc_number, "-", loc_name, "-", years_range,
                       "\nIndoor"),
         x = "Date",
         y = "Temperature (°C) / Relative Humidity (%)") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_x_datetime(date_breaks = "1 month", 
                     labels = date_format("%b %Y")) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black", linewidth = 0.2),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.8, "lines")
    )
  
  # Save plot
  plot_dir <- file.path(dir_registry$paths$vis_results, "5-1_BasicTimePl")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  plot_file <- file.path(plot_dir, 
                         sprintf("%s_Loc%d_%s_TimePlot.png", 
                                 prefix, loc_number, loc_name))
  
  ggsave(plot_file, p, width = 24, height = 16, dpi = 1200)
  
  # Return plot object for display in R Studio
  return(p)
}

# Function to process all locations
create_all_plots <- function() {
  cat("Starting plot creation\n")
  for(i in 1:nrow(location_names)) {
    loc_num <- location_names$Loc_number[i]
    loc_name <- location_names$Loc_name[i]
    
    cat("\nProcessing location:", loc_num, "-", loc_name, "\n")
    
    # Get location data
    loc_data <- get(paste0("Loc", loc_num))
    
    # Create and save plot
    p <- create_climate_plot(
      loc_data = loc_data,
      loc_number = loc_num,
      loc_name = loc_name,
      dir_registry = dir_registry,
      prefix = prefix
    )
    print(p)
  }
  cat("\nPlot creation complete\n")
}

# Run the plotting function
create_all_plots()

# STEP 3: ANNUAL DATA VISUALS
# ------------------------------------
#    TO-DO
#    - Work with full years only (get from MOD 2)
#    - Split in several years and save per year, per location

# STEP 4: COSTUM PLOT FOR SPECIFIC TIME RANGE ANALYSIS (OPTIONAL)
# ------------------------------------
#    TO-DO
#    - Allow user to define a specific period for analysis.
#    - Work with hashtags to taggle on and off certain specific analysis: choose locations and data from a list.

# STEP 5: OVERLAY ASHRAE ON TIME PLOTS (OPTIONAL)
# ------------------------------------
#    TO-DO
#    - Be able to choose your specific climate class
#    - Overlay on individual location charts
#    - Overlay on comparison charts
#    !!! Add the step by step monthly allowed changes (not yet part of MOD2)

