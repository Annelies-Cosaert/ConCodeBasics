# MODULE 3: BASIC VISUAL REPRESENTATIONS: T, RH and OTHER DERIVATIVES
# MODULE 3-3: PSYCHROMETRIC CHARTS
# =======================================

# OVERVIEW:
# This module uses the data generated in MOD1 and MOD2 to create psychrometric charts
# The plots visualize temperature and humidity relationships without risk interpretations

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# 1. Temperature and humidity range if needed
# 2. Position of the RH% on graph if needed

# PROCESSING STEPS:
# 1. Initial Setup
#    - Load required packages
#    - Set up psychrometric calculation functions

# 2. Create individual location charts
#    - Basic psychrometric plot per location
#    - Optional: Add seasonal coloring
#    - Optional: Add data density contours

# 3. Create comparison charts
#    - Multiple locations on same chart
#    - Color coding by location/season

# File Outputs:
# - [filename]_Loc[x]_[name]_PsychroPlot.png: psychrometric chart per location
# - [filename]_All_PsychroPlot.png: comparison chart

# STEP 1: FILE VERIFICATION AND INITIAL SETUP
# ------------------------------------

# Load required packages
required_packages <- c("ggplot2", "psychrolib", "lubridate", "scales", "grid", "gridExtra", "stats", "MASS")
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Verify prerequisites
if (!exists("dir_registry")) stop("Directory registry not found. Please run Module 0 first.")

# Verify MOD2 functions are available
required_functions <- c("find_calendar_years", "find_rolling_years", 
                        "validate_period_data", "analyze_time_periods")
for(func in required_functions) {
  if(!exists(func)) {
    stop(paste("Required function", func, "not found. Please run Module 2 first."))
  }
}

# Initialize psychrolib
psychrolib::SetUnitSystem("SI")

# STEP 2: CREATE PSYCHROMETRIC CHART FOR ONE LOCATION 
# ------------------------------------

# Function to create psychrometric chart for a single location
create_psychro_plot <- function(loc_data, loc_number, loc_name, dir_registry, prefix) {
  # Get appropriate time period using MOD2 function
  period_result <- analyze_time_periods(loc_data, paste("Location", loc_number))
  if (is.null(period_result)) {
    warning(sprintf("No valid data for location %d - %s", loc_number, loc_name))
    return(NULL)
  }
  
  # Use the selected data
  plot_data <- as.data.frame(period_result$selected_data)
  
  # Extract temperature and RH data
  temp_data <- plot_data[[paste0("Ti", loc_number)]]
  rh_data <- plot_data[[paste0("RHi", loc_number)]]
  
  # Calculate humidity ratio
  w_data <- sapply(1:length(temp_data), function(i) {
    psychrolib::GetHumRatioFromRelHum(temp_data[i], rh_data[i]/100, 101325)
  })
  
  # Create base plot data
  plot_data <- data.frame(
    Temperature = temp_data,
    RH = rh_data,
    HumidityRatio = w_data * 1000, # Convert to g/kg
    Season = plot_data$Season
  )
  
  # Remove any NA values
  plot_data <- na.omit(plot_data)
  
  # Define fixed ranges for temperature and humidity ratio
  # *** MODIFY T AND RH RANGES (IF NEEDED) FOR NEW ANALYSIS ***
  temp_limits <- c(10, 35)  # Fixed temperature range from 10-35°C
  rh_limits <- c(10, 80)    # Fixed RH range from 10-80%
  
  # Calculate max humidity ratio at max temp and RH for y-axis limit
  max_humidity_ratio <- psychrolib::GetHumRatioFromRelHum(temp_limits[2], 
                                                          rh_limits[2]/100, 
                                                          101325) * 1000  # Convert to g/kg
  
  # Create temperature sequence for RH lines
  temp_range <- seq(temp_limits[1], temp_limits[2], length.out = 100)
  
  # Generate RH reference lines
  rh_lines <- data.frame()
  rh_seq <- seq(rh_limits[1], rh_limits[2], by = 10)
  for(rh in rh_seq) {
    w_line <- sapply(temp_range, function(t) {
      psychrolib::GetHumRatioFromRelHum(t, rh/100, 101325)
    }) * 1000  # Convert to g/kg
    rh_lines <- rbind(rh_lines, 
                      data.frame(Temperature = temp_range,
                                 HumidityRatio = w_line,
                                 RH = rh))
  }
  
  # Find y-position at T=30°C for each RH line
  rh_lines_with_labels <- rh_lines %>%
    group_by(RH) %>%
    summarise(
      # *** MODIFY POSITION OF THE REL.HUM. % ON GRAPH (IF NEEDED) FOR NEW ANALYSIS ***
      label_x = 30,  # Fixed x position at 30°C
      label_y = HumidityRatio[which.min(abs(Temperature - 30))]
    )
  
  # Calculate density estimates
  dens <- MASS::kde2d(plot_data$Temperature, plot_data$HumidityRatio, 
                      n = 50,
                      lims = c(temp_limits, 
                               c(0, max_humidity_ratio)))
  dens_df <- data.frame(
    Temperature = rep(dens$x, length(dens$y)),
    HumidityRatio = rep(dens$y, each = length(dens$x)),
    Density = as.vector(dens$z)
  )
  
  # Get period info for title using MOD2 helper function
  period_info <- format_period_info(period_result)
  
  # Create the plot
  p <- ggplot() +
    # First: Add RH reference lines (bottom layer)
    geom_line(data = rh_lines,
              aes(x = Temperature, y = HumidityRatio, 
                  group = RH, linetype = factor(RH)),
              color = "gray70", linewidth = 0.3) +
    # Second: Add RH labels
    geom_text(data = rh_lines_with_labels,
              aes(x = label_x, y = label_y, 
                  label = paste0(RH, "% RH.")),
              color = "gray50", size = 3, 
              hjust = -0.2) +
    # Third: Add data points
    geom_point(data = plot_data,
               aes(x = Temperature, y = HumidityRatio,
                   color = Season),
               alpha = 0.3, size = 0.5) +
    # Fourth: Add density contours on top
    geom_contour(data = dens_df,
                 aes(x = Temperature, y = HumidityRatio, z = Density),
                 color = "black", linewidth = 0.2) +
    # Set colors for seasons
    scale_color_manual(values = c(
      "Winter" = "#0000FF",       # Blue
      "Spring" = "#228B22",       # Green
      "Summer" = "#FFD700",       # Yellow
      "Autumn" = "#FF8C00"        # Orange
    )) +
    # Set line types for RH curves
    scale_linetype_manual(name = "Relative Humidity (%)",
                          values = rep("dotted", length(rh_seq))) +
    # Set fixed axis ranges
    scale_x_continuous(
      limits = temp_limits,
      breaks = seq(temp_limits[1], temp_limits[2], by = 5),
      expand = c(0, 0)  # Remove padding
    ) +
    scale_y_continuous(
      limits = c(0, max_humidity_ratio),
      breaks = seq(0, ceiling(max_humidity_ratio), by = 2),
      expand = c(0, 0)  # Remove padding
    ) +
    # Remove RH from legend
    guides(linetype = "none") +
    # Labels and titles including period info
    labs(title = paste(prefix, "- Location", loc_number, "-", loc_name,
                       "\nPsychrometric Chart\n", period_info),
         x = "Dry Bulb Temperature (°C)",
         y = "Humidity Ratio (g/kg)") +
    # Theme customization
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black", linewidth = 0.2),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.8, "lines")
    )
  
  # Save plot
  plot_dir <- file.path(dir_registry$paths$vis_results, "5-3_BasicPsychroChrt")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  plot_file <- file.path(plot_dir, 
                         sprintf("%s_Loc%d_%s_PsychroPlot.png", 
                                 prefix, loc_number, loc_name))
  
  ggsave(plot_file, p, width = 24, height = 16, units = "cm", dpi = 1200)
  
  return(p)
}

# Run the plotting function
create_all_psychro_plots()