# MODULE 3: BASIC VISUAL REPRESENTATIONS: T, RH and OTHER DERIVATIVES
# MODULE 3-2: BOX PLOTS
# =======================================

# OVERVIEW:
# This module uses the data generated in MOD1 and MOD2 in order to create box plots
# The plots created visualize data without additional interpretations. Aka, no risk related collections are performed.

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# None

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

# 4. Create plot for specific time range (optional)
#    TO-DO

# File Outputs:
# - [filename]_Loc[x]_[name]_BoxPlot.png: box plot per location (all data)

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

# create boxplot functions
create_climate_boxplots <- function(loc_data, loc_number, loc_name, dir_registry, prefix) {
  # Print initial diagnostics
  print("Total rows in original data:")
  print(nrow(loc_data))
  print("Season distribution in original data:")
  print(table(loc_data$Season, useNA = "ifany"))
  
  # Add year column and ensure proper date handling
  loc_data$Year <- year(loc_data$DateTime)
  
  # Find first and last dates with valid data
  valid_data <- loc_data[!is.na(loc_data[[paste0("Ti", loc_number)]]) & 
                           !is.na(loc_data[[paste0("RHi", loc_number)]]), ]
  date_range <- range(valid_data$DateTime)
  date_range_text <- format(date_range, "%Y-%m-%d")
  
  print("Rows after NA filtering:")
  print(nrow(valid_data))
  
  # Create plotting data frame - avoiding duplication
  years <- unique(valid_data$Year)
  seasons <- c("Full Year", "Winter", "Spring", "Summer", "Autumn")
  
  plot_data <- data.frame()
  
  # Create seasonal data first
  seasonal_data <- data.frame(
    Year = valid_data$Year,
    Season = valid_data$Season,
    Temperature = valid_data[[paste0("Ti", loc_number)]],
    RH = valid_data[[paste0("RHi", loc_number)]],
    PlotGroup = paste(valid_data$Year, valid_data$Season)
  )
  
  # Create full year data
  full_year_data <- data.frame()
  for(yr in years) {
    year_data <- valid_data[valid_data$Year == yr, ]
    full_year_data <- rbind(full_year_data, data.frame(
      Year = yr,
      Season = "Full Year",
      Temperature = year_data[[paste0("Ti", loc_number)]],
      RH = year_data[[paste0("RHi", loc_number)]],
      PlotGroup = paste(yr, "Full Year")
    ))
  }
  
  # Combine data
  plot_data <- rbind(full_year_data, seasonal_data)
  
  # Set factor levels for correct ordering
  plot_data$Season <- factor(plot_data$Season, levels = seasons)
  plot_data$PlotGroup <- factor(plot_data$PlotGroup, 
                                levels = unique(plot_data$PlotGroup[order(plot_data$Year, 
                                                                          match(plot_data$Season, 
                                                                                levels(plot_data$Season)))]))
  
  print("Final plot_data dimensions:")
  print(dim(plot_data))
  print("Season distribution in plot_data:")
  print(table(plot_data$Season))
  
  # Define color schemes
  colors <- c(
    "Full Year" = "#FF1493",    # Pink
    "Winter" = "#0000FF",       # Blue
    "Spring" = "#228B22",       # Green
    "Summer" = "#FFD700",       # Yellow
    "Autumn" = "#FF8C00"        # Orange
  )
  
  # Create plotting function with fixed vertical line calculation
  create_plot <- function(data, y_var, y_name, y_limits, y_breaks, title_suffix, alpha = 1) {
    # Extract year from PlotGroup properly
    year_changes <- which(diff(as.numeric(gsub("([0-9]{4}).*", "\\1", 
                                               levels(data$PlotGroup)))) != 0)
    
    # Find positions of Full Year entries
    full_year_pos <- which(grepl("Full Year", levels(data$PlotGroup)))
    
    ggplot(data, aes_string(x = "PlotGroup", y = y_var, fill = "Season")) +
      # Add year separators only where years actually change
      geom_vline(xintercept = year_changes + 0.5,
                 linetype = "solid", color = "gray50", linewidth = 0.2) +
      # Add Full Year separators
      geom_vline(xintercept = full_year_pos + 0.5,
                 linetype = "dotted", color = "gray50", linewidth = 0.2) +
      geom_boxplot(width = 0.7, alpha = alpha) +
      scale_fill_manual(name = "Season",
                        values = colors) +
      scale_y_continuous(name = y_name,
                         limits = y_limits,
                         breaks = y_breaks) +
      labs(title = paste("Location", loc_number, "-", loc_name, "-", 
                         paste(date_range_text, collapse = " to "),
                         title_suffix),
           x = "Year and Season") +
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
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.8, "lines")
      )
  }
  
  # Create both plots using the new function
  p_temp <- create_plot(plot_data, "Temperature", "Temperature (°C)", 
                        c(5, 40), seq(5, 40, 5), "\nTemperature Distribution")
  
  p_rh <- create_plot(plot_data, "RH", "Relative Humidity (%)", 
                      c(10, 90), seq(10, 90, 10), "\nRelative Humidity Distribution", 
                      alpha = 0.7)
  
  # Save plots
  plot_dir <- file.path(dir_registry$paths$vis_results, "5-2_BasicBoxPl")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # Save Temperature plot
  temp_file <- file.path(plot_dir, 
                         sprintf("%s_Loc%d_%s_BoxPlot_T.png", 
                                 prefix, loc_number, loc_name))
  ggsave(temp_file, p_temp, width = 24, height = 16, dpi = 1200)
  
  # Save RH plot
  rh_file <- file.path(plot_dir, 
                       sprintf("%s_Loc%d_%s_BoxPlot_RH.png", 
                               prefix, loc_number, loc_name))
  ggsave(rh_file, p_rh, width = 24, height = 16, dpi = 1200)
  
  return(list(temp = p_temp, rh = p_rh))
}

# Function to process all locations
create_all_boxplots <- function() {
  cat("Starting box plot creation\n")
  for(i in 1:nrow(location_names)) {
    loc_num <- location_names$Loc_number[i]
    loc_name <- location_names$Loc_name[i]
    
    cat("\nProcessing location:", loc_num, "-", loc_name, "\n")
    
    # Get location data
    loc_data <- get(paste0("Loc", loc_num))
    
    # Create and save plots
    plots <- create_climate_boxplots(
      loc_data = loc_data,
      loc_number = loc_num,
      loc_name = loc_name,
      dir_registry = dir_registry,
      prefix = prefix
    )
    print(plots$temp)
    print(plots$rh)
  }
  cat("\nBox plot creation complete\n")
}

# Run the plotting function
create_all_boxplots()