# MODULE 3: BASIC VISUAL REPRESENTATIONS: T, RH and OTHER DERIVATIVES
# MODULE 3-2: BOX PLOTS
# =======================================

# OVERVIEW:
# This module uses the data generated in MOD1 and MOD2 in order to create box plots
# The plots created visualize data without additional interpretations. Aka, no risk related collections are performed.

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# 1. Scale of the y-axis if needed (for T and RH)

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

# STEP 2: CREATE SEASONAL BOXPLOTS
# ------------------------------------

# create seasonal boxplots per year
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
  # Find first and last dates with valid data, explicitly removing NAs
  date_range <- range(valid_data$DateTime, na.rm = TRUE)
  date_range_text <- format(date_range, "%Y-%m-%d")
  
  # Add a debug print after date range calculation
  date_range <- range(valid_data$DateTime)
  print("Date range for this location:")
  print(date_range)
  
  print("Rows after NA filtering:")
  print(nrow(valid_data))

  # Create seasonal data (excluding NA dates)
  plot_data <- data.frame(
    Year = valid_data$Year,
    Season = valid_data$Season,
    Temperature = valid_data[[paste0("Ti", loc_number)]],
    RH = valid_data[[paste0("RHi", loc_number)]],
    PlotGroup = paste(valid_data$Year, valid_data$Season)
  )
  plot_data <- plot_data[!is.na(valid_data$DateTime), ]
  
  # Set up seasons order
  seasons <- c("Winter", "Spring", "Summer", "Autumn")
  
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
    "Winter" = "#0000FF",       # Blue
    "Spring" = "#228B22",       # Green
    "Summer" = "#FFD700",       # Yellow
    "Autumn" = "#FF8C00"        # Orange
  )
  
  # Create plotting function
  create_plot <- function(data, y_var, y_name, y_limits, y_breaks, title_suffix, alpha = 1) {
    # Extract year from PlotGroup more robustly
    plot_groups <- levels(data$PlotGroup)
    years <- suppressWarnings(as.numeric(sub("([0-9]{4}).*", "\\1", plot_groups)))
    year_changes <- which(diff(years) != 0)
    
    p <- ggplot(data, aes(x = PlotGroup, y = .data[[y_var]], fill = Season)) +
      # Add year separators only where years actually change
      {if(length(year_changes) > 0) 
        geom_vline(xintercept = year_changes + 0.5,
                   linetype = "solid", color = "gray50", linewidth = 0.2)} +
      geom_boxplot(width = 0.7, alpha = alpha, na.rm = TRUE) +
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
  
  # NA Summary
  na_summary <- data.frame(
    Temperature_NAs = sum(is.na(plot_data$Temperature)),
    RH_NAs = sum(is.na(plot_data$RH)),
    Total_Rows = nrow(plot_data)
  )
  
  print("NA Summary:")
  print(na_summary)
  
  # Remove rows with NAs to avoid warnings
  plot_data <- plot_data[!is.na(plot_data$Temperature) & !is.na(plot_data$RH), ]
  
  # Create both plots using the new function
  # *** MODIFY THE SCALE OF THE Y AXIS IF NOT WIDE ENOUGH FOR NEW ANALYSIS ***
  p_temp <- create_plot(plot_data, "Temperature", "Temperature (°C)", 
                        c(5, 40), seq(5, 40, 5), "\nTemperature Distribution")
  
  p_rh <- create_plot(plot_data, "RH", "Relative Humidity (%)", 
                      c(10, 90), seq(10, 90, 10), "\nRelative Humidity Distribution", 
                      alpha = 0.5)
  
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

# STEP 2: CREATE ANNUAL BOXPLOTS COMPARING LOCATIONS
# ------------------------------------

# Create comparison boxplots across locations
create_location_comparison_boxplots <- function(dir_registry, prefix) {
  # Initialize empty data frame for all locations
  all_data <- data.frame()
  
  # Get overall date range
  overall_start <- as.POSIXct("2100-01-01")  # Initialize with future date
  overall_end <- as.POSIXct("1900-01-01")    # Initialize with past date
  
  # Collect data from all locations
  for(i in 1:nrow(location_names)) {
    loc_num <- location_names$Loc_number[i]
    loc_name <- location_names$Loc_name[i]
    
    # Get location data
    loc_data <- get(paste0("Loc", loc_num))
    
    # Add year and calculate annual stats
    loc_data$Year <- year(loc_data$DateTime)
    
    # Update overall date range
    valid_dates <- loc_data$DateTime[!is.na(loc_data$DateTime)]
    overall_start <- min(overall_start, min(valid_dates, na.rm = TRUE))
    overall_end <- max(overall_end, max(valid_dates, na.rm = TRUE))
    
    # Create summary data
    yearly_data <- data.frame(
      Location = paste("Location", loc_num, "-", loc_name),
      LocationNum = loc_num,
      Year = loc_data$Year,
      Temperature = loc_data[[paste0("Ti", loc_num)]],
      RH = loc_data[[paste0("RHi", loc_num)]]
    )
    
    all_data <- rbind(all_data, yearly_data)
  }
  
  # Remove any NA values
  all_data <- all_data[!is.na(all_data$Year) & 
                         !is.na(all_data$Temperature) & 
                         !is.na(all_data$RH), ]
  
  # Create ordered factors for plotting
  all_data$Location <- factor(all_data$Location,
                              levels = unique(all_data$Location[order(all_data$LocationNum)]))
  
  # Create PlotGroup
  all_data$PlotGroup <- paste(all_data$Location, all_data$Year)
  all_data$PlotGroup <- factor(all_data$PlotGroup, 
                               levels = unique(all_data$PlotGroup[order(all_data$LocationNum, all_data$Year)]))
  
  # Get unique years for color gradient
  unique_years <- sort(unique(all_data$Year))
  num_years <- length(unique_years)
  
  # Create color gradient
  colors <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "pink"))(num_years)
  names(colors) <- unique_years
  
  # Create plotting function
  create_comparison_plot <- function(data, y_var, y_name, y_limits, y_breaks, title_suffix, alpha = 1) {
    # Find location changes 
    print("Unique locations:")
    print(unique(data$Location))
    
    # Get number of years per location to calculate line positions
    n_years <- length(unique(data$Year))
    loc_changes <- seq(n_years, n_years * (length(unique(data$Location)) - 1), by = n_years)
    print("Vertical lines at positions:")
    print(loc_changes)
    
    p <- ggplot(data, aes(x = PlotGroup, y = .data[[y_var]], fill = factor(Year))) +
      # Add horizontal grid lines
      geom_hline(yintercept = y_breaks, 
                 color = "gray90", 
                 linewidth = 0.2) +
      # Add vertical location separators
      geom_vline(xintercept = loc_changes + 0.5,
                 linetype = "dotted", 
                 color = "black",
                 linewidth = 0.4) +
      geom_boxplot(width = 0.7, alpha = alpha) +
      scale_fill_manual(name = "Year",
                        values = colors) +
      scale_y_continuous(name = y_name,
                         limits = y_limits,
                         breaks = y_breaks) +
      labs(title = paste("All Locations -",
                         format(overall_start, "%Y-%m-%d"),
                         "to",
                         format(overall_end, "%Y-%m-%d"),
                         title_suffix),
           x = "Location and Year") +
      theme(
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
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
    
    return(p)
  }
  
  # *** MODIFY THE SCALE OF THE Y AXIS IF NOT WIDE ENOUGH FOR NEW ANALYSIS ***
  # Create Temperature plot
  p_temp <- create_comparison_plot(all_data, "Temperature", "Temperature (°C)", 
                                   c(5, 40), seq(5, 40, 5), 
                                   "\nTemperature Distribution")
  # Create RH plot
  p_rh <- create_comparison_plot(all_data, "RH", "Relative Humidity (%)", 
                                 c(10, 90), seq(10, 90, 10), 
                                 "\nRelative Humidity Distribution",
                                 alpha = 0.5)
  
  # Save plots
  plot_dir <- file.path(dir_registry$paths$vis_results, "5-2_BasicBoxPl")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # Save Temperature plot
  temp_file <- file.path(plot_dir, sprintf("%s_All_BoxPlot_T.png", prefix))
  ggsave(temp_file, p_temp, width = 24, height = 16, dpi = 1200)
  
  # Save RH plot
  rh_file <- file.path(plot_dir, sprintf("%s_All_BoxPlot_RH.png", prefix))
  ggsave(rh_file, p_rh, width = 24, height = 16, dpi = 1200, bg = "white")
  
  return(list(temp = p_temp, rh = p_rh))
}

# Function to create all comparison plots
create_all_comparison_boxplots <- function() {
  cat("Starting comparison box plot creation\n")
  plots <- create_location_comparison_boxplots(dir_registry, prefix)
  print(plots$temp)
  print(plots$rh)
  cat("\nComparison box plot creation complete\n")
}

# Run the plotting function
create_all_comparison_boxplots()