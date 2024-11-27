# Function to create time series plot for climate data
create_climate_plot <- function(loc_data, loc_number, loc_name, 
                                weather_extremes, fluct_data, dir_registry, prefix) {
  library(ggplot2)
  library(lubridate)
  library(scales)
  library(grid)
  library(gridExtra)
}

  # Format dates
  date_range <- range(loc_data$DateTime, na.rm = TRUE)
  years_range <- paste(year(date_range[1]), year(date_range[2]), sep="-")
  
  # Create base plot - wrapping in suppressWarnings
  p <- suppressWarnings(
    ggplot() +
      # Indoor climate data
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("Ti", loc_number)), 
                                     color = "Ti"), size = 0.2) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("T24havg", loc_number)), 
                                     color = "T24havg"), size = 0.2, alpha = 0.5) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("T7davg", loc_number)), 
                                     color = "T7davg"), size = 0.2, alpha = 0.3) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RHi", loc_number)), 
                                     color = "RHi"), size = 0.2) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RH24havg", loc_number)), 
                                     color = "RH24havg"), size = 0.2, alpha = 0.5) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("RH7davg", loc_number)), 
                                     color = "RH7davg"), size = 0.2, alpha = 0.3) +
      geom_line(data = loc_data, aes(x = DateTime, y = get(paste0("Dwpti", loc_number)), 
                                     color = "Dwpti"), size = 0.2)
  )
  
  # Add extreme weather periods
  if (!is.null(weather_extremes$heatwaves) && nrow(weather_extremes$heatwaves) > 0) {
    p <- p + geom_rect(data = weather_extremes$heatwaves,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Heatwave"),
                       alpha = 0.1)
  }
  
  if (!is.null(weather_extremes$cold_periods) && nrow(weather_extremes$cold_periods) > 0) {
    p <- p + geom_rect(data = weather_extremes$cold_periods,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Cold period"),
                       alpha = 0.1)
  }
  
  if (!is.null(weather_extremes$dry_periods) && nrow(weather_extremes$dry_periods) > 0) {
    p <- p + geom_rect(data = weather_extremes$dry_periods,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Dry period"),
                       alpha = 0.1)
  }
  
  if (!is.null(weather_extremes$wet_periods) && nrow(weather_extremes$wet_periods) > 0) {
    p <- p + geom_vline(data = weather_extremes$wet_periods,
                        aes(xintercept = as.POSIXct(DateTime), 
                            color = "Heavy precipitation"),
                        linetype = "dotted", size = 0.5)
  }
  
  if (!is.null(weather_extremes$humid_periods) && nrow(weather_extremes$humid_periods) > 0) {
    p <- p + geom_rect(data = weather_extremes$humid_periods,
                       aes(xmin = as.POSIXct(DateTime_from), 
                           xmax = as.POSIXct(DateTime_to),
                           ymin = -Inf, ymax = Inf, fill = "Humid period"),
                       alpha = 0.1)
  }
  
  # Add fluctuations
  p <- p +
    geom_line(data = fluct_data, aes(x = as.POSIXct(Date), y = T_fluct, 
                                     color = "T_fluct"), size = 0.2) +
    geom_line(data = fluct_data, aes(x = as.POSIXct(Date), y = RH_fluct, 
                                     color = "RH_fluct"), size = 0.2)
  
  # Set colors and legend
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
  
  # Add custom legend guide to show correct line types
  guides(color = guide_legend(override.aes = list(
    linetype = c("solid", "solid", "solid",  # Ti, T24havg, T7davg
                 "solid", "solid", "solid",   # RHi, RH24havg, RH7davg
                 "solid",                     # Dwpti
                 "dotted", "dotted",          # T_fluct, RH_fluct
                 "solid"),                    # Heavy precipitation
    alpha = c(1, 0.5, 0.3,                  # Temperature lines
              1, 0.5, 0.3,                   # RH lines
              1,                             # Dwpt
              1, 1,                          # Fluctuations
              1)                             # Heavy precipitation
  )),
  fill = guide_legend(override.aes = list(alpha = 0.1)))  # Extreme weather alpha
  
  # Customize plot appearance
  p <- p +
    labs(title = paste("Location", loc_number, "-", loc_name, "-", years_range,
                       "\nIndoor - Outdoor"),
         x = "Date",
         y = "Temperature (°C) / Relative Humidity (%)") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_x_datetime(date_breaks = "1 month", 
                     labels = date_format("%b %Y")) +
    theme(
      # White background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Black frame and grid lines
      axis.line = element_line(color = "black", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.2),
      panel.grid.major.y = element_line(color = "gray90", size = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Text formatting
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      
      # Legend formatting
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
  
  suppressWarnings({
    ggsave(plot_file, p, width = 24, height = 16, dpi = 1200)
    return(p)
  })

# Function to process all locations - with warning suppression
create_all_plots <- function() {
  for(i in 1:nrow(location_names)) {
    loc_num <- location_names$Loc_number[i]
    loc_name <- location_names$Loc_name[i]
    
    # Get required data
    loc_data <- get(paste0("Loc", loc_num))
    fluct_data <- get(paste0("Loc", loc_num, "_24hFluct"))
    
    # Create and save plot with warning suppression
    suppressWarnings({
      p <- create_climate_plot(
        loc_data = loc_data,
        loc_number = loc_num,
        loc_name = loc_name,
        weather_extremes = list(
          heatwaves = Weather_Heatwaves,
          cold_periods = Weather_Cold,
          dry_periods = Weather_DryPeriods,
          wet_periods = Weather_WetPeriods,
          humid_periods = Weather_HumidPeriods
        ),
        fluct_data = fluct_data,
        dir_registry = dir_registry,
        prefix = prefix
      )
      
      # Display plot in R Studio
      print(p)
    })
  }
}

# Run the plotting function
create_all_plots()