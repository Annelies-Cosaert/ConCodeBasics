# MODULE 2: SPECIFIC COLLECTION CONSERVATION CALCULATIONS 
# =======================================

# OVERVIEW:
# This module builds on Module 1's outputs to perform statistical analysis
# and generate conservation-related calculations.

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# 1. For custom climate class: define variables

# IF NEED TO CREATE LIST TO DOCUMENT METHOD 
# **** CREATE LIST ... TO DOCUMENT METHOD ****
# 1. List of Belgian extreme weather phenomena and variables
# 2. List of ASHRAE guideline specifics
# 3. List of cool, cold and frozen guidelines specifics
# 4. List of other guideline specifics

# PROCESSING STEPS:
# 1. Initial Setup and Verification
#    - Load required packages (dplyr, openxlsx, crayon, lubridate, zoo)
#    - Verify Module 1 outputs exist
#    - Set up color schemes for visualizations
#    - Initialize statistical functions

# 2. Period Analysis
#    - Identify complete calendar years
#    - Calculate rolling year periods (364-day intervals)
#    - Validate data completeness for each period
#    - Generate period summaries for each location

# 3. Statistical Calculations
#    - Process yearly statistics:
#      * Temperature and humidity averages
#      * Seasonal breakdowns
#      * Min/max values with outlier removal
#      * Standard deviations
#    - Calculate data quality metrics:
#      * Missing data percentages
#      * Outlier frequencies
#      * Data completeness scores

# 4. Table Generation
#    - Create comprehensive statistics tables
#    - Format data with proper decimal places
#    - Add seasonal classifications
#    - Generate summary statistics:
#      * Annual averages
#      * Seasonal patterns
#      * Data quality indicators

# 5. File Outputs (saved to Numeric_Results directory):
#    - [filename]_Loc[x]_[name]_Averages.csv: Statistical analysis results
#    - Contains:
#      * Date ranges
#      * Temperature statistics
#      * Humidity statistics
#      * Seasonal breakdowns
#      * Quality metrics

# 6. Quality Control and Reporting
#    - Validate statistical calculations
#    - Check for data completeness
#    - Generate quality control reports
#    - Produce data visualization outputs
#    - Create summary of findings

# VARIABLES AND METRICS:
# Statistical Metrics:
# - avg_[param]: Average value for parameter
# - med_[param]: Median value for parameter
# - sd_[param]: Standard deviation
# - max_[param]: Maximum value (outliers removed)
# - min_[param]: Minimum value (outliers removed)

# Seasonal Classifications:
# - winter_[param]: Winter season metrics
# - spring_[param]: Spring season metrics
# - summer_[param]: Summer season metrics
# - autumn_[param]: Autumn season metrics

# Quality Metrics:
# - na_pct: Percentage of missing values
# - outliers_pct: Percentage of outliers
# - completeness: Data completeness score

# Functions return NA for insufficient data (< 95% completeness)
# Outliers determined using 1.5 IQR method

# STEP 1: FILE VERIFICATION AND INITIAL SETUP
# ------------------------------------
# Load required packages
required_packages <- c("dplyr", "openxlsx", "crayon", "lubridate", "zoo", "purrr")
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# Load directory registry from MOD0
load_registry <- function(file_path) {
  data_directory <- dirname(normalizePath(file_path))
  prefix <- tools::file_path_sans_ext(basename(file_path))
  registry_file <- file.path(data_directory, "99_Registry", 
                             paste0(prefix, "_directory_registry.rds"))
  
  if (!file.exists(registry_file)) {
    stop("Directory registry not found. Please run MOD0 first.")
  }
  
  return(readRDS(registry_file))
}

# Load the registry
dir_registry <- load_registry(file_path)

# Check Module 1 outputs
.required_objects <- c("BUILDING_h", "WEATHER", "results", paste0("Loc", 1:5))

.check_module1 <- function() {
  missing_objects <- .required_objects[!sapply(.required_objects, exists)]
  if(length(missing_objects) > 0) {
    warning("\n!!! RUN MODULE 1 BEFORE USING THIS MODULE !!!\nMissing outputs: ", 
            paste(missing_objects, collapse=", "), 
            "\n", call.=FALSE)
    return(FALSE)
  }
  return(TRUE)
}

# Run initial check
if(!.check_module1()) {
  cat("\nPlease run Module 1 (Data Preparation and Processing) first.\n")
  stop("Required objects from Module 1 not found")
}

# STEP 2: HELPER FUNCTIONS
# ------------------------------------
# Update the remove_outliers function
remove_outliers <- function(x, factor = 1.5) {
  # Return NA if input is empty or all NA
  if (length(x[!is.na(x)]) == 0) return(NA)
  
  qnt <- quantile(x, probs=c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(qnt)
  lower <- qnt[1] - factor * iqr
  upper <- qnt[2] + factor * iqr
  x[x < lower | x > upper] <- NA
  
  # If all values were outliers, return NA instead of empty vector
  if (all(is.na(x))) return(NA)
  
  return(x)
}

# Add safe min/max functions
safe_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA)
  min(x, na.rm = na.rm)
}

safe_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = na.rm)
}

safe_stats <- function(x, func) {
  if (length(x[!is.na(x)]) == 0) return(NA)
  return(func(x, na.rm = TRUE))
}

# STEP 3: YEAR ANALYSIS FUNCTIONS
# ------------------------------------
find_calendar_years <- function(data) {
  if (!inherits(data$DateTime, "POSIXct")) {
    tryCatch({
      data$DateTime <- as.POSIXct(data$DateTime)
    }, error = function(e) {
      stop("Could not convert DateTime to POSIXct format")
    })
  }
  
  years <- as.numeric(format(data$DateTime, "%Y"))
  unique_years <- unique(years)
  
  calendar_years <- lapply(unique_years, function(y) {
    year_data <- data[years == y, ]
    start_date <- as.POSIXct(paste0(y, "-01-01 00:00:00"))
    end_date <- as.POSIXct(paste0(y, "-12-31 23:59:59"))
    expected_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
    actual_days <- n_distinct(as.Date(year_data$DateTime))
    
    list(
      year = y,
      complete = actual_days >= expected_days * 0.99,
      days = actual_days,
      expected_days = expected_days
    )
  })
  
  return(calendar_years)
}

find_rolling_years <- function(data) {
  start_date <- min(data$DateTime, na.rm = TRUE)
  end_date <- max(data$DateTime, na.rm = TRUE)
  total_days <- as.numeric(difftime(end_date, start_date, units="days"))
  
  rolling_years <- list()
  if (total_days >= 364) {
    n_rolling_years <- floor(total_days / 364)
    for (i in 1:n_rolling_years) {
      roll_start <- start_date + days((i-1) * 364)
      roll_end <- roll_start + days(364)
      if (roll_end <= end_date) {
        year_data <- data[data$DateTime >= roll_start & data$DateTime <= roll_end, ]
        rolling_years[[i]] <- list(
          year = i,
          start = roll_start,
          end = roll_end,
          days = n_distinct(as.Date(year_data$DateTime))
        )
      }
    }
  }
  return(rolling_years)
}

validate_period_data <- function(data) {
  validation <- list(
    is_valid = TRUE,
    messages = character()
  )
  
  if (!"DateTime" %in% names(data)) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Missing DateTime column")
  }
  
  if (!"Season" %in% names(data)) {
    validation$is_valid <- FALSE
    validation$messages <- c(validation$messages, "Missing Season column")
  }
  
  return(validation)
}

analyze_time_periods <- function(data, location_name) {
  validation <- validate_period_data(data)
  if (!validation$is_valid) {
    warning(paste("Validation failed for", location_name, ":", 
                  paste(validation$messages, collapse = "; ")))
    return(NULL)
  }
  
  if (!inherits(data$DateTime, "POSIXct")) {
    tryCatch({
      data$DateTime <- as.POSIXct(data$DateTime)
    }, error = function(e) {
      warning(paste("Failed to convert DateTime for", location_name))
      return(NULL)
    })
  }
  
  calendar_years <- find_calendar_years(data)
  rolling_years <- find_rolling_years(data)
  
  period_summary <- list(
    location = location_name,
    total_period = list(
      start = min(data$DateTime),
      end = max(data$DateTime),
      total_days = as.numeric(difftime(max(data$DateTime), 
                                       min(data$DateTime), 
                                       units = "days")) + 1
    ),
    calendar_years = calendar_years,
    rolling_years = rolling_years
  )
  
  class(period_summary) <- "period_analysis"
  return(period_summary)
}

visualize_year_splits <- function(data, location_name) {
  data <- data[!is.na(data$DateTime), ]
  start_date <- min(data$DateTime, na.rm = TRUE)
  end_date <- max(data$DateTime, na.rm = TRUE)
  
  cat("\nYear Split Analysis for", location_name)
  cat("\n=======================================")
  cat("\nPeriod:", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"))
  cat("\n\nDetailed Timeline:")
  cat("\n-----------------\n")
  
  years <- sort(unique(year(data$DateTime)))
  years <- years[!is.na(years)]
  n_years <- length(years)
  
  for (i in seq_along(years)) {
    y <- years[i]
    year_data <- data[year(data$DateTime) == y, ]
    year_start <- min(year_data$DateTime, na.rm = TRUE)
    year_end <- max(year_data$DateTime, na.rm = TRUE)
    days_in_year <- n_distinct(as.Date(year_data$DateTime))
    
    is_complete <- days_in_year >= 364
    
    cat(sprintf("\n%d: ", y))
    cat(format(year_start, "%b %d"), "→", format(year_end, "%b %d"))
    cat(" (", days_in_year, " days) ", sep="")
    
    if (is_complete) {
      cat("[COMPLETE]")
    } else {
      if (i == 1) cat("[PARTIAL START]")
      else if (i == n_years) cat("[PARTIAL END]")
      else cat("[INCOMPLETE]")
    }
    
    coverage <- round(days_in_year / 365 * 100, 1)
    cat(sprintf(" - %.1f%% coverage", coverage))
  }
  
  total_days <- as.numeric(difftime(end_date, start_date, units="days"))
  
  if (!is.na(total_days) && total_days >= 364) {
    n_rolling_years <- floor(total_days / 364)
    cat("\n\nPossible Rolling Years:")
    cat("\n---------------------")
    for (i in 1:n_rolling_years) {
      roll_start <- start_date + days((i-1) * 364)
      roll_end <- roll_start + days(364)
      if (roll_end <= end_date) {
        cat(sprintf("\nRY%d: %s → %s [364 days]", 
                    i,
                    format(roll_start, "%Y-%m-%d"),
                    format(roll_end, "%Y-%m-%d")))
      }
    }
  }
  
  complete_years <- sum(sapply(years, function(y) {
    year_data <- data[year(data$DateTime) == y, ]
    n_distinct(as.Date(year_data$DateTime)) >= 364
  }))
  
  partial_years <- n_years - complete_years
  
  cat("\n\nSummary:")
  cat("\n--------")
  cat("\nTotal measurement period:", round(total_days), "days")
  cat("\nComplete calendar years:", complete_years)
  cat("\nPartial/incomplete years:", partial_years)
  if (!is.na(total_days) && total_days >= 364) {
    cat("\nPossible rolling year periods:", floor(total_days / 364))
  }
  cat("\n\n")
}

visualize_all_locations <- function() {
  loc_objects <- ls(pattern = "^Loc\\d+$", envir = .GlobalEnv)
  for (loc_name in loc_objects) {
    data <- get(loc_name, envir = .GlobalEnv)
    tryCatch({
      visualize_year_splits(data, loc_name)
    }, error = function(e) {
      cat("\nError processing", loc_name, ":", e$message, "\n")
    })
  }
}

# STEP 4: STATISTICAL CALCULATIONS
# ------------------------------------
calculate_period_stats <- function(data, period_start, period_end, location_number) {
  period_data <- data[data$DateTime >= period_start & data$DateTime <= period_end, ]
  
  if (nrow(period_data) == 0) return(NULL)
  
  temp_col <- paste0("Ti", location_number)
  rh_col <- paste0("RHi", location_number)
  
  stats <- list()
  
  temp_clean <- remove_outliers(period_data[[temp_col]])
  stats$yearly <- list(
    temperature = list(
      mean = safe_stats(period_data[[temp_col]], mean),
      median = safe_stats(period_data[[temp_col]], median),
      max = safe_stats(temp_clean, max),
      min = safe_stats(temp_clean, min),
      sd = safe_stats(period_data[[temp_col]], sd)
    ),
    humidity = list(
      mean = safe_stats(period_data[[rh_col]], mean),
      median = safe_stats(period_data[[rh_col]], median),
      max = safe_stats(remove_outliers(period_data[[rh_col]]), max),
      min = safe_stats(remove_outliers(period_data[[rh_col]]), min),
      sd = safe_stats(period_data[[rh_col]], sd)
    )
  )
  
  stats$seasonal <- list()
  for (season in unique(period_data$Season)) {
    season_data <- period_data[period_data$Season == season, ]
    if (nrow(season_data) > 0) {
      temp_clean_season <- remove_outliers(season_data[[temp_col]])
      stats$seasonal[[season]] <- list(
        temperature = list(
          mean = safe_stats(season_data[[temp_col]], mean),
          median = safe_stats(season_data[[temp_col]], median),
          max = safe_stats(temp_clean_season, max),
          min = safe_stats(temp_clean_season, min),
          sd = safe_stats(season_data[[temp_col]], sd)
        ),
        humidity = list(
          mean = safe_stats(season_data[[rh_col]], mean),
          median = safe_stats(season_data[[rh_col]], median),
          max = safe_stats(remove_outliers(season_data[[rh_col]]), max),
          min = safe_stats(remove_outliers(season_data[[rh_col]]), min),
          sd = safe_stats(season_data[[rh_col]], sd)
        ),
        n_days = n_distinct(as.Date(season_data$DateTime))
      )
    }
  }
  
  stats$metadata <- list(
    period_start = period_start,
    period_end = period_end,
    total_days = n_distinct(as.Date(period_data$DateTime)),
    location = location_number
  )
  
  return(stats)
}

# STEP 5: TABLE CREATION AND EXPORT
# ------------------------------------
get_seasonal_indices <- function() {
  list(
    "Winter" = list(
      temp_avg = 4,    # Index for Winter_T average
      rh_avg = 9,      # Index for Winter_RH average
      temp_min = 14,   # Index for Winter_T minimum
      temp_max = 19    # Index for Winter_T maximum
    ),
    "Spring" = list(
      temp_avg = 5,    # Index for Spring_T average
      rh_avg = 10,     # Index for Spring_RH average
      temp_min = 15,   # Index for Spring_T minimum
      temp_max = 20    # Index for Spring_T maximum
    ),
    "Summer" = list(
      temp_avg = 6,    # Index for Summer_T average
      rh_avg = 11,     # Index for Summer_RH average
      temp_min = 16,   # Index for Summer_T minimum
      temp_max = 21    # Index for Summer_T maximum
    ),
    "Autumn" = list(
      temp_avg = 7,    # Index for Autumn_T average
      rh_avg = 12,     # Index for Autumn_RH average
      temp_min = 17,   # Index for Autumn_T minimum
      temp_max = 22    # Index for Autumn_T maximum
    )
  )
}

# Update create_annual_stats_table function to include RH min/max
create_annual_stats_table <- function(data, location_number) {
  years <- sort(unique(year(data$DateTime)))
  
  metrics <- c(
    "Date from", "Date to",
    "Average_T", "- Winter_T", "- Spring_T", "- Summer_T", "- Autumn_T",
    "Average_RH", "- Winter_RH", "- Spring_RH", "- Summer_RH", "- Autumn_RH",
    "Min_T", "- Winter_T", "- Spring_T", "- Summer_T", "- Autumn_T",
    "Max_T", "- Winter_T", "- Spring_T", "- Summer_T", "- Autumn_T",
    "Min_RH", "- Winter_RH", "- Spring_RH", "- Summer_RH", "- Autumn_RH",  # Added RH min
    "Max_RH", "- Winter_RH", "- Spring_RH", "- Summer_RH", "- Autumn_RH",  # Added RH max
    "Sd per year", "NA's (%) per year", "Outliers (%) per year"
  )
  
  results <- data.frame(
    Metric = metrics,
    matrix(NA, nrow = length(metrics), 
           ncol = length(years), 
           dimnames = list(NULL, paste0("Year_", years)))
  )
  
  # Update season indices to include RH min/max
  season_indices <- list(
    "Winter" = list(
      temp_avg = 4,    # Index for Winter_T average
      rh_avg = 9,      # Index for Winter_RH average
      temp_min = 14,   # Index for Winter_T minimum
      temp_max = 19,   # Index for Winter_T maximum
      rh_min = 24,     # Index for Winter_RH minimum
      rh_max = 29      # Index for Winter_RH maximum
    ),
    "Spring" = list(
      temp_avg = 5,    # Index for Spring_T average
      rh_avg = 10,     # Index for Spring_RH average
      temp_min = 15,   # Index for Spring_T minimum
      temp_max = 20,   # Index for Spring_T maximum
      rh_min = 25,     # Index for Spring_RH minimum
      rh_max = 30      # Index for Spring_RH maximum
    ),
    "Summer" = list(
      temp_avg = 6,    # Index for Summer_T average
      rh_avg = 11,     # Index for Summer_RH average
      temp_min = 16,   # Index for Summer_T minimum
      temp_max = 21,   # Index for Summer_T maximum
      rh_min = 26,     # Index for Summer_RH minimum
      rh_max = 31      # Index for Summer_RH maximum
    ),
    "Autumn" = list(
      temp_avg = 7,    # Index for Autumn_T average
      rh_avg = 12,     # Index for Autumn_RH average
      temp_min = 17,   # Index for Autumn_T minimum
      temp_max = 22,   # Index for Autumn_T maximum
      rh_min = 27,     # Index for Autumn_RH minimum
      rh_max = 32      # Index for Autumn_RH maximum
    )
  )
  
  for (i in seq_along(years)) {
    y <- years[i]
    col <- paste0("Year_", y)
    year_data <- data[year(data$DateTime) == y, ]
    
    # Fill dates
    results[1, col] <- format(min(year_data$DateTime, na.rm = TRUE), "%Y-%m-%d")
    results[2, col] <- format(max(year_data$DateTime, na.rm = TRUE), "%Y-%m-%d")
    
    temp_col <- paste0("Ti", location_number)
    rh_col <- paste0("RHi", location_number)
    
    # Function to safely format numeric values
    safe_format <- function(x, format = "%.2f") {
      if (is.na(x) || is.nan(x)) return("NA")
      sprintf(format, x)
    }
    
    # Calculate yearly averages
    results[3, col] <- safe_format(mean(year_data[[temp_col]], na.rm = TRUE))
    results[8, col] <- safe_format(mean(year_data[[rh_col]], na.rm = TRUE))
    
    # Calculate yearly min/max (excluding outliers)
    temp_clean_year <- remove_outliers(year_data[[temp_col]])
    rh_clean_year <- remove_outliers(year_data[[rh_col]])
    
    results[13, col] <- safe_format(safe_min(temp_clean_year))  # Min T
    results[18, col] <- safe_format(safe_max(temp_clean_year))  # Max T
    results[23, col] <- safe_format(safe_min(rh_clean_year))    # Min RH
    results[28, col] <- safe_format(safe_max(rh_clean_year))    # Max RH
    
    # Calculate seasonal statistics
    for (season in c("Winter", "Spring", "Summer", "Autumn")) {
      season_data <- year_data[year_data$Season == season, ]
      if (nrow(season_data) > 0) {
        idx <- season_indices[[season]]
        
        # Temperature statistics
        temp_clean <- remove_outliers(season_data[[temp_col]])
        results[idx$temp_avg, col] <- safe_format(mean(season_data[[temp_col]], na.rm = TRUE))
        results[idx$temp_min, col] <- safe_format(safe_min(temp_clean))
        results[idx$temp_max, col] <- safe_format(safe_max(temp_clean))
        
        # RH statistics
        rh_clean <- remove_outliers(season_data[[rh_col]])
        results[idx$rh_avg, col] <- safe_format(mean(season_data[[rh_col]], na.rm = TRUE))
        results[idx$rh_min, col] <- safe_format(safe_min(rh_clean))
        results[idx$rh_max, col] <- safe_format(safe_max(rh_clean))
      }
    }
    
    # Calculate additional statistics
    results[33, col] <- safe_format(sd(year_data[[temp_col]], na.rm = TRUE))  # Updated index for SD
    
    # Calculate NA percentage
    total_values <- nrow(year_data) * 2  # Both temp and RH
    na_count <- sum(is.na(year_data[[temp_col]]) | is.na(year_data[[rh_col]]))
    results[34, col] <- sprintf("%.2f%%", (na_count/total_values) * 100)  # Updated index for NA%
    
    # Calculate outlier percentage
    outliers_temp <- sum(is.na(remove_outliers(year_data[[temp_col]]))) - sum(is.na(year_data[[temp_col]]))
    outliers_rh <- sum(is.na(remove_outliers(year_data[[rh_col]]))) - sum(is.na(year_data[[rh_col]]))
    results[35, col] <- sprintf("%.2f%%", ((outliers_temp + outliers_rh)/total_values) * 100)  # Updated index for outliers
  }
  
  return(results)
}

format_annual_stats <- function(df, location_name) {
  wb <- createWorkbook()
  sheet <- addWorksheet(wb, "Statistics")
  
  writeData(wb, sheet, 
            paste("Location", location_name, "- Averages, Min and Max, seasonal and per Year"),
            startRow = 1, startCol = 1)
  
  writeData(wb, sheet, df, startRow = 3)
  
  return(list(df = df, wb = wb))
}

save_fluctuations <- function(data, location_number, prefix, registry) {
  # Calculate fluctuations
  results <- calculate_fluctuation_tables(data, location_number)
  
  # Format tables
  daily_table <- results$daily %>%
    mutate(across(starts_with(c("T_", "RH_")), ~round(., 2))) %>%
    arrange(Date)
  
  weekly_table <- results$weekly %>%
    mutate(across(starts_with(c("T_", "RH_")), ~round(., 2))) %>%
    arrange(Start_Date)
  
  # Save to R environment
  env_name_daily <- sprintf("Loc%d_24hFluct", location_number)
  env_name_weekly <- sprintf("Loc%d_7dFluct", location_number)
  assign(env_name_daily, daily_table, envir = .GlobalEnv)
  assign(env_name_weekly, weekly_table, envir = .GlobalEnv)
  
  # Save to CSV files
  file_name_daily <- sprintf("%s_Loc%d_24hFluct.csv", prefix, location_number)
  file_name_weekly <- sprintf("%s_Loc%d_7dFluct.csv", prefix, location_number)
  
  write.csv(daily_table, 
            file.path(registry$paths$fluctuations, file_name_daily), 
            row.names = FALSE)
  write.csv(weekly_table, 
            file.path(registry$paths$fluctuations, file_name_weekly), 
            row.names = FALSE)
  
  return(list(
    file_names = c(file_name_daily, file_name_weekly),
    tables = list(daily = daily_table, weekly = weekly_table)
  ))
}

save_annual_stats <- function(formatted_data, location_number, prefix, registry) {
  # Get location name from Module 1's location_names
  loc_name <- location_names$Loc_name[location_names$Loc_number == location_number]
  
  # Save CSV with location name included in 4-1_Averages directory
  csv_file <- file.path(registry$paths$num_results, "4-1_Averages",
                        sprintf("%s_Loc%d_%s_Averages.csv", 
                                prefix, location_number, loc_name))
  
  # Create directory if it doesn't exist
  dir.create(dirname(csv_file), recursive = TRUE, showWarnings = FALSE)
  
  # Save to CSV
  write.csv(formatted_data$df, csv_file, row.names = FALSE)
  
  # Save to R environment with new naming convention
  env_name <- sprintf("Loc%d_Avg", location_number)
  assign(env_name, formatted_data$df, envir = .GlobalEnv)
  
  # Print confirmation
  cat(sprintf("\nSaved to CSV: %s", basename(csv_file)))
  cat(sprintf("\nSaved to R environment as: %s", env_name))
  
  return(csv_file)
}

create_all_tables <- function() {
  if (!exists("file_path")) {
    stop("file_path not found. Run Module 1 first.")
  }
  
  if (!exists("location_names")) {
    stop("location_names not found. Run Module 1 first.")
  }
  
  if (!exists("dir_registry")) {
    stop("dir_registry not found. Load registry first.")
  }
  
  prefix <- tools::file_path_sans_ext(basename(file_path))
  
  loc_objects <- ls(pattern = "^Loc\\d+$", envir = .GlobalEnv)
  if (length(loc_objects) == 0) {
    stop("No location objects found. Run Module 1 first.")
  }
  
  saved_files <- character()
  
  for(loc_name in loc_objects) {
    loc_number <- as.numeric(gsub("Loc", "", loc_name))
    cat("\nProcessing Location", loc_number, "...\n")
    
    data <- get(loc_name, envir = .GlobalEnv)
    stats_df <- create_annual_stats_table(data, loc_number)
    formatted_stats <- format_annual_stats(stats_df, loc_number)
    saved_files[loc_name] <- save_annual_stats(formatted_stats, 
                                               loc_number, 
                                               prefix, 
                                               dir_registry)
  }
  
  # Print summary
  cat("\n=== FILES SAVED ===\n")
  cat("Directory:", file.path(dir_registry$paths$num_results, "4-1_Averages"), "\n")
  cat("\nStatistics files:\n")
  for(loc_name in names(saved_files)) {
    cat(sprintf("\nLocation %s: %s\n", 
                loc_name, 
                basename(saved_files[loc_name])))
  }
}

calculate_fluctuation_tables <- function(data, location_number) {
  # Extract temperature and RH columns
  temp_col <- paste0("Ti", location_number)
  rh_col <- paste0("RHi", location_number)
  
  # Safe min/max functions that handle all-NA groups
  safe_min <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) return(NA)
    min(x, na.rm = na.rm)
  }
  
  safe_max <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) return(NA)
    max(x, na.rm = na.rm)
  }
  
  safe_mean <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) return(NA)
    mean(x, na.rm = na.rm)
  }
  
  # Calculate daily fluctuations
  daily <- data %>%
    mutate(Date = as.Date(DateTime)) %>%
    filter(!is.na(Date)) %>%  # Remove rows with NA dates
    group_by(Date) %>%
    summarise(
      T_min = safe_min(!!sym(temp_col)),
      T_max = safe_max(!!sym(temp_col)),
      T_avg = safe_mean(!!sym(temp_col)),
      T_fluct = T_max - T_min,
      RH_min = safe_min(!!sym(rh_col)),
      RH_max = safe_max(!!sym(rh_col)),
      RH_avg = safe_mean(!!sym(rh_col)),
      RH_fluct = RH_max - RH_min,
      .groups = "drop"
    ) %>%
    filter(!is.na(T_min) & !is.na(RH_min))  # Remove rows where all values were NA
  
  # Calculate weekly fluctuations
  weekly <- data %>%
    mutate(Week_Start = floor_date(DateTime, unit = "week")) %>%
    filter(!is.na(Week_Start)) %>%  # Remove rows with NA dates
    group_by(Start_Date = Week_Start) %>%
    summarise(
      End_Date = max(DateTime),
      T_min = safe_min(!!sym(temp_col)),
      T_max = safe_max(!!sym(temp_col)),
      T_avg = safe_mean(!!sym(temp_col)),
      T_fluct = T_max - T_min,
      RH_min = safe_min(!!sym(rh_col)),
      RH_max = safe_max(!!sym(rh_col)),
      RH_avg = safe_mean(!!sym(rh_col)),
      RH_fluct = RH_max - RH_min,
      .groups = "drop"
    ) %>%
    filter(!is.na(T_min) & !is.na(RH_min))  # Remove rows where all values were NA
  
  return(list(daily = daily, weekly = weekly))
}

analyze_all_fluctuations <- function(prefix, registry) {
  # Get all location objects
  loc_objects <- ls(pattern = "^Loc\\d+$", envir = .GlobalEnv)
  
  # Create list to store all file names
  all_files <- character()
  
  # Process each location
  all_results <- list()
  for (loc_name in loc_objects) {
    loc_number <- as.numeric(gsub("Loc", "", loc_name))
    data <- get(loc_name, envir = .GlobalEnv)
    
    # Save fluctuations and get results
    results <- save_fluctuations(data, loc_number, prefix, registry)
    
    # Store results and file names
    all_results[[loc_name]] <- results$tables
    all_files <- c(all_files, results$file_names)
  }
  
  # Print summary
  cat("\n=== FLUCTUATION FILES SAVED ===")
  cat("\nDirectory:", registry$paths$fluctuations)
  cat("\n\nFluctuation files:\n")
  for (i in seq_along(all_files)) {
    cat(sprintf("%d. %s\n", i, all_files[i]))
  }
  
  return(invisible(all_results))
}

# Run the analysis
visualize_all_locations()  # Show year splits first
create_all_tables()        # Create and save summary tables
analyze_all_fluctuations(prefix, dir_registry)  # Add this line to run fluctuation analysis

# STEP 5: IDENTIFY EXTREME WEATHER FROM WEATHER FILE
# ------------------------------------
# **** CREATE LIST OF DEFINITION OF DIFFERENT EXTREME WEATHER PHENOMENA TO DOCUMENT METHOD ****
# Make sure all information except heathers and date-time are considered numeric (insert this in MOD1 before file safe)
# Extreme weather according to the meterological institute in Belgium
# Heatwave: reaching 25°C (daily max >= 25°C) for > 5 consecutive days AND at least 3 days >= 30°C (daily max >= 30°C)
# Coldwave: constantly under 0°C (daily max < 0°C) for > 5 consecutive days AND at least 3 days <= 10°C (daily min <= 10°C)
# Heavy rain: Between 31 and 50 l/m² in 1h OR between 41 and 60 l/m² in 6h OR between 51 and 100 l/m² in 24h
# Extreme rain: > 50 l/m² in 1h OR > 60 l/m² in 6h OR > 100 l/m² in 24h 

# BECAUSE COLD PERIODS DO NOT EXIST AND HEAVY AND EXTREME RAINFALL WERE DIFFICULT TO EXTRACT USING WORLDMET DATA,
# THE FOLLOWING THRESHOLDS WERE CHOSEN:
# Identify heatwave (from To column): reaching 25°C (daily max >= 25°C) for > 5 consecutive days AND at least 3 days >= 30°C (daily max >= 30°C)
# Identify cold period (from To column): Coldest period of every year based on a 5 day rolling average
# Identify wet periods (from precip_12h column): Any record over 10mm/12h. Many NA values, take into account that very heavy rainfall might not be recorded
# Identify dry periods (from precip column): Any record over 200h with no 0 rainfall measured
# Identify humid periods (from RHo column): any period where a 7day rolling average is above 85%

# Function to identify extreme weather periods (3 most extreme events per rolling year based on 3 day median)
analyze_weather_extremes <- function(weather_data, prefix, registry) {
  # Helper function for date formatting
  format_datetime <- function(x) {
    format(as.POSIXct(x), "%Y-%m-%d %H:%M:%S")
  }
  
  # 1. Find Dry Periods (>200h)
  find_dry_periods <- function(data) {
    # Find consecutive dry periods
    dry_runs <- rle(data$Precip == 0)
    run_lengths <- dry_runs$lengths[dry_runs$values]
    end_pos <- cumsum(dry_runs$lengths)[dry_runs$values]
    start_pos <- end_pos - run_lengths + 1
    
    # Create dataframe of long dry periods
    tibble(
      start_idx = start_pos,
      end_idx = end_pos,
      duration = run_lengths
    ) %>%
      filter(duration > 200) %>%  # Only periods >200 hours
      mutate(
        DateTime_from = format_datetime(data$DateTime[start_idx]),
        DateTime_to = format_datetime(data$DateTime[end_idx]),
        Avg_value = round(duration, 2),
        Unit = "hours without rain"
      ) %>%
      select(DateTime_from, DateTime_to, Avg_value, Unit)
  }
  
  # 2. Find Wet Periods (>10mm/12h)
  find_wet_periods <- function(data) {
    data %>%
      filter(Precip_12h > 10) %>%
      mutate(
        DateTime = format_datetime(DateTime),
        Value = round(Precip_12h, 2),
        Unit = "mm/12h"
      ) %>%
      select(DateTime, Value, Unit)
  }
  
  # 3. Find Heatwaves
  find_heatwaves <- function(data) {
    # Get daily max temperatures
    daily_max <- data %>%
      group_by(Date = as.Date(DateTime)) %>%
      summarise(
        max_temp = max(To, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Find potential heatwave starts (5+ days >=25°C)
    heatwaves <- map_df(1:(nrow(daily_max)-4), function(i) {
      period <- daily_max[i:(i+4), ]
      if (all(period$max_temp >= 25)) {
        # Check for 3 days >=30°C in or right after this period
        check_period <- daily_max[i:(min(i+6, nrow(daily_max))), ]
        if (sum(check_period$max_temp >= 30) >= 3) {
          # Get full period data
          period_data <- data %>%
            filter(
              DateTime >= as.POSIXct(paste(period$Date[1], "00:00:00")),
              DateTime <= as.POSIXct(paste(period$Date[5], "23:59:59"))
            )
          
          tibble(
            DateTime_from = format_datetime(min(period_data$DateTime)),
            DateTime_to = format_datetime(max(period_data$DateTime)),
            Max_value = round(max(period_data$To, na.rm = TRUE), 2),
            Min_value = round(min(period_data$To, na.rm = TRUE), 2),
            Avg_value = round(mean(period_data$To, na.rm = TRUE), 2),
            Unit = "°C"
          )
        }
      }
    }) %>%
      distinct()  # Remove any duplicates
  }
  
  # 4. Find Cold Periods
  find_cold_periods <- function(data) {
    data %>%
      # Calculate 5-day rolling average
      mutate(
        rolling_avg = rollapply(To, width = 120, # 5 days * 24 hours
                                mean, align = "center", fill = NA),
        Year = year(DateTime)
      ) %>%
      # Group by year and find coldest periods
      group_by(Year) %>%
      # Find potential cold periods
      mutate(
        event_group = cumsum(
          c(TRUE, diff(as.numeric(DateTime)) > 144*3600) # 6 days gap minimum
        )
      ) %>%
      group_by(Year, event_group) %>%
      slice_min(order_by = rolling_avg, n = 1) %>%
      ungroup() %>%
      group_by(Year) %>%
      slice_min(order_by = rolling_avg, n = 3) %>%
      ungroup() %>%
      # Get full period data
      pmap_dfr(function(...) {
        current <- tibble(...)
        period_data <- data %>%
          filter(
            DateTime >= current$DateTime - hours(60),
            DateTime <= current$DateTime + hours(60)
          )
        
        tibble(
          DateTime_from = format_datetime(min(period_data$DateTime)),
          DateTime_to = format_datetime(max(period_data$DateTime)),
          Max_value = round(max(period_data$To, na.rm = TRUE), 2),
          Min_value = round(min(period_data$To, na.rm = TRUE), 2),
          Avg_value = round(mean(period_data$To, na.rm = TRUE), 2),
          Unit = "°C"
        )
      })
  }
  
  # 5. Find Humid Periods
  find_humid_periods <- function(data) {
    data %>%
      mutate(
        # Calculate 7-day rolling average RH
        rolling_rh = rollapply(RHo, width = 168, # 7 days * 24 hours
                               mean, align = "center", fill = NA),
        Year = year(DateTime)
      ) %>%
      # Find periods where average RH > 85%
      filter(rolling_rh > 85) %>%
      group_by(Year) %>%
      # Group consecutive periods
      mutate(
        event_group = cumsum(
          c(TRUE, diff(as.numeric(DateTime)) > 192*3600) # 8 days gap minimum
        )
      ) %>%
      group_by(Year, event_group) %>%
      summarise(
        DateTime_from = format_datetime(min(DateTime) - hours(84)),
        DateTime_to = format_datetime(max(DateTime) + hours(84)),
        Avg_value = round(mean(rolling_rh, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(Unit = "%RH")
  }
  
  # Process all extremes
  dry_periods <- weather_data %>%
    filter(!is.na(Precip)) %>%
    find_dry_periods()
  
  wet_periods <- weather_data %>%
    filter(!is.na(Precip_12h)) %>%
    find_wet_periods()
  
  heatwaves <- find_heatwaves(weather_data)
  
  cold_periods <- find_cold_periods(weather_data)
  
  humid_periods <- find_humid_periods(weather_data)
  
  # Save results
  dir.create(file.path(registry$paths$num_results, "4-2_WeatherExtremes"), 
             recursive = TRUE, showWarnings = FALSE)
  
  save_results <- function(data, name) {
    write.csv(data,
              file.path(registry$paths$num_results, "4-2_WeatherExtremes", 
                        paste0(prefix, "_Weather_", name, ".csv")),
              row.names = FALSE)
    assign(paste0("Weather_", name), data, envir = .GlobalEnv)
  }
  
  save_results(dry_periods, "DryPeriods")
  save_results(wet_periods, "WetPeriods")
  save_results(heatwaves, "Heatwaves")
  save_results(cold_periods, "Cold")
  save_results(humid_periods, "HumidPeriods")
  
  return(list(
    dry_periods = dry_periods,
    wet_periods = wet_periods,
    heatwaves = heatwaves,
    cold_periods = cold_periods,
    humid_periods = humid_periods
  ))
}

extreme_weather <- analyze_weather_extremes(WEATHER, prefix, dir_registry)

# STEP 6: DEFINE ASHRAE GUIDELINES: CLIMATE CLASSES / COOL-COLD-FROZEN
# ------------------------------------
# **** CREATE LIST OF ASHRAE SPECIFIC GUIDELINES TO DOCUMENT METHOD ****

# "Temperature usually below 25°C": 
# (Specifically for ASHRAE Climate Class C) Is ignored. Rarely over 30 is taken as a threshold.

# "Short term fluctuation means": 
# (In the ASHRAE Climate Class table notes) Is ignored. The step by step change of climate monthly is evident. 
# Because it is not (always) possible to automise this process it should not be a condition. 
# If natural change is permitted, this process should be present relatively automatic.

# "Stable":
# (specifically from BIZOT) is ignored as variable.

# Annual / historic average:
# Chosen average is the annual average, with the possibility to sat it yourself. Excluding outliers. 
# Get from table 'Prefix_Locx_Name_Averages'(e.g. Letterhuis_Loc1_Beelden_Averages), from the Average_T, Average_RH (per year).  

# Seasonal fluctuations: 
# Difference between seasonal average (difference between winter-summer T and RH per year). 
# Get from table 'Prefix_Locx_Name_Averages'(e.g. Letterhuis_Loc1_Beelden_Averages), from the (average) Winter_T, Summer_T, Winter_RH and Summer_RH (per year).  

# 24h fluctuations (short term): 
# Fluctuations above and below the daily average. 
# However, slightly simplified as total T and RH fluctuations a day (so range, not specifically around the daily average). E.g. +-5 % becomes <10 %. 
# Get from 'Locx_24hFluct' (e.g. Letterhuis_Loc1_Beelden_24hFluct), from the T_Fluct and RH_Fluct columns.

# Long term outer limits: 
# Set per climate class. 
# In theory, short term 24h fluctuations might go on top of long term outer limits. E.g. for class A1, RH +-5% it and max. 65% it can go over 65% with an absolute max of 70% as long as it goes under 65% again within 24h. 
# However long term fluctuations are seen here and an absolute maximum and minimum with the possibility to visualize fluctuations at later stage.
# Get from the 'Locx' ('Letterhuis_Loc1_Beelden'), 'Tix' and 'RHix' (Ti1 and 'RHix) columns

# Define all climate class criteria
climate_classes <- list(
  AA = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(10, 25),
    rh_limits = c(35, 65),
    seasonal_temp = c(-5, 5),
    seasonal_rh = c(0, 0),
    fluct_temp = 4,
    fluct_rh = 10
  ),
  A1 = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(10, 25),
    rh_limits = c(35, 65),
    seasonal_temp = c(-10, 5),
    seasonal_rh = c(-10, 10),
    fluct_temp = 4,
    fluct_rh = 10
  ),
  A2 = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(10, 25),
    rh_limits = c(35, 65),
    seasonal_temp = c(-10, 5),
    seasonal_rh = c(0, 0),
    fluct_temp = 4,
    fluct_rh = 20
  ),
  B = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(-Inf, 30),
    rh_limits = c(30, 70),
    seasonal_temp = c(-20, 10),
    seasonal_rh = c(-10, 10),
    fluct_temp = 10,
    fluct_rh = 20
  ),
  C = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(-Inf, 40),
    rh_limits = c(25, 75),
    seasonal_temp = NULL,
    seasonal_rh = NULL,
    fluct_temp = NULL,
    fluct_rh = NULL,
    temp_under30 = list(
      max_temp = 30,                 # Tmax daily cannot exceed 30°C
      max_consecutive_days = 5       # for more than 5 consecutive days
    ),
    rh_below75 = list(
      max_rh_avg = 65,              # RH average should not exceed 65%
      max_consecutive_days = 5       # for more than 5 consecutive days
    )
  ),
  D = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(-Inf, Inf),
    rh_limits = c(-Inf, 75),
    seasonal_temp = NULL,
    seasonal_rh = NULL,
    fluct_temp = NULL,
    fluct_rh = NULL,
    rh_below75 = list(
      max_rh_avg = 65,              # RH average should not exceed 65%
      max_consecutive_days = 5       # for more than 5 consecutive days
    )
  ),
  BIZOT = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(16, 25),
    rh_limits = c(45, 55),
    seasonal_temp = NULL,
    seasonal_rh = NULL,
    fluct_temp = NULL,
    fluct_rh = 20
  ),
  # *** MODIfy VARIABLES IF NEED FOR CUSTOM ANALYSIS ***
  # CUSTOM = list(
  # annual_temp = NA,    
  # annual_rh = NA,
  # temp_limits = c(NA, NA),
  # rh_limits = c(NA, NA),
  # seasonal_temp = c(NA, NA),
  # seasonal_rh = c(NA, NA),
  # fluct_temp = NA,
  # fluct_rh = NA
  # )
)

# **** CREATE LIST OF COOL-COLD-FROZEN SPECIFIC GUIDELINES TO DOCUMENT METHOD ****
# Define special storage criteria
special_storage <- list(
  Cool = list(
    annual_temp = 12,    # Fixed average from ISO
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(8, 16),
    rh_limits = c(30, 50)
  ),
  Cold = list(
    annual_temp = 4,     # Fixed average from ISO
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(0, 8),
    rh_limits = c(30, 50)
  ),
  Frozen = list(
    annual_temp = NULL,  # If NULL, will use Average_T from Averages table (excluding outliers)
    annual_rh = NULL,    # If NULL, will use Average_RH from Averages table (excluding outliers)
    temp_limits = c(-20, 0),
    rh_limits = c(30, 50)
  )
)

# STEP 7: CALCULATE COMPLIANCE
# ------------------------------------
