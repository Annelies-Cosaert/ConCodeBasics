# MODULE 1: DATA PREPARATION AND PROCESSING
# =======================================

# OVERVIEW:
# This script processes indoor and outdoor climate data, calculating various 
# environmental parameters and creating location-specific analyses.

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# 1. File path to input Excel file
# 2. Weather station ID
# 3. Climate region
# 4. Location/room names

# IF NEED TO CREATE LIST TO DOCUMENT METHOD 
# **** CREATE LIST ... TO DOCUMENT METHOD ****
# 1.List of calculation functions, variables and units (1/2)
# 2.List with data of chosen weather station
# 3.List of further calculation functions, variables and units (2/2)

# PROCESSING STEPS:
# 1. File Upload and Setup
#    - Load required packages
#    - Set file paths and create directories
#    - Read Excel input file

# 2. Data Preparation
#    - Rename columns to standard format (Ti1, RHi1, etc.)
#    - Convert timestamps to hourly intervals
#    - Add seasonal classifications

# 3. Weather Data
#    - Download data from specified weather station
#    - Process and align with building timeframe
#    - Calculate outdoor parameters

# 4. Location Processing
#    - Split data by location/room
#    - Calculate derived parameters:
#      * Dew point (Dwpt)
#      * Absolute Humidity (AH)
#      * 24-hour averages
#      * 7-day averages

# 5. File Outputs (saved to original directory):
#    - [filename]_BUILDING_h.csv: Processed building data
#    - [filename]_WEATHER.csv: Weather station data
#    - [filename]_Loc[x]_[name].csv: Individual location files

# 6. Quality Control
#    - Generate missing data reports
#    - Print file save confirmations
#    - Display location assignments

# VARIABLES NAMING CONVENTION:
# - Ti[x]: Indoor temperature at location x
# - RHi[x]: Indoor relative humidity at location x
# - To: Outdoor temperature
# - RHo: Outdoor relative humidity
# - Dwpti[x]: Indoor dew point at location x
# - AHi[x]: Indoor absolute humidity at location x

# STEP 1: INITIAL SETUP
# ------------------------------------
# Load required packages
required_packages <- c("readxl", "dplyr", "lubridate", "worldmet", "zoo")
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# STEP 2: FUNCTION DEFINITIONS
# ------------------------------------
# File path validation
validate_file_path <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  return(normalizePath(file_path))
}

# Registry loading
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

# Directory handling
ensure_directories <- function(registry) {
  for (path in unlist(registry$paths)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      cat("\nCreated directory:", path)
    }
  }
}

# DateTime checking
check_datetime_entries <- function(data) {
  problem_dates <- which(is.na(as.POSIXct(data$DateTime)))
  
  if(length(problem_dates) > 0) {
    cat("\nProblematic DateTime entries found at rows:", problem_dates)
    cat("\nValues:\n")
    print(data$DateTime[problem_dates])
  }
  
  return(problem_dates)
}

# DateTime cleaning
clean_datetime <- function(data) {
  original_datetime <- data$DateTime
  
  cleaned_datetime <- tryCatch({
    dmy_hms(original_datetime)
  }, error = function(e) {
    tryCatch({
      parse_date_time(original_datetime, 
                      orders = c("dmy HMS", "dmy HM",
                                 "ymd HMS", "ymd HM",
                                 "dmY HMS", "dmY HM"))
    }, error = function(e) {
      as.POSIXct(NA)
    })
  })
  
  still_invalid <- which(is.na(cleaned_datetime))
  fixed_entries <- which(is.na(as.POSIXct(original_datetime)) & !is.na(cleaned_datetime))
  
  if(length(fixed_entries) > 0) {
    cat("\nFixed DateTime entries:", length(fixed_entries))
  }
  
  if(length(still_invalid) > 0) {
    cat("\nEntries that could not be fixed:", length(still_invalid))
    cat("\nRows:", still_invalid)
    cat("\nOriginal values:\n")
    print(original_datetime[still_invalid])
  }
  
  return(cleaned_datetime)
}

# Data validation
validate_data <- function(data, file_path) {
  validation <- list(
    is_valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  if (!file.exists(file_path)) {
    validation$is_valid <- FALSE
    validation$errors <- c(validation$errors, 
                           sprintf("File not found: %s", file_path))
    return(validation)
  }
  
  required_cols <- c("DateTime")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    validation$is_valid <- FALSE
    validation$errors <- c(validation$errors,
                           sprintf("Missing required columns: %s", 
                                   paste(missing_cols, collapse = ", ")))
  }
  
  ti_cols <- grep("^Ti\\d+$", names(data), value = TRUE)
  rhi_cols <- grep("^RHi\\d+$", names(data), value = TRUE)
  
  if (length(ti_cols) == 0 || length(rhi_cols) == 0) {
    validation$is_valid <- FALSE
    validation$errors <- c(validation$errors,
                           "No temperature (Ti) or humidity (RHi) columns found")
  }
  
  if (length(ti_cols) > 0 && length(rhi_cols) > 0) {
    ti_numbers <- as.numeric(gsub("Ti", "", ti_cols))
    rhi_numbers <- as.numeric(gsub("RHi", "", rhi_cols))
    
    if (!all(ti_numbers == rhi_numbers)) {
      validation$warnings <- c(validation$warnings,
                               "Mismatch between temperature and humidity location numbers")
      cat("\nTemperature columns:", paste(ti_cols, collapse=", "))
      cat("\nHumidity columns:", paste(rhi_cols, collapse=", "))
    }
  }
  
  if ("DateTime" %in% names(data)) {
    datetime_check <- try(as.POSIXct(data$DateTime), silent = TRUE)
    if (inherits(datetime_check, "try-error")) {
      validation$warnings <- c(validation$warnings,
                               "DateTime column contains invalid entries")
      problem_dates <- check_datetime_entries(data)
      if (length(problem_dates) > 0) {
        validation$warnings <- c(validation$warnings,
                                 sprintf("Found %d invalid DateTime entries", 
                                         length(problem_dates)))
      }
    }
  }
  
  return(validation)
}

# CSV saving function
save_csv <- function(data, filename, registry) {
  if ("DateTime" %in% names(data)) {
    data$DateTime <- format(data$DateTime, "%Y-%m-%d %H:%M:%S")
  }
  
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- round(data[numeric_cols], 2)
  
  if (grepl("_BUILDING_h\\.csv$", filename)) {
    save_dir <- registry$paths$basic
  } else if (grepl("_WEATHER\\.csv$", filename)) {
    save_dir <- registry$paths$basic
  } else if (grepl("_Loc\\d+_.*\\.csv$", filename)) {
    save_dir <- registry$paths$locations
  } else {
    save_dir <- registry$paths$basic
  }
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  full_path <- file.path(save_dir, filename)
  tryCatch({
    write.csv(data, full_path, row.names = FALSE)
    cat("Successfully saved:", full_path, "\n")
  }, error = function(e) {
    stop("Error saving file: ", full_path, "\nError: ", e$message)
  })
}

# STEP 3: MAIN PROCESSING
# ------------------------------------
# Set and validate file path
file_path <- validate_file_path("/Users/annelies/Desktop/R_developments/Letterhuis.xlsx")

# Load registry and ensure directories exist
dir_registry <- load_registry(file_path)
ensure_directories(dir_registry)

# Read and validate the data
Letterhuis <- read_excel(file_path)

# Print original column names for reference
cat("\nOriginal column names:\n")
print(names(Letterhuis))

prefix <- tools::file_path_sans_ext(basename(file_path))

# Store the data with a generic name for processing
BUILDING <- Letterhuis

# Proceed with column renaming
names(BUILDING)[1] <- "DateTime"
num_cols <- ncol(BUILDING)
col_names <- c("DateTime")
for (i in 1:((num_cols - 1) / 2)) {
  col_names <- c(col_names, 
                 paste0("Ti", i),  # Temperature indoor location i
                 paste0("RHi", i)) # Relative humidity indoor location i
}
names(BUILDING) <- col_names[1:num_cols]

# Run validation after renaming
validation_results <- validate_data(BUILDING, file_path)
if (!validation_results$is_valid) {
  stop("Data validation failed:\n",
       paste(validation_results$errors, collapse = "\n"))
}
if (length(validation_results$warnings) > 0) {
  warning("Data validation warnings:\n",
          paste(validation_results$warnings, collapse = "\n"))
}

# STEP 4: DATETIME PROCESSING
# ------------------------------------
BUILDING$DateTime <- dmy_hms(BUILDING$DateTime)
BUILDING$DateTime <- floor_date(BUILDING$DateTime, unit = "hour")
BUILDING_h <- BUILDING %>%
  distinct(DateTime, .keep_all = TRUE)

# STEP 5: ADD SEASONAL DATA
# -----------------------
# Define seasons based on region
getSeasonByRegion <- function(DATES, region = "temperate_north") {
  date_fixed <- make_date(year = 2012, month = month(DATES), day = day(DATES))
  case_when(
    region == "temperate_north" ~ case_when(
      date_fixed >= as_date("2012-03-21") & date_fixed < as_date("2012-06-21") ~ "Spring",
      date_fixed >= as_date("2012-06-21") & date_fixed < as_date("2012-09-21") ~ "Summer",
      date_fixed >= as_date("2012-09-21") & date_fixed < as_date("2012-12-21") ~ "Autumn",
      TRUE ~ "Winter"
    ),
    region == "temperate_south" ~ case_when(
      date_fixed >= as_date("2012-09-21") & date_fixed < as_date("2012-12-21") ~ "Spring",
      date_fixed >= as_date("2012-12-21") & date_fixed < as_date("2013-03-21") ~ "Summer",
      date_fixed >= as_date("2012-03-21") & date_fixed < as_date("2012-06-21") ~ "Autumn",
      TRUE ~ "Winter"
    ),
    region == "tropical" ~ case_when(
      date_fixed >= as_date("2012-11-01") & date_fixed < as_date("2013-04-01") ~ "Dry",
      TRUE ~ "Wet"
    ),
    region == "monsoon" ~ case_when(
      date_fixed >= as_date("2012-06-01") & date_fixed < as_date("2012-09-01") ~ "Monsoon",
      date_fixed >= as_date("2012-09-01") & date_fixed < as_date("2013-03-01") ~ "Winter",
      TRUE ~ "Summer"
    ),
    region == "arid" ~ case_when(
      date_fixed >= as_date("2012-05-01") & date_fixed < as_date("2012-10-01") ~ "Summer",
      TRUE ~ "Winter"
    ),
    TRUE ~ NA_character_
  )
}

# *** MODIFY TYPE OF SEASON IF NEEDED FOR NEW ANALYSIS ***
BUILDING_h <- BUILDING_h %>%
  mutate(Season = getSeasonByRegion(DateTime, region = "temperate_north"))

# Define calculation functions for use in later steps
# **** CREATE LIST OF CALCULATION FUCTIONS, VARIABLES AND UNITS TO DOCUMENT METHOD (1/2) ****
source_calc_functions <- function() {
  TK <- function(T) {
    T <- as.numeric(T)
    round(T + 273.15, 2)
  }
  
  pWVP <- function(T, RH) {
    T <- as.numeric(T)
    RH <- as.numeric(RH)
    round(0.6108 * exp(17.27 * T / (T + 237.3)) * RH / 100 * 1000, 2)
  }
  
  Dwpt <- function(T, RH) {
    T <- as.numeric(T)
    RH <- as.numeric(RH)
    alpha <- log(RH / 100) + (17.27 * T / (237.3 + T))
    round((237.3 * alpha) / (17.27 - alpha), 2)
  }
  
  DwptK <- function(T, RH) {
    TK(Dwpt(T, RH))  # Now it can access TK function defined above
  }
  
  AH <- function(T, RH) {
    T <- as.numeric(T)
    RH <- as.numeric(RH)
    round((pWVP(T, RH) * 2.1674) / TK(T), 2)
  }
  
  SH <- function(T, RH, pressure = 101325) {
    T <- as.numeric(T)
    RH <- as.numeric(RH)
    round(0.622 * pWVP(T, RH) / (pressure - pWVP(T, RH)), 2)
  }
  
  MR <- function(T, RH, pressure = 101325) {
    SH(T, RH, pressure)
  }
  
  MRg <- function(T, RH, pressure = 101325) {
    round(MR(T, RH, pressure) * 1000, 2)
  }
  
  # Return all the functions in a list
  return(list(
    TK = TK,
    pWVP = pWVP,
    Dwpt = Dwpt,
    DwptK = DwptK,
    AH = AH,
    SH = SH,
    MR = MR,
    MRg = MRg
  ))
}

# Load calculation functions
calc_functions <- source_calc_functions()

# STEP 6: DOWNLOAD WEATHER DATA
# --------------------------
# Save current timeout and set new one to avoid hanging
old_timeout <- getOption("timeout")
options(timeout = 60)

# Get available weather stations
# Set station code for Uccle
# *** MODIFY STATION CODE IF NEEDED FOR NEW ANALYSIS, DEFAULT IS UCCLE, BE ***
station_id <- "064500-99999"
start_year <- year(min(BUILDING_h$DateTime, na.rm = TRUE))
end_year <- year(max(BUILDING_h$DateTime, na.rm = TRUE))

# Download and process weather data
WEATHER <- tryCatch({
  # First download the raw weather data
  weather_raw <- importNOAA(code = station_id, year = start_year:end_year, hourly = TRUE)
  
  # Then process it with the desired columns
  processed_weather <- weather_raw %>%
    mutate(
      DateTime = floor_date(as.POSIXct(date), unit = "hour")
    ) %>%
    select(
      DateTime,
      To = air_temp,             # Temperature outdoor
      Dwpto = dew_point,         # Dew point outdoor
      RHo = RH,                  # Relative humidity outdoor
      Precip = precip,           # Hourly precipitation
      Precip_6h = precip_6,      # 6-hour precipitation
      Precip_12h = precip_12     # 12-hour precipitation
    ) %>%
    mutate(across(where(is.numeric), ~round(as.numeric(.), 2))) %>%
    filter(DateTime >= min(BUILDING_h$DateTime, na.rm = TRUE) & 
             DateTime <= max(BUILDING_h$DateTime, na.rm = TRUE))
  
  # Return the processed weather data
  processed_weather
  
}, error = function(e) {
  cat("\nProblem with weather data. Check your internet connection.\n")
  stop(e)
}, finally = {
  options(timeout = old_timeout)  # Reset timeout
})

# STEP 7: DEFINE LOCATION NAMES
# --------------------------

# *** MODIFY LOCATION NAMES IF NEEDED FOR NEW ANALYSIS ***
# Create a simple data frame for location names with proper structure
location_names <- data.frame(
  Loc_number = 1:5,
  Loc_name = c("Beelden", "Archief", "Schilderijen", "Foto", "Gemengd"),
  stringsAsFactors = FALSE
)

# Print location assignments for verification
print(location_names)

# STEP 8: SAVE MAIN FILES
# ---------------------
# Save BUILDING_h and WEATHER data
save_csv(BUILDING_h, paste0(prefix, "_BUILDING_h.csv"), dir_registry)
save_csv(WEATHER, paste0(prefix, "_WEATHER.csv"), dir_registry)

# STEP 9: CREATE AND PROCESS LOCATION-SPECIFIC FILES
# ----------------------------------------------
# First, identify all location numbers from the Ti columns
Loc_num <- unique(gsub("\\D", "", names(BUILDING_h)[grepl("^Ti\\d+$", names(BUILDING_h))]))

# Process each location
for (location_num in Loc_num) {
  tryCatch({
    # Create location-specific dataframe
    loc_data <- BUILDING_h %>%
      select(DateTime, Season,
             !!sym(paste0("Ti", location_num)),
             !!sym(paste0("RHi", location_num))) %>%
      mutate(across(matches("^(Ti|RHi)"), as.numeric))  # Ensure numeric values
    
    # **** CREATE LIST OF CALCULATIONS AND UNITS TO DOCUMENT METHOD (2/2) ****
    # Add calculations
    temp_col <- paste0("Ti", location_num)
    rh_col <- paste0("RHi", location_num)
    
    # Calculate derived variables
    loc_data[[paste0("Dwpti", location_num)]] <- 
      calc_functions$Dwpt(loc_data[[temp_col]], loc_data[[rh_col]])
    
    loc_data[[paste0("AHi", location_num)]] <- 
      calc_functions$AH(loc_data[[temp_col]], loc_data[[rh_col]])
    
    # Add rolling averages with numeric check
    loc_data[[paste0("T24havg", location_num)]] <- 
      round(rollapply(as.numeric(loc_data[[temp_col]]), 
                      24, mean, align = "right", fill = NA), 2)
    
    loc_data[[paste0("T7davg", location_num)]] <- 
      round(rollapply(as.numeric(loc_data[[temp_col]]), 
                      24*7, mean, align = "right", fill = NA), 2)
    
    loc_data[[paste0("RH24havg", location_num)]] <- 
      round(rollapply(as.numeric(loc_data[[rh_col]]), 
                      24, mean, align = "right", fill = NA), 2)
    
    loc_data[[paste0("RH7davg", location_num)]] <- 
      round(rollapply(as.numeric(loc_data[[rh_col]]), 
                      24*7, mean, align = "right", fill = NA), 2)
    
    # Assign to environment with simple name
    assign(paste0("Loc", location_num), loc_data)
    
    # Save to file with full name
    save_csv(loc_data, 
             paste0(prefix, "_Loc", location_num, "_", 
                    location_names$Loc_name[location_names$Loc_number == as.numeric(location_num)], 
                    ".csv"),
             dir_registry)
    
  }, error = function(e) {
    cat("Error processing location", location_num, ":", e$message, "\n")
  })
}

# STEP 10: CREATE STRUCTURED OUTPUT
# ------------------------------
# Output structure functions
create_output_structure <- function(BUILDING_h, WEATHER, locations, file_path, station_id) {
  # Create metadata
  metadata <- list(
    file_info = list(
      original_file = basename(file_path),
      directory = dirname(normalizePath(file_path)),
      processing_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    data_summary = list(
      date_range = range(BUILDING_h$DateTime),
      total_hours = nrow(BUILDING_h),
      n_locations = length(locations),
      weather_station = station_id
    ),
    location_info = location_names,
    missing_data = list(
      building = calculate_missing_data(BUILDING_h),
      weather = calculate_missing_data(WEATHER)
    )
  )
  
  # Create main output structure
  output <- list(
    metadata = metadata,
    data = list(
      building = BUILDING_h,
      weather = WEATHER,
      locations = locations
    )
  )
  
  class(output) <- "climate_data"
  return(output)
}

# Helper function for missing data calculation
calculate_missing_data <- function(data) {
  total_rows <- nrow(data)
  sapply(data, function(x) {
    n_missing <- sum(is.na(x))
    percentage <- round(n_missing / total_rows * 100, 2)
    sprintf("%d (%.2f%%)", n_missing, percentage)
  })
}

# Custom print method for climate_data class
print.climate_data <- function(x, ...) {
  cat("\nClimate Data Analysis Results")
  cat("\n============================\n")
  
  cat("\nFile Information:")
  cat("\n----------------")
  cat(sprintf("\nOriginal File: %s", x$metadata$file_info$original_file))
  cat(sprintf("\nProcessing Date: %s", x$metadata$file_info$processing_date))
  
  cat("\n\nData Summary:")
  cat("\n------------")
  cat(sprintf("\nTime Period: %s to %s",
              format(x$metadata$data_summary$date_range[1], "%Y-%m-%d %H:%M:%S"),
              format(x$metadata$data_summary$date_range[2], "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("\nTotal Hours: %d", x$metadata$data_summary$total_hours))
  cat(sprintf("\nNumber of Locations: %d", x$metadata$data_summary$n_locations))
  cat(sprintf("\nWeather Station: %s", x$metadata$data_summary$weather_station))
  
  cat("\n\nLocation Information:")
  cat("\n-------------------\n")
  print(x$metadata$location_info)
}

# STEP 11: GENERATE FINAL OUTPUT AND REPORTS
# --------------------------------------
# Collect all location data
locations <- mget(paste0("Loc", Loc_num))

# Create structured output
results <- create_output_structure(BUILDING_h, WEATHER, locations, file_path, station_id)

# Generate missing data report
missing_data_report <- function(data, name) {
  total_vals <- nrow(data)
  na_summary <- data %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    mutate(across(everything(), ~ paste0(., " (", round((. / total_vals) * 100, 2), "%)")))
  cat("\nMissing Data Report for", name, ":\n")
  print(na_summary)
}

# Print reports and summaries
cat("\n=== ANALYSIS SUMMARY ===\n")
print(results)

cat("\n=== MISSING DATA REPORTS ===\n")
missing_data_report(BUILDING_h, "BUILDING_h")
missing_data_report(WEATHER, "WEATHER")

# Save structured output
saveRDS(results, file.path(dir_registry$paths$registry, paste0(prefix, "_processed_data.rds")))

# Print summary of saved files
cat("\n=== FILES SAVED ===\n")
cat("Directory:", data_directory, "\n")
cat("\nMain files:\n")
cat("1.", paste0(prefix, "_BUILDING_h.csv"), "\n")
cat("2.", paste0(prefix, "_WEATHER.csv"), "\n")
cat("3.", paste0(prefix, "_processed_data.rds"), " (Complete analysis results)\n")
cat("\nLocation files:\n")
for (loc in 1:nrow(location_names)) {
  cat(sprintf("%s_Loc%d_%s.csv\n", 
              prefix, 
              location_names$Loc_number[loc], 
              location_names$Loc_name[loc]))
}

# STEP 12: FILE SAFETY FUCNTIONS FOR R
# --------------------------------------

# Function to create automatic backups before file operations
create_backup <- function(file_path) {
  if (file.exists(file_path)) {
    # Create backup directory if it doesn't exist
    backup_dir <- file.path(dirname(file_path), "backups")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir)
    }
    
    # Create timestamp for backup file
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_file <- file.path(backup_dir, 
                             paste0(tools::file_path_sans_ext(basename(file_path)),
                                    "_backup_", timestamp,
                                    tools::file_ext(file_path, TRUE)))
    
    # Copy file to backup location
    file.copy(file_path, backup_file)
    return(backup_file)
  }
  return(NULL)
}

# Function to safely move R files
safe_move_file <- function(from_path, to_path) {
  # Create backup before moving
  backup_path <- create_backup(from_path)
  
  # Attempt to move the file
  tryCatch({
    # Ensure destination directory exists
    to_dir <- dirname(to_path)
    if (!dir.exists(to_dir)) {
      dir.create(to_dir, recursive = TRUE)
    }
    
    # Move file
    file.copy(from_path, to_path)
    if (file.exists(to_path)) {
      file.remove(from_path)
      message("File successfully moved with backup created at: ", backup_path)
    } else {
      stop("Failed to copy file to new location")
    }
  }, error = function(e) {
    # Restore from backup if move fails
    if (!is.null(backup_path) && file.exists(backup_path)) {
      file.copy(backup_path, from_path)
      message("Move failed. Original file restored from backup.")
    }
    stop(paste("Error moving file:", e$message))
  })
}

# Function to automatically save R scripts periodically
setup_autosave <- function(interval_mins = 5) {
  # Create autosave function
  autosave <- function() {
    current_file <- rstudioapi::getActiveDocumentContext()$path
    if (current_file != "") {
      create_backup(current_file)
      message("Autosave backup created at: ", format(Sys.time(), "%H:%M:%S"))
    }
  }
  
  # Set up timer for autosave
  timer <- later::later(function() {
    autosave()
    setup_autosave(interval_mins)  # Recursive call for continuous operation
  }, interval_mins * 60)
  
  invisible(timer)
}

# STEP 13: END OF SCRIPT MESSAGE
# --------------------------------------

# End of script message
cat("\nAnalysis complete! All data has been processed and saved.\n")
cat("Use readRDS('", file.path(data_directory, paste0(prefix, "_processed_data.rds")), 
    "') to load the complete analysis results.\n")

