# MODULE 0: DIRECTORY STRUCTURE SETUP
# =======================================

# OVERVIEW:
# This script creates the directory structure for the entire analysis workflow
# All other modules will reference these directories

# REQUIRED MODIFICATIONS FOR NEW ANALYSIS:
# marked with: *** MODIFY ... FOR NEW ANALYSIS ***
# File path to input Excel file

# IF NEED TO CREATE LIST TO DOCUMENT METHOD 
# **** CREATE LIST ... TO DOCUMENT METHOD ****
# Whole data structure

# STEP 1: FILE LOCATION AND DIRECTORY SETUP
# ------------------------------------
# Load required packages
required_packages <- c("dplyr", "tools")
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

# *** MODIFY THIS PATH FOR NEW ANALYSIS ***
# Set file path to your Excel file
file_path <- "/Users/annelies/Desktop/R_developments/letterhuis.xlsx"

# Get base directory and prefix
data_directory <- dirname(normalizePath(file_path))
prefix <- file_path_sans_ext(basename(file_path))

# Create directory structure
create_analysis_directories <- function(base_dir, prefix) {
  # Define main directories with descriptions
  directories <- list(
    "00_MethodMetaData" = list(
      description = "Method documentation and metadata (MOD0)"
    ),
    "01_BasicFiles" = list(
      description = "Primary processed data files (MOD1)"
    ),
    "02_IndoorLocations" = list(
      description = "Location-specific data files (MOD1)"
    ),
    "03_Fluctuations" = list(
      description = "Temperature and RH fluctuation analysis (MOD2)"
    ),
    "04_BasicNumRes" = list(
      description = "Basic numerical results",
      subdirs = list(
        "4-1_Averages" = "Statistical averages per location (MOD2)",
        "4-2_WeatherExtremes" = "Extreme weather event analysis (MOD2)",
        "4-3_ClimateClassThresholds" = "Determines the T/RH avg/max/min for every climate class (MOD2)",
        "4-4_ASHRAE" = "ASHRAE guideline compatibility analysis (MOD2)",
        "4-5_OtherGuidelines" = "Additional guidelines, (cold, cool, frozen) analysis (MOD2)"
      )
    ),
    "05_BasicVisRes" = list(
      description = "Basic visualization results",
      subdirs = list(
        "5-1_BasicTimePl" = "Basic time plots (MOD3)",
        "5-2_BasicBoxPl" = "Basic box plots (MOD3)",
        "5-3_BasicPsychroChrt" = "Psychrometric charts (MOD3)",
        "5-4_BasicOutdoorInd" = "Outdoor-indoor comparisons (MOD3)",
        "5-5_BasicHistogrFluct" = "Fluctuation histograms (MOD3)",
        "5-6_TimePlotComparison" = "Time plot comparisons (MOD3)"
      )
    ),
    "06_PreservMetr" = list(
      description = "Preservation metrics",
      subdirs = list(
        "6-1_ChemRisk" = "Chemical degradation risk analysis (MOD4)",
        "6-2_BioRisk" = "Biological degradation risk analysis (MOD4)",
        "6-3_MechRisk" = "Mechanical degradation risk analysis (MOD4)"
      )
    ),
    "07_PreservMetrVis" = list(
      description = "Preservation metrics visualization",
      subdirs = list(
        "7-1_ChemicalRisk" = "Chemical risk visualizations (MOD5)",
        "7-2_BioRisk" = "Biological risk visualizations (MOD5)",
        "7-3_MechRisk" = "Mechanical risk visualizations (MOD5)"
      )
    ),
    "99_Registry" = list(
      description = "Directory registry and configuration files"
    )
  )
  
  # Create directories
  for (dir_name in names(directories)) {
    main_dir <- file.path(base_dir, dir_name)
    dir.create(main_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Create subdirectories if they exist
    if (is.list(directories[[dir_name]]) && !is.null(directories[[dir_name]]$subdirs)) {
      for (subdir in names(directories[[dir_name]]$subdirs)) {
        dir.create(file.path(main_dir, subdir), showWarnings = FALSE)
      }
    }
  }
  
  # Create directory registry
  registry <- list(
    base_dir = base_dir,
    prefix = prefix,
    directories = directories,
    paths = list(
      method = file.path(base_dir, "00_MethodMetaData"),
      basic = file.path(base_dir, "01_BasicFiles"),
      locations = file.path(base_dir, "02_IndoorLocations"),
      fluctuations = file.path(base_dir, "03_Fluctuations"),
      num_results = file.path(base_dir, "04_BasicNumRes"),
      vis_results = file.path(base_dir, "05_BasicVisRes"),
      preserv_metrics = file.path(base_dir, "06_PreservMetr"),
      preserv_vis = file.path(base_dir, "07_PreservMetrVis"),
      registry = file.path(base_dir, "99_Registry")
    ),
    created = Sys.time()
  )
  
  # Save registry in registry directory
  saveRDS(registry, 
          file.path(registry$paths$registry, 
                    paste0(prefix, "_directory_registry.rds")))
  
  # Print creation summary
  cat("\nAnalysis directories created for:", prefix)
  cat("\n=====================================")
  for (dir_name in names(directories)) {
    cat("\n", dir_name, "-", directories[[dir_name]]$description)
    if (is.list(directories[[dir_name]]) && !is.null(directories[[dir_name]]$subdirs)) {
      for (subdir in names(directories[[dir_name]]$subdirs)) {
        cat("\n  ├──", subdir, "-", directories[[dir_name]]$subdirs[[subdir]])
      }
    }
  }
  cat("\n\nDirectory registry saved as:", 
      paste0(prefix, "_directory_registry.rds"), 
      "in 99_Registry\n")
  
  return(registry)
}

# Create directories and get registry
dir_registry <- create_analysis_directories(data_directory, prefix)

# End of MOD0_Structure.R
