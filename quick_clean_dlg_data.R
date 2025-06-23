# Install and Load required package
install.packages("dplyr")
install.packages("stringr")
library(dplyr)
library(stringr)

# Set current working directory
setwd("/Users/raymondlukwago/Documents/WatSSUP/HYT_Grant/250621_district_water_data")

# Code line for clearing the Environment Panel
rm(list=ls())

# Function to read csv files and load them into working environment
read_csv_files <- function(file_list, expected_schema = NULL) {
  summary_report <- list()
  
  for (name in names(file_list)) {
    file_path <- file_list[[name]]
    tryCatch({
      df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

      # check if the schema is provided
      if (!is.null(expected_schema)) {
        missing_columns <- setdiff(expected_schema, colnames(df))
        if (length(missing_columns) > 0) {
          warning_msg <- paste("!!!", name, "is missing columns:", paste(missing_cols, collapse = ", "))
          message(warning_msg)
          summary_report[[name]] <- list(status = "Warning", missing = missing_columns)
        } else {
          message(paste("$$", name, "passed column validation."))
          summary_report[[name]] <- list(status = "OK")
        }
      } else {
        message(paste("$$", name, "loaded successfully."))
        summary_report[[name]] <- list(status = "OK")
      }
      assign(name, df, envir = .GlobalEnv)
    }, error = function(e) {
      error_msg <- paste("XX Failed to read", file_path, "-", e$message)
      message(error_msg)
      summary_report[[name]] <- list(status = "Error", message = e$message)
    })
  }
  return(summary_report)
}

# List of functions to support in column name cleaning

# Func 1: Remove identified Columns
remove_columns <- function(df, to_remove_col) {
  df <- df[,!(names(df) %in% to_remove_col)]
  return(df)
}

# Func 2: Rename Column Names
rename_columns <- function(df, rename_cols) {
  report <- list(success = character(), skipped = character(), duplicated = character())
  
  for (old_name in names(rename_cols)) {
    new_name <- rename_cols[[old_name]]
    matches <- which(names(df) == old_name)
    
    if (length(matches) == 1) {
      names(df)[matches] <- new_name
      report$success <- c(report$success, paste(old_name, "→", new_name))
    } else if (length(matches) > 1) {
      names(df)[matches[1]] <- new_name
      report$duplicated <- c(report$duplicated, paste(old_name, "renamed first of", length(matches), "instances"))
    } else {
      report$skipped <- c(report$skipped, paste(old_name, "not found"))
    }
  }
  
  message("\n✅ Rename Summary:")
  if (length(report$success)) {
    message(" • Renamed:")
    for (m in report$success) message("   • ", m)
  }
  if (length(report$duplicated)) {
    message(" • Duplicates handled:")
    for (m in report$duplicated) message("   • ", m)
  }
  if (length(report$skipped)) {
    message(" • Skipped:")
    for (m in report$skipped) message("   • ", m)
  }
  
  return(df)
}

# Func 3: Clean-Up column name strings
clean_column_names <- function(df) {
  stopifnot(is.data.frame(df))
  
  names(df) <- vapply(names(df), function(nm) {
    nm2 <- str_trim(nm)  # remove leading/trailing white space
    # 1. Strip leading underscores
    nm2 <- str_remove(nm2, "^_+")
    # 2. Strip leading numbers + dots + optional underscore or space
    #    ^\d+(?:\.\d+)*[_ ]*
    nm2 <- str_remove(nm2, "^\\d+(?:\\.\\d+)*[_ ]*")
    # 3. Replace spaces with underscores
    nm2 <- str_replace_all(nm2, "\\s+", "_")
    # 4. Capitalise first letter, rest left untouched
    nm2 <- str_replace(nm2, "^([a-z])", toupper)
    return(nm2)
  }, FUN.VALUE = character(1))
  return(df)
}

# Func 4: Replace values in target column using values from source column when
# they meet a defined condition
replace_if_source_present <- function(df, target_col, source_col,
                                      condition_value = "Other/Not on the list") {
  df %>%
    mutate(
      !!target_col := if_else(
        .data[[target_col]] == condition_value & 
          !is.na(.data[[source_col]]) & .data[[source_col]] != "",
        .data[[source_col]],
        .data[[target_col]]
      )
    )
}

# Func 5: Master Function to standardize column names
standardizeColumnNames <- function(df_list, to_remove_col = character(),
                                   rename_cols = list()) {
  standardized_dfs <- lapply(df_list, function(df) {
    df <- remove_columns(df, to_remove_col)
    df <- rename_columns(df, rename_cols)
    df <- clean_column_names(df)
    df <- replace_if_source_present(
      df,target_col = "Source_name",
      source_col = "Alter_source_name",
      condition_value = "Other/Not on the list")
    df <- remove_columns(df, to_remove_col = "Alter_source_name")
    return(df)
  })
  # Assign each standardized data-frame into Global environment
  list2env(standardized_dfs, envir = .GlobalEnv)
  return(standardized_dfs)
}

# Load district local government data sets into work environments
district_files <- list(
  yumbe_dlg = "FORM_1_A_yumbe.csv",
  madiokollo_dlg = "FORM_1_A_madiokollo.csv",
  terego_dlg = "FORM_1_A_terego.csv"
)

csv_file_import_report <- read_csv_files(file_list = district_files, expected_schema = NULL)
print(csv_file_import_report)

# Do Data Prep: Current Scope caters only for column cleaning
df_list <- list(madiokollo_dlg = madiokollo_dlg,
                terego_dlg = terego_dlg,
                yumbe_dlg = yumbe_dlg)

to_remove_columns <- c(
  "1.6 GPS coordinated", "_1.6 GPS coordinated_altitude", "Take a photo of the water source",
  "1.7.2 Where is the water point located?", "IDP Camp Name",
  "Refugee Settlement Name", "In which Zone is it","2.1 What is the type of source",
  "Number of spouts", "Indicate the number of tapstands",
  "If other, explain:", "2.2.1. Indicate the type of mother scheme/system",
  "2.2.2.If part of a system, indicate the name of mother scheme/system",
  "2.3 Is the water source located within the premises?",
  "2.4 How many households are found within the premises?",
  "2.5.1 Type of users and estimated number of users/Students",
  "2.5.1 Type of users and estimated number of users/Patients",
  "2.5.1 Type of users and estimated number of users/Soldiers/Police officers",
  "2.5.1 Type of users and estimated number of users/Households - host community",
  "2.5.1 Type of users and estimated number of users/Househods - refugees",
  "2.5.1 Type of users and estimated number of users/other",
  "Estimated number of households (host) using this source",
  "Average number of people per (host) household",
  "Estimated number of refugees' households using this source",
  "Average number of people per refugees' household",
  "total_households", "Total number of households using the source",
  "2.5.2 Of the total number of households using the source, how many are located 500 meters or less to the source (5 minutes or less)",
  "2.5.2 Of the total number of households using the source, how many are located between 500 and 1000 meters to the source (5 to 10 minutes)",
  "tenminplus",
  "2.5.2. How many are located more than 1000 meters to the source (more than 10 minutes) ${tenminplus}",
  "2.6.1 Select the type of institution",
  "2.6.2 Estimated number of students/teachers using this source",
  "2.6.2 Estimated number of patients/health staff using this source",
  "2.6.2 Estimated number of people using this source within the institution",
  "2.6.2 Number of people withinh the institution using this source",
  "3.4 Source of funding/Private",
  "3.4 Source of funding/NGO", "3.4 Source of funding/GoU Central Gvt",
  "3.4 Source of funding/GoU Local Gvt", "3.4 Source of funding/Other",
  "Name of private funding", "Name of NGO",
  "Central Govt (specify):", "Local Govt (specify):",
  "Other (specify):",
  "3.5 Current Ownership/Private", "3.5 Current Ownership/Community",
  "3.5 Current Ownership/Institutional", "3.5 Current Ownership/Other",
  "Specify institution", "Other institution (specify the kind of institution)",
  "Name of private funding", "Other type of ownership (specify):",
  "Name of the institution",
  "Name of the individual", "Name of the operator", "Specify institution",
  "Other (specify):",
  "Date of training",
  "Say if the date of construction is exact or an estimate",
  "4.6. If WSC is functional, tick appropriate box(es)/WSC is collecting user fees",
  "4.6. If WSC is functional, tick appropriate box(es)/WSC undertakes regular servicing/minor repairs",
  "4.6. If WSC is functional, tick appropriate box(es)/WSC is holding regular meetings",
  "4.6. If WSC is functional, tick appropriate box(es)/Environment/sanitation around the source is ok",
  "4.7. If WSC is not functional, indicate the main reason(s) why",
  "4.7. If WSC is not functional, indicate the main reason(s) why/Source dried up/low yield",
  "4.7. If WSC is not functional, indicate the main reason(s) why/WSC not trained",
  "4.7. If WSC is not functional, indicate the main reason(s) why/Majority of members shifted/moved/died",
  "4.7. If WSC is not functional, indicate the main reason(s) why/Alternative water facility nearby",
  "4.7. If WSC is not functional, indicate the main reason(s) why/source brokedown beyond means of community",
  "4.7. If WSC is not functional, indicate the main reason(s) why/WSC not commited",
  "4.7. If WSC is not functional, indicate the main reason(s) why/other",
  "Other (specify):",
  "Tick the applicable position(s) below/Chairperson", "Tick the applicable position(s) below/Secretary",
  "Tick the applicable position(s) below/Vice-chairperson", "Tick the applicable position(s) below/Treasurer", "If R.D, give reasons why",
  "5.2 If the water source is non-funtional or not in use, when did it break down?",
  "5.3 Give the reason(s) why the source is non-functional or not in use/dry/low yielding",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Technical breakdown",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Water quality",
  "5.3 Give the reason(s) why the source is non-functional or not in use/WSC not functioning",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Silted (Valley tanks/Dams)",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Leaking (Rainwater Harvesting Tanks)",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Alternative source nearby",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Vandalism",
  "5.3 Give the reason(s) why the source is non-functional or not in use/Other",
  "Technical breakdown (specify):", "Water quality (specify):",
  "Water quality (specify):/Smelly water", "Water quality (specify):/Tasty water (salty for example)",
  "Water quality (specify):/Brown water", "Water quality (specify):/Other coloured water",
  "Water quality (specify):/Suspended particles", "Water quality (specify):/Oily water",
  "Water quality (specify):/Dirty water", "Water quality (specify):/Itchy water",
  "Water quality (specify):/Other", "Other (specify):", "Vandalism (specify):", "Other (specify):",
  "5.4 For both non-functional and not used sources, give more details and explainations of the main reason(s) why",
  "5.6 Give details of the repairs done",
  "6.1 Is there a latrine within 10 m of the water point?", "6.2 Is there a latrine uphill of the water point?",
  "6.3 Are there any other sources of pollution within 10 m of the water point? (e.g. animal breeding, cultivation, roads, industry, solid waste, etc.)",
  "6.4 Is the drainage faulty, causing stagnant water within 2 meters of the water point?",
  "6.5 Is the drainage channel cracked, broken or need cleaning?", "6.6 Is the fence missing or damaged, allowing animal entry?",
  "6.7 Is the apron less than 1m radius?", "6.8 Does spilt water collect in the apron area?",
  "6.9 Is the apron cracked or damaged?", "6.10 Is the handpump loose at the point of attachment to apron?",
  "6.11 Is the well-cover damaged", "yesnolatrined", "yesnolatrineupd", "yesnopollutiond",
  "yesnodrainagefaultyd", "yesnodrainagebrokend", "yesnofenced", "yesnoaprond",
  "yesnospiltwaterd", "yesnoaproncrackedd", "yesnohploosed", "yesnocoverdamagedd",
  "deepbore_quality", "Deep Borehole Risk assessment", "sh_well_quality",
  "Shallow Well risk assessment", "6.1 Is the nearest latrine or sewer within 100 m of pumphouse?",
  "6.2 Is the nearest latrine a pit latrine that percolates to soil (i.e. unsewered)?",
  "6.3 Is there any source of other pollution within 50m? (animal breeding, cultivation, roads, industry, solid waste, etc.)",
  "6.4 Is there an uncapped well within 100m of the borehole?",
  "6.5 Is the drainage around pumphouse faulty, permitting ponding and/or leakage to ground?",
  "6.6 Is the fencing missing or damaged, that could allow animals access",
  "6.7 Is the floor of the pumphouse permeable to water?",
  "6.8 Does water form pools in the pumphouse?","6.9 Is the well seal insanitary?",
  "yesnolatrineneard", "yesnolatrineunseweredd", "yesnopollutionfiftyd", "yesnouncappedd",
  "yesnodrainagearoundd", "fencingdamagedd", "yesnopermeabled", "yesnopoolsd",
  "yesnoinsanitaryseald", "deepbore_mec_quality", "Mecanical BH risk assessment",
  "6.1 Is the spring unprotected", "6.2 Is the masonry protecting the spring faulty?",
  "6.3 Is the blackfill area behind the retaining wall eroded?", "6.4 Does spilt water flood the collection area?",
  "6.5 Is the fence absent or damaged?",
  "6.6 Can animal have access within 10m of the spring?", "6.7 Is there a latrine uphill and/or withinh 30m of the spring?",
  "6.8 Does surface water collect uphill of the spring?", "6.9 Is the diversion ditch above the spring absent or non functional?",
  "6.10 Are there any other sources of pollution uphill of the spring? (animal breeding, cultivation, roads, industry, solid waste, etc.)",
  "yesnounprotectedd", "yesnomasonryd", "yesnoblackfilld", "yesnospiltwatd", "yesnofenceabsentd", "yesnoanimald", "yesnolatrineupandneard",
  "yesnosurfaced", "yesnodiversionditchd", "yesnootherpollutiond", "spring_quality", "Spring Risk assessment",
  "6.1 Are any tap dirty or in poor conditoons?","6.2 Do any standpipe leak at sample site?",
  "6.3 Is the drainage faulty (does water collect around the sample site)?",
  "6.4 Can source of pollution be seen within 30m of the sampling area (animal breeding, cultivation, roads, industry, solid waste, etc.)",
  "6.5 Has there been discontinuity of supply within the last 10 days at sample sit?",
  "6.6 Are there any exposed network pipes close to the sampling area?",
  "6.7 Are there any leackage visible from the network close to the sampling area",
  "6.8 Is the storage tank cover absent or in poor conditions?", "6.9 Is the service reservoir cracked or leaking?",
  "6.10 Are the air vents poorly designed so that contaminents could enter the storage tank?",
  "6.8 Do the community report any pipe breaks in the last week?", "6.9 Is the main pipe exposed anywhere in the Parish?",
  "yesnotapdirtyd", "yesnostandpipeleakd", "yesnodrainagefaultd", "pollutionsamplingaread",
  "yesnodiscontinuityd", "pipesexposedd", "pipesleakd", "storagetankcoverd", "reservoircrackedd", "yesnoairventsd",
  "yesnocommunitypiped", "yesnopipeexposedparishd", "tap_quality", "Public stand post and Kiosk Risk assessment",
  "tap_quality_001", "Yard tap Risk assessment", "6.1 Is rainwater collected in an open container?",
  "6.3 Is guttering that collects water dirty or blocked?",
  "6.4 Are the top or walls of the tank cracked or damaged?", "6.5 Is water collected directly from the tank (no tap on the tank)?",
  "6.6 Is there a bucket is use and is left where it can become contaminated?", "6.7 If there is a tap,is it leaking or damaged?",
  "6.8 Is the concrete floor under the tap defective or dirty?",
  "6.9 Is there any source of pollution around the tank or water collection area (animal breeding, cultivation, roads, industry, solid waste, etc.)?",
  "6.10 Is the tank clean inside?", "yesnoraincontainerd", "yesnorainroofd", "yesnogutteringd", "yesnorainwallcrackedd", "yesnoraindirectd",
  "yesnorainbucketd", "yesnorainleakd", "rainconcreted", "yesnorainpollutiond", "yesnoraintankcleand", "tap_quality_002",
  "Rain water harvesting Risk Assessment", "7. Other information as required by the DWO", "8.1 Respondent name:",
  "8.2 Respondent gender", "8.3 Repondent telephone number", "8.4 Respondent title/responsibility", "Date of data collection",
  "Name of Enumerator", "Title/designation of enumerator", "Other, specify", "Phone number of enumerator", "Volume of tank (in Liters)",
  "Volume of Dam (in m3)", "Volume of valley tank (in m3)", "2.4 Are there any households within the premise of the institution?",
  "Estimated number of soldiers using this source",
  "If 'other', specify typer of users", "If 'other', specify number of other users", "calculate_oNMRc8cV2", "calculate_wT7SIOCx1",
  "calculate_HEyY4vFQD", "How many people from institutions are using this source?", "_id", "_uuid", "_validation_status", "_notes",
  "_status", "__version__", "_tags", "_index", "2.6.2 Specify the type of institution", "Total number of users: ${total_users}",
  "6.2 Are there visible signs of contamination on the roof  (plants excreta, dust, ect.)",
  "4.6. If WSC is functional, tick appropriate box(es)", "Tick the applicable position(s) below"
)

rename_map <- list(
  "1.1 District" = "District", "1.2 County" = "County", "1.3 Sub county" = "Sub-county", "1.4 Parish" = "Parish", 
  "1.5 Village" = "village", "1.5.1 water source name" = "source_name",
  "If the water source is not on the list, specify the name of the source" = "alter_source_name", 
  "1.5.2 What is the DWD number of the source" = "DWD_number", "_1.6 GPS coordinated_latitude" = "latitude",
  "_1.6 GPS coordinated_longitude" = "longitude", "_1.6 GPS coordinated_precision" = "coordinate_precision",
  "Take a photo of the water source_URL" = "source_photo_url",
  "2.5.1 Type of users and estimated number of users" = "customer_demographic",
  "3.1 Date of construction" = "commission_date",
  "3.4 Source of funding" = "funding_source",
  "4.1. Type of management" = "management_type",
  "4.2. Does this source have a  Water and Sanitation Committee (WSC) established?" = "WSC_exist",
  "4.3. When was the WSC established?" = "WSC_date_establish", "4.4. Has the WSC been trained?" = "WSC_trained",
  "4.5. Is the WSC functional" = "WSC_functional",
  "4.8. Number of members on WSC" = "WSC_members",
  "4.9. Number of active members on WSC" = "WSC_members_active", "4.10. Number of women on WSC" = "WSC_members_women", 
  "4.11. Are there any women holding key positions?" = "WSC_women_positions",
  "4.12. Number of women holding key positions" = "WSC_women_number",
  "5.1 Functionality of the water source" = "functionality",
  "5.3 Give the reason(s) why the source is non-functional or not in use" = "non_functionality_reason",
  "total_users" = "total_users_served", "How many people are using this source?" = "estimated_water_users",
  "Indicate the month/year of last repair" = "last_repair_datetime", "5.5 Was there any major repair done?" = "Is_major_repair",
  "2.6 Does this source serve and institution?" = "serves_institution"
)

# Process data frame columns
cleaned_dfs <- standardizeColumnNames(df_list, to_remove_columns, rename_map)

# Merge all three district local government datasets across shared columns with
# full join
merged_dataframe <- Reduce(function(x, y) {
  merge(x, y, by = intersect(names(x), names(y)), all = TRUE)
}, cleaned_dfs)

# Write and save combined duplicates_and_missing csv
write.csv(merged_dataframe, '250621_dlg_water_supply_inventory.csv', row.names = FALSE)
















