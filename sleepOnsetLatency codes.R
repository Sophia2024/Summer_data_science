File Name: SleepOnsetLatency.R
File created date: summer 2023

###############preparation and setting ########################################

# Install and load the stringr package
install.packages("stringr")
install.packages("tidyverse")
library(tidyverse, stringr)

# set the working directory
setwd("~/Library/CloudStorage/Box-Box/UTX000")

# Specify the path to the folder fitbit
fitbit_folder <- "~/Library/CloudStorage/Box-Box/UTX000/fitbit"

###################get participantID#########################################

# Get the list of folder names under folder fitbit
participantID <- list.dirs(fitbit_folder, full.names = FALSE) 
participantID <- participantID[participantID != ""]

# Print the all the folder names under folder fitbit
print(participantID)

# timescounter that counts effective data and missing data
# missingdata means that the file does not have any data under sleep column
# notmissing is the opposite
filenotexist <- 0
missingdata <- 0
notmissing <- 0
filenamemissing <- c()
filenamehaving <- c()
rem_start_time <- c()
sleep_start_time <- c()
sleep_endDate <- c()
participantID2 <- c()
filenotexistname <- c()
nap <- 0
nap_with_stage <- 0

#nap_df <- data.frame(
 # participantID = participantID2,
  #SleepStartTime = sleep_start_time,
  #sleepEndDate = sleep_endDate,
  #Nap = nap,
  #napWithStage = nap_with_stage

#)


# Iterate over the participantID and import the fitbit_daily_records file
for (id in participantID) {
  # Specify the path to the file within each participant's folder
  file_path <- paste0(fitbit_folder, "/", id, "/fitbit_daily_records.csv") 
  
  # Check if the fitbit_daily_records file exists for the participant
  if (!file.exists(file_path)) {
    filenotexist <- filenotexist + 1
    filenotexistname <- id
    warning(paste("File", file_path, "does not exist. Skipping..."))
  } else {
    # File exists, perform operations on the file
    fitbit_daily_records <- read.csv(file_path)
    
    # Check if fitbit_daily_records$sleep column is empty
    if (any(length(fitbit_daily_records$sleep) == 0 | all(is.na(fitbit_daily_records$sleep)))) {
      filenamemissing <- append(filenamemissing, id)
      missingdata <- missingdata + 1
    } else {
      notmissing <- notmissing + 1
      filenamehaving <- append(filenamehaving, id)
      
      # For loop to iterate over observations in fitbit_daily_records$sleep
      for (observation in fitbit_daily_records$sleep) {
        sleep_observation_list <- c()
        
        # extract all the sleep/nap events into sleep_observation_list separately for each night sleep
        all_sleep_index <- gregexpr("dateOfSleep", observation)[[1]] 
        if (length(all_sleep_index) > 1){
          for (i in 1: (length(all_sleep_index) - 1)){
            start_index <- all_sleep_index[i]
            end_index <- all_sleep_index[i + 1]
            sleep_observation <- str_sub(observation, start_index, end_index)
            sleep_observation_list <- append(sleep_observation_list, sleep_observation)
          }
          i <- length(all_sleep_index)
          start_index <- all_sleep_index[i]
          end_index <- str_length(observation)
          last_observation <- str_sub(observation, start_index, end_index)
          sleep_observation_list <- append(sleep_observation_list, last_observation)
        }else{
          sleep_observation_list <- observation
        }
        #nap <- 0
        #nap_with_stage <- 0
  
        # run a for loop to see if each sleep/nap events in a night fits our criteria of mainsleep with rem data
        for (observation_period in sleep_observation_list){
          # Check if isMainSleep is True and contains "rem"
          if (grepl("isMainSleep': True", observation_period) && grepl("rem", observation_period)) {
            rem_index <- regexpr("rem", observation_period)[1]
            last_match_rem <- tail(rem_index, 1)
            rem_all_index <- gregexpr("rem", observation_period)[[1]]
            if (length(rem_all_index) != 1){
              rem_observation <- str_sub(observation_period, rem_index - 36, rem_index - 14) # -25~-14 time; -36~-14 date+time
              rem_observation_modified <- str_split(rem_observation, "T")
              
              rem_start_time <- append(rem_start_time, rem_observation)
              # date + time example: 2020-05-24T13:40:00.000 --> format = "%Y-%m-%dT%H:%M:%OS"
              
              index <- regexpr("endTime", observation_period)[1]
              sleep_endDate_observation <- str_sub(observation_period, index + 11, index + 33) # 11-20 date; 11-33 date + time
              sleep_endDate <- append(sleep_endDate, sleep_endDate_observation)
              
              participantID2 <- append(participantID2, id)
              
              levels_index <- regexpr("levels", observation_period)[1]
              sleep_observation <- str_sub(observation_period, levels_index + 33, levels_index + 55) # 44-55 time; 33-55 date + time
              sleep_start_time <- append(sleep_start_time, sleep_observation)
            }
          }
        }
      }
    }
  }
}

# Convert start and end time strings to POSIXlt objects
rem_start_posix <- strptime(rem_start_time, format = "%Y-%m-%dT%H:%M:%OS")
sleep_start_posix <- strptime(sleep_start_time, format = "%Y-%m-%dT%H:%M:%OS")

# Adjust end time if it is earlier than start time (next day scenario)
#rem_start_posix[rem_start_posix < sleep_start_posix] <- rem_start_posix[rem_start_posix < sleep_start_posix] + 86400

# Calculate the duration as the difference between end time and start time
duration <- as.numeric(difftime(rem_start_posix, sleep_start_posix, units = "min"))

# Example output data
output_data <- data.frame(
  participantID = participantID2,
  SleepStartTime = sleep_start_time,
  sleepEndDate = sleep_endDate,
  remStartTime = rem_start_time,
  remOnsetLatency = duration
)

# Specify the file path and name for the new CSV file
output_file <- "~/Library/CloudStorage/Box-Box/UTX000/Data Wrangling Coding Files/remOnsetLatency_data_revisited3.csv"

# Create the directory if it doesn't exist
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Write the output data to the CSV file
write.csv(output_data, file = output_file, row.names = FALSE)
