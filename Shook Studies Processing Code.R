# Shook studies data processing code
# Written by G. Petrucci Jr.
# Created: 11-3-2022
# Edited: 12-2-2022

rm(list = ls()) # clear out the R environment

# Install necessary libraries----
library(tidyverse)
library(devtools)
withr::with_envvar(c(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true"), 
                   remotes::install_github('robmarcotte/MOCAfunctions'))
library(MOCAfunctions)
library(MOCAModelData)
library(readxl)
library(PhysicalActivity)
library(lubridate)
library(beepr)


# MOCAfunctions library dependencies----
library(data.table)
library(tidyverse)
library(lubridate)
library(GGIR) # for ENMO metrics
library(slider) # needed for Crouter 2010 method
library(randomForest) # needed for Staudenmayer 2015 method 

# Add working directory for files----
wd = '/Volumes/HBCD_PE_x64/Shook Studies/P1 1sec epoch' # where the AG csv files are (Files are in Mode 13)
files <- list.files(wd)
n_files <- length(files)
# Add in body weight data for kcal estimates
body_weight <- read.csv('/Users/gregpetruccijr./Desktop/P1 Participant weights.csv') # body weights for particpants in P1

# Read ActiGraph data function 
read_ag = function(filepath, ENMO_calibrate = F, device_serial_calibrate = F, calibration_file, parse_timestamp =T, samp_freq = 80, sf_coerce = F){
  
  check_data = fread(filepath,header = F,skip = 10, nrows = 1)[1,]
  if(is.numeric(check_data[1,])){
    file_data = fread(filepath,header = F, skip = 10, stringsAsFactors = F)
  } else {
    file_data = fread(filepath,header = T, skip = 10, stringsAsFactors = F)
  }
  
  
  ag_header = read.csv(filepath, header = F,stringsAsFactors = F, nrows = 10)
  device_serial = str_split(ag_header[2,],'Number: ')[[1]][2]
  start = str_split(ag_header[3,],'Time ')[[1]][2]
  date = str_split(ag_header[4,],'Date ')[[1]][2]
  # frequency = as.numeric(str_split(str_split(ag_header[1,],'at ')[[1]][3],' Hz')[[1]][1])
  
  epoch_full = str_split(ag_header[5,],' 00:')[[1]][2]
  epoch_temp = str_split(epoch_full,':')
  epoch = (as.numeric(epoch_temp[[1]][1])*60) + (as.numeric(epoch_temp[[1]][2]))
  
  file_length = nrow(file_data)
  date_time_start = mdy_hms(paste(date,start, sep = ' '))
  
  # if(epoch < 1){
  #   Timestamp = seq(from = date_time_start,to = (date_time_start + (file_length/frequency)), by = 1/frequency)[1:nrow(file_data)]
  #   
  #   if(any(colnames(file_data) == 'Timestamp')){
  #     file_data = file_data %>% dplyr::select(-Timestamp)
  #   }
  #   
  #   # For Raw data
  #   file_data = file_data %>% mutate(`Accelerometer X` = as.numeric(`Accelerometer X`),
  #                                    `Accelerometer Y` = as.numeric(`Accelerometer Y`),
  #                                    `Accelerometer Z` = as.numeric(`Accelerometer Z`))
  #   
  #   # Possible that ACC signal actual vs expected samp_freqs don't match. If that's the case and user wants a single file, resample ACC signal to match expected samp_freq
  #   # Note: Specific to ActiGraph files since sampling frequency is embedded in file header
  #   sf_match = data.table::fread(filepath, nrow = 1)$V16 == samp_freq
  #   if(sf_match == F & sf_coerce == T){
  #     warning('Actual and expected Acceleration signal frequencies do not match. Resampling to expected SF....')
  #     
  #     # Resample acceleration signals to proper sampling frequency
  #     temp_data = data.frame(HEADER_TIME_STAMP = Timestamp,
  #                            X = file_data$`Accelerometer X`,
  #                            Y = file_data$`Accelerometer Y`,
  #                            Z = file_data$`Accelerometer Z`)
  #     
  #     # Resample signal using MIMSunit package
  #     temp_data = MIMSunit::interpolate_signal(temp_data, sr = samp_freq)
  #     
  #     # Resample signal using signal package... Incomplete
  #     # for(col_index in 1:ncol(file_data)){
  #     #   temp
  #     #   # temp_signal = signal::interp(file_data[,..col_index], samp_freq/frequency, )
  #     #
  #     #   temp_data = bind_cols(temp_data, temp_signal)
  #     # }
  #     
  #     
  #     file_data = temp_data %>% dplyr::select(-HEADER_TIME_STAMP) %>%
  #       dplyr::rename(`Accelerometer X` =  X,
  #                     `Accelerometer Y` =  Y,
  #                     `Accelerometer Z` =  Z)
  #     
  #     Timestamp = seq(from = date_time_start,to = (date_time_start + (file_length/frequency)), by = 1/samp_freq)[1:nrow(file_data)]
  #     frequency = samp_freq
  #     
  #   }
  #   
  #   
  #   file_data = mutate(file_data,
  #                      VM = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2),
  #                      VMcorrG = abs(sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2)-1))
  #   
  #   colnames(file_data) = c('AxisX','AxisY','AxisZ','VM','VMcorrG')
  #   
  #   
  #   if(ENMO_calibrate == T){
  #     if(nrow(file_data) > 12*60*60*frequency){
  #       C = g.calibrate(filepath,use.temp = F, printsummary=F)
  #     } else {
  #       C = list(offset = c(0,0,0),
  #                scale = c(1,1,1))
  #     }
  #     
  #     if(device_serial_calibrate == T){
  #       if(C$offset[1] == 0 & C$scale[1] == 1){
  #         
  #         device_serial_index = str_which(calibration_file$Serial,device_serial)
  #         
  #         if(length(device_serial_index) == 0){
  #           
  #         } else {
  #           C$offset[1] = calibration_file$Offset_X[device_serial_index]
  #           C$offset[2] = calibration_file$Offset_Y[device_serial_index]
  #           C$offset[3] = calibration_file$Offset_Z[device_serial_index]
  #           
  #           C$scale[1] = calibration_file$Scale_X[device_serial_index]
  #           C$scale[2] = calibration_file$Scale_Y[device_serial_index]
  #           C$scale[3] = calibration_file$Scale_Z[device_serial_index]
  #           
  #         }
  #       }
  #     }
  #     
  #     file_data = mutate(file_data,
  #                        calX = AxisX*C$scale[1] + C$offset[1],
  #                        calY = AxisY*C$scale[2] + C$offset[2],
  #                        calZ = AxisZ*C$scale[3] + C$offset[3],
  #                        ENMO = (sqrt(calX^2 + calY^2 + calZ^2)-1)*1000) # convert to milli-gravitational units
  #     
  #     file_data = mutate(file_data, ENMO = ifelse(ENMO < 0,0,ENMO))
  #     
  #     colnames(file_data) = c('AxisX','AxisY','AxisZ', 'VM','VMcorrG','calX','calY','calZ','ENMO')
  #   }
  
  # } else {
  # If Count data does not have column names when being read in
  # gp changes for Shook studies
  colnames(file_data) <- c('Axis1', 'Axis2','Axis3', 'Steps')[1:ncol(file_data)] # hardcoded column names may result in issues
  
  
  frequency = epoch
  
  file_data = mutate(file_data, VM = sqrt(Axis1^2 + Axis2^2 + Axis3^2))
  
  Timestamp = seq(from = date_time_start,to = (date_time_start + file_length*epoch), by = epoch)[1:nrow(file_data)]
  
  
  
  
  if(parse_timestamp == T){
    new_data = dplyr::bind_cols(filename = basename(filepath),
                                Timestamp = Timestamp,
                                Date = lubridate::date(Timestamp),
                                Time = format(Timestamp, format = "%H:%M:%S"),
                                file_data)
    colnames(new_data) = c('Filename','Timestamp','Date','Time',colnames(file_data))
    
  } else {
    
    file_data = dplyr::bind_cols(filename = basename(filepath),
                                 Timestamp = Timestamp,
                                 file_data)
    
  }
  
  return(new_data)
}

# Pre-dispose P1 Master data list
P1_master_list = list()

# start for loop to process P1 participant files----
for(i in 1:length(1:n_files)){

file_i= files[i]
  file_i_wd <- paste(wd, file_i, sep='/')
    current_participant <- str_split(file_i, ' ')[[1]][1]
      bw_inds<- which(str_detect(body_weight$Filename, current_participant))
        bw <- body_weight$Bodyweight[bw_inds]

# Read in AG data
  ag_data = read_ag(file_i_wd, ENMO_calibrate = F, device_serial_calibrate = F)
  
# Use Choi wear time alg to detect non-wear, with default parameters----
  temp_choi <- wearingMarking(ag_data, frame=90, perMinuteCts = 60, TS='Timestamp', cts='Axis1', 
                              getMinuteMarking = T)
  # Expand Choi wear marking output to 1 sec epoch so it matches ag_data
  first_time <- temp_choi$Timestamp[1]
  n_choi<- nrow(temp_choi)
  new_Timestamp <- seq(ymd_hms(first_time),length.out=n_choi*60, by = 'sec')
  new_n <- length(new_Timestamp)
  new_choi <- data.frame(Timestamp=rep(NA, new_n), wearing=rep(NA, new_n),
                         weekday=rep(NA, new_n),day=rep(NA, new_n))
  new_choi$Timestamp <- new_Timestamp
  new_choi$wearing <- rep(temp_choi$wearing, each=60)
  new_choi$weekday <- rep(temp_choi$weekday, each=60)
  new_choi$day <- rep(temp_choi$day, each=60)
  
    wear_data <- new_choi %>% filter(wearing=='w') # exclude non-wear determined by Choi
     # calculate wear seconds on days that do have valid wear.. what about days that don't?
        wear_days <- wear_data %>% group_by(day) %>% summarise(wear_secs=n())
        # find days with >= 360000 secs (10 hours)
        valid_wear_days <- wear_days %>% filter(wear_secs > 36000)
      
      # filter out days that don't meet the valid wear days criteria
       days_to_remove <- subset(wear_days, !(day %in% valid_wear_days$day))[,1]
       days_to_remove <- as.data.frame(days_to_remove)
       dim_remove<- dim(days_to_remove)[1]
       days_to_remove <- days_to_remove$day[1:dim_remove]
       days_to_remove <- c(days_to_remove)
        if(length(days_to_remove)==0){
          valid_wear_data <- wear_data
        } else{
        valid_wear_data <- wear_data  %>% filter(!day %in% days_to_remove)
        }
        
      # merge choi valid wear time data with ag_data
        full_valid_wear_data <- right_join(ag_data, valid_wear_data, by='Timestamp')
        
   # Old code to do non wear filtering----     
      # # filter out epochs of non-wear    
      #   non_wear_epochs <- which(str_detect(new_choi$wearing, 'nw'))
      #   wear_epochs <- new_choi[-non_wear_epochs,]
         
      # find the day/time of the first and last valid wear day  
        ## first valid wear day
      #   first_valid_wear_days_number <- wear_epochs$days[1]
      #   
      #   ## last valid wear day
      #   valid_wear_days_n <- nrow(valid_wear_days) 
      #   last_valid_wear_days_number <- valid_wear_days$days[valid_wear_days_n]
      # 
      # # filter before first valid wear day number from ag_data  
      #   first_valid_wear_days_number_inds <- which(temp_choi$days==first_valid_wear_days_number)[1]
      #   Timestamp_first_valid_wear_days <- (temp_choi$Timestamp[first_valid_wear_days_number_inds+1]) # have to use plus one minute here because i=1 is weird since they start at midnight, means we lose 1 min of data
      #   
      #   ag_data_first_valid_wear_days_inds<-which(ag_data$Timestamp==Timestamp_first_valid_wear_days)
      #   ag_data_filter_first <- ag_data[-c(1:ag_data_first_valid_wear_days_inds),]
      #   
      # # filter after last valid wear day numb er from ag_data  
      #   last_valid_wear_days_number_inds <- which(temp_choi$days==last_valid_wear_days_number)
      #   very_last_valid_wear_days_number_inds <- tail(last_valid_wear_days_number_inds, n=1)
      #   Timestamp_last_valid_wear_days <- (temp_choi$Timestamp[very_last_valid_wear_days_number_inds])
      #   
      #   ag_data_last_valid_wear_days_inds<-which(ag_data_filter_first$Timestamp==Timestamp_last_valid_wear_days)
      #   ag_data_filtered_n <- nrow(ag_data_filter_first)
      #   ag_data_filtered <- ag_data_filter_first[-c((ag_data_last_valid_wear_days_inds+1):ag_data_filtered_n),] #why did I have to add one here?
      # 
      # # filter epochs of non-wear in between the first and last valid days
      #   non_wear_temp <- temp_choi[non_wear_epochs,]
      #   non_wear_in_range <- non_wear_temp  %>% filter(days>=first_valid_wear_days_number & days<=last_valid_wear_days_number)
      #   
      #   first_valid_day_temp <- temp_choi %>% filter(days==first_valid_wear_days_number) 
      #   first_valid_wear_epoch <- which(first_valid_day_temp$wearing=='w')[1]
      #   first_valid_wear_epoch_Timestamp <- first_valid_day_temp$Timestamp[first_valid_wear_epoch]
      #   
      #   ag_data_first_valid_wear_epoch_inds<-which(ag_data_filtered$Timestamp==first_valid_wear_epoch_Timestamp)
      #   ag_data_filter_first_epoch <- ag_data_filtered[-c(1:ag_data_first_valid_wear_epoch_inds),]
      # # now how do i get all the other non wear out from the middle of the file?
      # # would make it alot easier if choi can return wear marking in 1 sec epochs, is this possible?   
    
        
# Apply ag_epochr to get data in 5 second epoch----
ag_epochr = function(ag_data_1sec,epoch = 60){
          
          rows = nrow(ag_data_1sec)
          new_rows = ceiling(rows/epoch)
          
          ag_data_1sec$index = rep(seq(1, new_rows, by = 1), each = epoch)[1:rows]
          ag_data_1sec$Timestamp = as.character(ag_data_1sec$Timestamp)
          
          first_only_colnames = which(str_detect(colnames(ag_data_1sec), paste('file','stamp', 'Date','Time','wearing','weekday','day',sep = '|')))
          
          epoch_data = as.data.frame(ag_data_1sec[seq(1, nrow(ag_data_1sec), by = epoch), ..first_only_colnames])
          colnames(epoch_data) = colnames(ag_data_1sec)[first_only_colnames]
          
          count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                             Axis1 = sum(Axis1, na.rm = T),
                                                                             Axis2 = sum(Axis2, na.rm = T),
                                                                             Axis3 = sum(Axis3, na.rm = T)) %>% select(-index) %>%
            mutate(VM = sqrt(Axis1^2 + Axis2^2+ Axis3^2))
          
          epoch_data = left_join(epoch_data, count_data)
          
          # If there's step data,  reaggregate to epoch level
          if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
            step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                              Steps = sum(Steps, na.rm = T))%>% select(-index)
            
            epoch_data = left_join(epoch_data, step_data)
          }
          
          # If there's inclinometer data, reaggregate to epoch level
          if(any(str_detect(colnames(ag_data_1sec), 'Inclinometer'))){
            inclinometer_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                      `Inclinometer Off` = sum(`Inclinometer Off`, na.rm = T),
                                                                                      `Inclinometer Standing` = sum(`Inclinometer Standing`, na.rm = T),
                                                                                      `Inclinometer Sitting` = sum(`Inclinometer Sitting`, na.rm = T),
                                                                                      `Inclinometer Lying` = sum(`Inclinometer Lying`, na.rm = T))%>% select(-index)
            
            epoch_data = left_join(epoch_data, inclinometer_data)
          }
          
          # If there's lux data, reaggregate to epoch level
          if(any(str_detect(colnames(ag_data_1sec), 'Lux'))){
            lux_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                             Lux = mean(Lux, na.rm =T)) %>% select(-index)
            
            epoch_data = left_join(epoch_data, lux_data)
          }
          
          # if theres LFE data, repeat all steps for count axes and step data
          if(any(str_detect(colnames(ag_data_1sec), 'LFE'))){
            count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                               Axis1_LFE = sum(Axis1_LFE, na.rm = T),
                                                                               Axis2_LFE = sum(Axis2_LFE, na.rm = T),
                                                                               Axis3_LFE = sum(Axis3_LFE, na.rm = T)) %>%
              mutate(VM_LFE = sqrt(Axis1_LFE^2 + Axis2_LFE^2+ Axis3_LFE^2))%>% select(-index)
            
            
            epoch_data = left_join(epoch_data, count_data)
            
            # If there's step data, also reaggregate to epoch level
            if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
              step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                Steps_LFE = sum(Steps_LFE, na.rm = T))%>% select(-index)
              
              epoch_data = left_join(epoch_data, step_data)
            }
            
            
          }
          
          
          epoch_data$VM = sqrt(epoch_data$Axis1^2 + epoch_data$Axis2^2 + epoch_data$Axis3^2)
          
          epoch_data = epoch_data %>% mutate(Timestamp = ymd_hms(Timestamp))
          
          return(epoch_data)
          
        }        
ag_data_filtered_5sec_epoch <- ag_epochr(full_valid_wear_data, epoch=5)  

# Apply Crouter 2010 method----
crouter2010 = function(acc_data_counts, epoch = 10, expand_1sec = F){
  if(epoch != 10){
    stop("Crouter 2010 two-regression was developed using 10-second epochs. As of now, cutpoint scaling is not supported.")
  }
  
  
  acc_data_new = acc_data_counts
  
  acc_data_new$METs = NA
  acc_data_new$cv1 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 0, .after = 5, .complete = T)
  acc_data_new$cv2 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 1, .after = 4, .complete = T)
  acc_data_new$cv3 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 2, .after = 3, .complete = T)
  acc_data_new$cv4 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 3, .after = 2, .complete = T)
  acc_data_new$cv5 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 4, .after = 1, .complete = T)
  acc_data_new$cv6 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 5, .after = 0, .complete = T)
  
  acc_data_new = acc_data_new %>% dplyr::rowwise() %>% dplyr::mutate(CV = min(c(cv1, cv2, cv3, cv4, cv5, cv6), na.rm = T))
  
  for(i in 1:nrow(acc_data_new)){
    
    if(acc_data_new$Axis1[i] <= 8){
      acc_data_new$METs[i] = 1.0
    } else {
      acc_data_new$METs[i] = ifelse(acc_data_new$CV[i] <= 10,
                                    2.294275*(exp(0.00084679*acc_data_new$Axis1[i])),
                                    0.749395+(0.716431*log(acc_data_new$Axis1[i]))-(0.179874*(log(acc_data_new$Axis1[i])^2))+(0.033173*(log(acc_data_new$Axis1[i])^3)))
    }
  }
  
  # Average MET value of 6 consecutive 10-second epochs within each minute is calculated to obtain average MET value for that minute.... not isn't alway true-- need a new index that looks at the Timestamp 
  acc_data_new$index = rep(seq(1, ceiling((nrow(acc_data_new)/(60/epoch)))), each = (60/epoch))[1:nrow(acc_data_new)]
  
  
  acc_data_minute = acc_data_new %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),weekday = dplyr::first(weekday), day= dplyr::first(day),
                                                                          Crouter_METs = mean(METs, na.rm = T))
  
    acc_data_minute$Crouter2010 = factor(cut(acc_data_minute$Crouter_METs, breaks = c(-Inf, 1.51, 3, 6, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))
  
  if(expand_1sec == T){
    Crouter2010 = data.frame(Timestamp = acc_data_counts$Timestamp,
                             METs = rep(acc_data_minute$METs, each = 60)[1:nrow(acc_data_counts)],
                             Crouter2010 = factor(rep(acc_data_minute$Crouter2010, each = 60), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    
    return(Crouter2010)
    
  } else {
    
    return(acc_data_minute)
    
  }
}
# Applies Crouter 2010 method and returns data in 1 minute epoch        
  # ag_data_filtered_Crouter <- crouter2010(ag_data_filtered_10sec_epoch, epoch=10) # turned off because this is a hip method :(
# Apply Crouter 2015 Vertical Axis (VA) and Vector Magnitude (VM) Methods---- 
crouter2015_ = function(acc_data_counts, epoch = 5, expand_1sec = F){
  if(epoch != 5){
    stop("crouter2015_VA Cutpoint was developed using 5-second epochs. As of now, cutpoint scaling is not supported.")
  }
  
 # acc_data_new = ag_epochr(acc_data_counts, epoch = epoch) turned off because I have this hard coded before in the code
  ag_data_METs_VA <- acc_data_counts %>% mutate(VA_METs=ifelse(Axis1<=35, 1, 
                                                            1.592+(0.0039*Axis1)))
  ag_data_METs_VA_VM <- ag_data_METs_VA %>% mutate(VM_METs=ifelse(VM<=100, 1, 
                                                           1.475+(0.0025*VM)))
  ag_data_METs_VA_VM$index = rep(seq(1, ceiling((nrow(ag_data_METs_VA_VM)/(60/epoch)))), each = (60/epoch))[1:nrow(ag_data_METs_VA_VM)]
   
  acc_data_METs_VA_VM <- ag_data_METs_VA_VM %>% group_by(index) %>% summarize(Timestamp=dplyr::first(Timestamp),
                                                                Date=dplyr::first(Date), Time=dplyr::first(Time), wearing=dplyr::first(wearing),
                                                                weekday=dplyr::first(weekday), day=dplyr::first(day),
                                                                Axis1=sum(Axis1),Axis2=sum(Axis2),Axis3=sum(Axis3), VM=sum(VM),
                                                                Steps=sum(Steps), VA_METs=mean(VA_METs),VM_METs=mean(VM_METs))
  
  
  return(acc_data_METs_VA_VM)
  
}
# Applies Crouter 2015  methods and returns data in 1 min epoch----        
ag_data_filtered_Crouter2015 <- crouter2015_(ag_data_filtered_5sec_epoch, epoch=5)

# Convert Crouter METs/min to kcals/min
      bw <- bw/2.2 # pounds to kgs
      bw_div<- bw/200
      ag_data_filtered_kcals <- ag_data_filtered_Crouter2015 %>% mutate(VA_kCals=(VA_METs*3.5*bw_div),VM_kCals=(VM_METs*3.5*bw_div))
# Add Molnar RMR instead of Crouter 1.0 MET estimate and compute kCals----
  # RMR = 50.2*Weight (kg) +29.6* Height (cm) - 144.5*Age(yr)-550Sex*+594.3

# Create individual summary data----             
save_dir <- '/Users/gregpetruccijr./Desktop/Shook Studies P1 Individual Processed Files'
write.table(ag_data_filtered_kcals,
            file = paste(save_dir, paste(current_participant,'_Processed.csv', sep = ''), sep = '/'),
            sep = ',', col.names = T, row.names = F)

# Fill in P1 master data list----
  final_processed_data <- ag_data_filtered_kcals
  n_row_final_processed_data <- nrow(final_processed_data)
  final_processed_data$Participant <- rep(i, times=n_row_final_processed_data) # add participant to the final df
    P1_master_list[[i]] <- final_processed_data

print(current_participant)
beep(sound=1)      
} # end for loop


# Format the list and save RDS
P1_master_data = do.call(rbind, P1_master_list)
save(P1_master_data, file=paste(save_dir, paste('P1_master_data','_.rds', sep = ''), sep = '/'))
beep(sound=3)


## P2 processing code----

# Add working directory for P2 files----
p2_wd = '/Volumes/HBCD_PE_x64/Shook Studies/P2 1sec epoch' # where the AG csv files are
files <- list.files(p2_wd)
n_files <- length(files)

# Pre-dispose P1 Master data list
P2_master_list = list()

# start for loop to process P1 participant files----
for(i in 1:length(1:n_files)){
  
  file_i= files[i]
  file_i_wd <- paste(p2_wd, file_i, sep='/')
  current_participant <- str_split(file_i, ' ')[[1]][1]
  bw_inds<- which(str_detect(body_weight$Filename, current_participant))
  bw <- body_weight$Bodyweight[bw_inds]
  
  # Read in AG data
  ag_data = read_ag(file_i_wd, ENMO_calibrate = F, device_serial_calibrate = F)
  
  # Use Choi wear time alg to detect non-wear, with default parameters----
  temp_choi <- wearingMarking(ag_data, frame=90, perMinuteCts = 60, TS='Timestamp', cts='Axis1', 
                              getMinuteMarking = T)
  # Expand Choi wear marking output to 1 sec epoch so it matches ag_data
  first_time <- temp_choi$Timestamp[1]
  n_choi<- nrow(temp_choi)
  new_Timestamp <- seq(ymd_hms(first_time),length.out=n_choi*60, by = 'sec')
  new_n <- length(new_Timestamp)
  new_choi <- data.frame(Timestamp=rep(NA, new_n), wearing=rep(NA, new_n),
                         weekday=rep(NA, new_n),day=rep(NA, new_n))
  new_choi$Timestamp <- new_Timestamp
  new_choi$wearing <- rep(temp_choi$wearing, each=60)
  new_choi$weekday <- rep(temp_choi$weekday, each=60)
  new_choi$day <- rep(temp_choi$day, each=60)
  
  wear_data <- new_choi %>% filter(wearing=='w') # exclude non-wear determined by Choi
  # calculate wear seconds on days that do have valid wear.. what about days that don't?
  wear_days <- wear_data %>% group_by(day) %>% summarise(wear_secs=n())
  # find days with >= 360000 secs (10 hours)
  valid_wear_days <- wear_days %>% filter(wear_secs > 36000)
  
  # filter out days that don't meet the valid wear days criteria
  days_to_remove <- subset(wear_days, !(day %in% valid_wear_days$day))[,1]
  days_to_remove <- as.data.frame(days_to_remove)
  dim_remove<- dim(days_to_remove)[1]
  days_to_remove <- days_to_remove$day[1:dim_remove]
  days_to_remove <- c(days_to_remove)
  if(length(days_to_remove)==0){
    valid_wear_data <- wear_data
  } else{
    valid_wear_data <- wear_data  %>% filter(!day %in% days_to_remove)
  }
  
  # merge choi valid wear time data with ag_data
  full_valid_wear_data <- right_join(ag_data, valid_wear_data, by='Timestamp')
  
  # Old code to do non wear filtering----     
  # # filter out epochs of non-wear    
  #   non_wear_epochs <- which(str_detect(new_choi$wearing, 'nw'))
  #   wear_epochs <- new_choi[-non_wear_epochs,]
  
  # find the day/time of the first and last valid wear day  
  ## first valid wear day
  #   first_valid_wear_days_number <- wear_epochs$days[1]
  #   
  #   ## last valid wear day
  #   valid_wear_days_n <- nrow(valid_wear_days) 
  #   last_valid_wear_days_number <- valid_wear_days$days[valid_wear_days_n]
  # 
  # # filter before first valid wear day number from ag_data  
  #   first_valid_wear_days_number_inds <- which(temp_choi$days==first_valid_wear_days_number)[1]
  #   Timestamp_first_valid_wear_days <- (temp_choi$Timestamp[first_valid_wear_days_number_inds+1]) # have to use plus one minute here because i=1 is weird since they start at midnight, means we lose 1 min of data
  #   
  #   ag_data_first_valid_wear_days_inds<-which(ag_data$Timestamp==Timestamp_first_valid_wear_days)
  #   ag_data_filter_first <- ag_data[-c(1:ag_data_first_valid_wear_days_inds),]
  #   
  # # filter after last valid wear day numb er from ag_data  
  #   last_valid_wear_days_number_inds <- which(temp_choi$days==last_valid_wear_days_number)
  #   very_last_valid_wear_days_number_inds <- tail(last_valid_wear_days_number_inds, n=1)
  #   Timestamp_last_valid_wear_days <- (temp_choi$Timestamp[very_last_valid_wear_days_number_inds])
  #   
  #   ag_data_last_valid_wear_days_inds<-which(ag_data_filter_first$Timestamp==Timestamp_last_valid_wear_days)
  #   ag_data_filtered_n <- nrow(ag_data_filter_first)
  #   ag_data_filtered <- ag_data_filter_first[-c((ag_data_last_valid_wear_days_inds+1):ag_data_filtered_n),] #why did I have to add one here?
  # 
  # # filter epochs of non-wear in between the first and last valid days
  #   non_wear_temp <- temp_choi[non_wear_epochs,]
  #   non_wear_in_range <- non_wear_temp  %>% filter(days>=first_valid_wear_days_number & days<=last_valid_wear_days_number)
  #   
  #   first_valid_day_temp <- temp_choi %>% filter(days==first_valid_wear_days_number) 
  #   first_valid_wear_epoch <- which(first_valid_day_temp$wearing=='w')[1]
  #   first_valid_wear_epoch_Timestamp <- first_valid_day_temp$Timestamp[first_valid_wear_epoch]
  #   
  #   ag_data_first_valid_wear_epoch_inds<-which(ag_data_filtered$Timestamp==first_valid_wear_epoch_Timestamp)
  #   ag_data_filter_first_epoch <- ag_data_filtered[-c(1:ag_data_first_valid_wear_epoch_inds),]
  # # now how do i get all the other non wear out from the middle of the file?
  # # would make it alot easier if choi can return wear marking in 1 sec epochs, is this possible?   
  
  
  # Apply ag_epochr to get data in 5 second epoch----
  ag_epochr = function(ag_data_1sec,epoch = 60){
    
    rows = nrow(ag_data_1sec)
    new_rows = ceiling(rows/epoch)
    
    ag_data_1sec$index = rep(seq(1, new_rows, by = 1), each = epoch)[1:rows]
    ag_data_1sec$Timestamp = as.character(ag_data_1sec$Timestamp)
    
    first_only_colnames = which(str_detect(colnames(ag_data_1sec), paste('file','stamp', 'Date','Time','wearing','weekday','day',sep = '|')))
    
    epoch_data = as.data.frame(ag_data_1sec[seq(1, nrow(ag_data_1sec), by = epoch), ..first_only_colnames])
    colnames(epoch_data) = colnames(ag_data_1sec)[first_only_colnames]
    
    count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                       Axis1 = sum(Axis1, na.rm = T),
                                                                       Axis2 = sum(Axis2, na.rm = T),
                                                                       Axis3 = sum(Axis3, na.rm = T)) %>% select(-index) %>%
      mutate(VM = sqrt(Axis1^2 + Axis2^2+ Axis3^2))
    
    epoch_data = left_join(epoch_data, count_data)
    
    # If there's step data,  reaggregate to epoch level
    if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
      step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                        Steps = sum(Steps, na.rm = T))%>% select(-index)
      
      epoch_data = left_join(epoch_data, step_data)
    }
    
    # If there's inclinometer data, reaggregate to epoch level
    if(any(str_detect(colnames(ag_data_1sec), 'Inclinometer'))){
      inclinometer_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                `Inclinometer Off` = sum(`Inclinometer Off`, na.rm = T),
                                                                                `Inclinometer Standing` = sum(`Inclinometer Standing`, na.rm = T),
                                                                                `Inclinometer Sitting` = sum(`Inclinometer Sitting`, na.rm = T),
                                                                                `Inclinometer Lying` = sum(`Inclinometer Lying`, na.rm = T))%>% select(-index)
      
      epoch_data = left_join(epoch_data, inclinometer_data)
    }
    
    # If there's lux data, reaggregate to epoch level
    if(any(str_detect(colnames(ag_data_1sec), 'Lux'))){
      lux_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                       Lux = mean(Lux, na.rm =T)) %>% select(-index)
      
      epoch_data = left_join(epoch_data, lux_data)
    }
    
    # if theres LFE data, repeat all steps for count axes and step data
    if(any(str_detect(colnames(ag_data_1sec), 'LFE'))){
      count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                         Axis1_LFE = sum(Axis1_LFE, na.rm = T),
                                                                         Axis2_LFE = sum(Axis2_LFE, na.rm = T),
                                                                         Axis3_LFE = sum(Axis3_LFE, na.rm = T)) %>%
        mutate(VM_LFE = sqrt(Axis1_LFE^2 + Axis2_LFE^2+ Axis3_LFE^2))%>% select(-index)
      
      
      epoch_data = left_join(epoch_data, count_data)
      
      # If there's step data, also reaggregate to epoch level
      if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
        step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                          Steps_LFE = sum(Steps_LFE, na.rm = T))%>% select(-index)
        
        epoch_data = left_join(epoch_data, step_data)
      }
      
      
    }
    
    
    epoch_data$VM = sqrt(epoch_data$Axis1^2 + epoch_data$Axis2^2 + epoch_data$Axis3^2)
    
    epoch_data = epoch_data %>% mutate(Timestamp = ymd_hms(Timestamp))
    
    return(epoch_data)
    
  }        
  ag_data_filtered_5sec_epoch <- ag_epochr(full_valid_wear_data, epoch=5)  
  
  # Apply Crouter 2010 method----
  crouter2010 = function(acc_data_counts, epoch = 10, expand_1sec = F){
    if(epoch != 10){
      stop("Crouter 2010 two-regression was developed using 10-second epochs. As of now, cutpoint scaling is not supported.")
    }
    
    
    acc_data_new = acc_data_counts
    
    acc_data_new$METs = NA
    acc_data_new$cv1 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 0, .after = 5, .complete = T)
    acc_data_new$cv2 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 1, .after = 4, .complete = T)
    acc_data_new$cv3 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 2, .after = 3, .complete = T)
    acc_data_new$cv4 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 3, .after = 2, .complete = T)
    acc_data_new$cv5 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 4, .after = 1, .complete = T)
    acc_data_new$cv6 = slider::slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 5, .after = 0, .complete = T)
    
    acc_data_new = acc_data_new %>% dplyr::rowwise() %>% dplyr::mutate(CV = min(c(cv1, cv2, cv3, cv4, cv5, cv6), na.rm = T))
    
    for(i in 1:nrow(acc_data_new)){
      
      if(acc_data_new$Axis1[i] <= 8){
        acc_data_new$METs[i] = 1.0
      } else {
        acc_data_new$METs[i] = ifelse(acc_data_new$CV[i] <= 10,
                                      2.294275*(exp(0.00084679*acc_data_new$Axis1[i])),
                                      0.749395+(0.716431*log(acc_data_new$Axis1[i]))-(0.179874*(log(acc_data_new$Axis1[i])^2))+(0.033173*(log(acc_data_new$Axis1[i])^3)))
      }
    }
    
    # Average MET value of 6 consecutive 10-second epochs within each minute is calculated to obtain average MET value for that minute.... not isn't alway true-- need a new index that looks at the Timestamp 
    acc_data_new$index = rep(seq(1, ceiling((nrow(acc_data_new)/(60/epoch)))), each = (60/epoch))[1:nrow(acc_data_new)]
    
    
    acc_data_minute = acc_data_new %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),weekday = dplyr::first(weekday), day= dplyr::first(day),
                                                                            Crouter_METs = mean(METs, na.rm = T))
    
    acc_data_minute$Crouter2010 = factor(cut(acc_data_minute$Crouter_METs, breaks = c(-Inf, 1.51, 3, 6, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))
    
    if(expand_1sec == T){
      Crouter2010 = data.frame(Timestamp = acc_data_counts$Timestamp,
                               METs = rep(acc_data_minute$METs, each = 60)[1:nrow(acc_data_counts)],
                               Crouter2010 = factor(rep(acc_data_minute$Crouter2010, each = 60), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
      
      return(Crouter2010)
      
    } else {
      
      return(acc_data_minute)
      
    }
  }
  # Applies Crouter 2010 method and returns data in 1 minute epoch        
  # ag_data_filtered_Crouter <- crouter2010(ag_data_filtered_10sec_epoch, epoch=10) # turned off because this is a hip method :(
  # Apply Crouter 2015 Vertical Axis (VA) and Vector Magnitude (VM) Methods---- 
  crouter2015_ = function(acc_data_counts, epoch = 5, expand_1sec = F){
    if(epoch != 5){
      stop("crouter2015_VA Cutpoint was developed using 5-second epochs. As of now, cutpoint scaling is not supported.")
    }
    
    # acc_data_new = ag_epochr(acc_data_counts, epoch = epoch) turned off because I have this hard coded before in the code
    ag_data_METs_VA <- acc_data_counts %>% mutate(VA_METs=ifelse(Axis1<=35, 1, 
                                                                 1.592+(0.0039*Axis1)))
    ag_data_METs_VA_VM <- ag_data_METs_VA %>% mutate(VM_METs=ifelse(VM<=100, 1, 
                                                                    1.475+(0.0025*VM)))
    ag_data_METs_VA_VM$index = rep(seq(1, ceiling((nrow(ag_data_METs_VA_VM)/(60/epoch)))), each = (60/epoch))[1:nrow(ag_data_METs_VA_VM)]
    
    acc_data_METs_VA_VM <- ag_data_METs_VA_VM %>% group_by(index) %>% summarize(Timestamp=dplyr::first(Timestamp),
                                                                                Date=dplyr::first(Date), Time=dplyr::first(Time), wearing=dplyr::first(wearing),
                                                                                weekday=dplyr::first(weekday), day=dplyr::first(day),
                                                                                Axis1=sum(Axis1),Axis2=sum(Axis2),Axis3=sum(Axis3), VM=sum(VM),
                                                                                 VA_METs=mean(VA_METs),VM_METs=mean(VM_METs)) #Removed steps from here, for some reason when I made the P2 csv files in ActiLife I didn't include steps...?
    
    
    return(acc_data_METs_VA_VM)
    
  }
  # Applies Crouter 2015  methods and returns data in 1 min epoch----        
  ag_data_filtered_Crouter2015 <- crouter2015_(ag_data_filtered_5sec_epoch, epoch=5)
  
  # Convert Crouter METs/min to kcals/min
  bw <- bw/2.2 # pounds to kgs
  bw_div<- bw/200
  ag_data_filtered_kcals <- ag_data_filtered_Crouter2015 %>% mutate(VA_kCals=(VA_METs*3.5*bw_div),VM_kCals=(VM_METs*3.5*bw_div))
  # Add Molnar RMR instead of Crouter 1.0 MET estimate and compute kCals----
  # RMR = 50.2*Weight (kg) +29.6* Height (cm) - 144.5*Age(yr)-550Sex*+594.3
  
  # Create individual summary data----             
  save_dir <- '/Users/gregpetruccijr./Desktop/Shook Studies P2 Individual Processed Files'
  write.table(ag_data_filtered_kcals,
              file = paste(save_dir, paste(current_participant,'_Processed.csv', sep = ''), sep = '/'),
              sep = ',', col.names = T, row.names = F)
  
  # Fill in P1 master data list----
  final_processed_data <- ag_data_filtered_kcals
  n_row_final_processed_data <- nrow(final_processed_data)
  final_processed_data$Participant <- rep(i, times=n_row_final_processed_data) # add participant to the final df
  P2_master_list[[i]] <- final_processed_data
  
  print(current_participant)
  beep(sound=1)      
} # end for loop


# Format the list and save RDS
P2_master_data = do.call(rbind, P2_master_list)
save(P2_master_data, file=paste(save_dir, paste('P2_master_data','_.rds', sep = ''), sep = '/'))
beep(sound=3)
