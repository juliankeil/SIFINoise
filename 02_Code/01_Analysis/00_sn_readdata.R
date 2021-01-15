### SIFI Noise Read data
# Steps:
# 1. Get Data and File lists
# 2. Loop through the data
# 3. Define data Read in different parts of the experiment
# 3.1. Demographics
# 3.2. Sanity Checks
# 3.3. Experiment Data
# 3.4. Control Block
# Combine data

## 1. Get Data and File lists
# 1.1 Set Working Directors
setwd("/Users/juliankeil/Documents/Arbeit/Kiel/Abschlussarbeiten/Fertig/Zimmermann/SIFINoise_Git/01_Data/00_RawData")

# 1.2 Get the list of files in the Directory
file_list <- list.files(pattern='.csv')

## 2. Loop through files to read in data
VP_data <- list() # Create placeholder for the main list
VP_demographics <- matrix(data=NA,nrow = 7, ncol = length(file_list))
VP_checks <- matrix(data=NA,nrow = 10, ncol = length(file_list))

for (i in 1:length(file_list)) {

  # 3. Get data for one participant
  VP <- data.frame(read.csv(file_list[i]), stringsAsFactors = F)

  # Change Event-IDs to Numeric
  VP$sender_id <- as.character(VP$sender_id) # Force as character to select elements
  VP$sender_id <- substr(VP$sender_id,1,4) # Select the first four elements as IDs
  VP$sender_id <- as.numeric(gsub("_", "", VP$sender_id)) #Changed sub to gsub, Force as Numeric
  
  # 3.1. Extract the demographic information
  # Define the Info
  dem_cols <- c('id_birth',
  				'id_drugs',
  				'id_eyesight',
  				'id_gender',
  				'id_hearing',
  				'id_neuro',
  				'id_participant')
  dem_row <- NULL
  for (j in 1:length(VP)) {
  	if (any(names(VP[j]) == dem_cols)) {
  		dem_sel <- which(names(VP[j]) == dem_cols)
  		dem_row[dem_sel] <- levels(VP[2,j])[2]
  		} # close the if loop
  } # close the for loop
  VP_demographics[,i] <- dem_row # store the demographic information
  
  
  # 3.2. Extract the sanity check information
  # Define the Info
  exp_cols <- c('exp_ImpairementVisual',
  				'exp_ImparementAuditory',
  				'exp_auditory',
  				'exp_delay',
  				'exp_delayfrequency',
  				'exp_delayintensity',
  				'exp_noise',
  				'exp_noiseBlocks',
  				'exp_noiseWithinblock',
  				'exp_visual')
  exp_row <- NULL
  for (j in 1:length(VP)) {
  	if (any(names(VP[j]) == exp_cols)) {
  		exp_sel <- which(names(VP[j]) == exp_cols)
  		exp_row[exp_sel] <- levels(VP[2,j])[2]
  		} # close the if loop
  } # close the for loop
  VP_checks[,i] <- exp_row # store the sanity check info
  
  # 3.3. Get the Data from the Main Experiment
  # Define the Info
  data_cols <- c('main baseline +noise+',
  				 'main +noise+ condition1',
  				 'main +noise+ condition2',
  				 'main +noise+ condition3',
  				 'main +noise+ condition4',
  				 'main +noise+ condition5',
  				 'main +noise+ condition6',
  				 'main +noise+ condition7',
  				 'Kontrollblock loop +noise+')
  # 3.3.1. Find the position of the blocks in the data				 
  pos <- list()
  for (k in 1:length(data_cols)) {	
    pos[k] <- which(VP$sender == data_cols[k])
  } # close for loop
  
  # 3.3.2. Find the event ID for the blocks
  # 2.1.1.3 Find the Event ID Indicating the Block Onset
  blocks <- list()
  for (k in 1:length(pos)) {
    blocks[k] <- as.numeric(VP[as.numeric(pos[[k]]-1), "sender_id"], as.numeric) 
  } # close for loop
  
  # 3.3.3. Loop through the data to find the single blocks
  SIFI_data <- data.frame()
  for (l in 1:length(blocks)) {
    tmppos <- which((VP$sender == "SIFI response") &                               
                      (VP$sender_id == blocks[l]))
    
    Label_noiselevel <- as.factor(VP[tmppos, "SIFIlabel"])
    Value_noiselevel <- as.numeric(VP[tmppos, "duration"])
    Response_noiselevel <- as.factor(VP[tmppos, "response"])
    Block_noiselevel <- as.factor(data_cols[l])
    
    tmp_frame <-  data.frame("Label_noiselevel" = Label_noiselevel,  
                             "Value_noiselevel" = Value_noiselevel,
                             "Response_noiselevel" = Response_noiselevel,
                             "Block_noiselevel" = Block_noiselevel,
                             stringsAsFactors = F)
    
    # And Add to big list for one participant
    SIFI_data <- rbind(SIFI_data, tmp_frame)
  } # close for loop of experiment data
  
  # store the SIFI data 
  tmplist <- list(SIFI_data)
  VP_data[i] <- list(tmplist)
  names(VP_data)[i] <- paste0(dem_row[7])
  
} # Close big loop around all participants

# Final clean-up: name the demographics         
rownames(VP_demographics) <- dem_cols # Name the demographic rows
rownames(VP_checks) <- exp_cols # name the sanity check rows

## 3. Save data IAPS 
# Write data back to disk in RDA format.
save(VP_data, VP_demographics, VP_checks , file='VP_data.rda')