### SIFI Noise Process data
# Steps:
# 1. Load data
# 2. Exclude based on demographics
# 3. Exclude based on checks
# 4. Exclude based on control trials

## 1. Get Data
# 1.1 Set Working Directors
setwd('/Users/juliankeil/Documents/Arbeit/Kiel/Abschlussarbeiten/Fertig/Zimmermann/SIFINoise_Git/01_Data/01_ProcessedData/')

load('VP_data.rda')

## 2. Exclude based on demographics
dem_sel <- c("id_drugs","id_eyesight","id_hearing","id_neuro")
dem_catch <- which(VP_demographics[dem_sel,] == "yes", arr.ind = TRUE)

VP_data[dem_catch[2]] <- NULL

## 3. Exclude based on checks
# 'No'-Responses
exp_sel <- c("exp_noise", "exp_visual", "exp_auditory")
exp_catch <- which(VP_checks[exp_sel,] == "no", arr.ind = TRUE)

VP_data[exp_catch[2]] <- NULL

# 'always'-Responses
exp_sel <- c("exp_delayfrequency")
exp_catch <- which(VP_checks[exp_sel,] == "always", arr.ind = TRUE)

VP_data[exp_catch] <- NULL

# 'zero'-Responses
exp_sel <- c("exp_noiseBlocks", "exp_noiseWithinblock")
exp_catch <- which(VP_checks[exp_sel,] == "zero", arr.ind = TRUE)

VP_data[exp_catch[2]] <- NULL

## 4. Exclude based on poor performance in control blocks -> more than 1/3 errors
excl_vec_cb <- NULL
comp <- NULL
for(i in 1:length(VP_data)) {
	
	blocks <- which(VP_data[i][[1]][[1]]$Block_noiselevel == 'Kontrollblock loop +noise+')
	stim <- as.numeric(substring(VP_data[i][[1]][[1]]$Label_noiselevel[blocks],2,2))
	resp <- as.numeric(VP_data[i][[1]][[1]]$Response_noiselevel[blocks])-2 # somehow adds 2
	comp[i] <- sum(stim != resp)
	
		if (comp[i]/(length(stim)) > 0.34) {
			excl_vec_cb <- c(excl_vec_cb,i)
		} # if loop
		
	} # for loop
	
VP_data[excl_vec_cb] <- NULL

## Exclude based on poor performance in A0V2 -> more than 1/10 errors
excl_vec_a0v2 <- NULL
comp <- NULL
for(i in 1:length(VP_data)) {
	
	blocks <- which(VP_data[i][[1]][[1]]$Block_noiselevel != 'Kontrollblock loop +noise+')
	trials <- which(VP_data[i][[1]][[1]]$Label_noiselevel[blocks] == "A0V2")
	stim <- as.numeric(substring(VP_data[i][[1]][[1]]$Label_noiselevel[blocks][trials],4,4))
	resp <- as.numeric(VP_data[i][[1]][[1]]$Response_noiselevel[blocks][trials])-2 # somehow adds 2
	comp[i] <- sum(stim != resp)
	
		if (comp[i]/(length(stim)) > 0.10) {
			excl_vec_a0v2 <- c(excl_vec_a0v2,i)
		} # if loop
		
	} # for loop
	
VP_data[excl_vec_a0v2] <- NULL
