### SIFI Noise Process data
# Steps:
# 1. Load data
# 2. Exclude based on demographics
# 3. Exclude based on checks
# 4. Exclude based on control trials

## 0. Get packages
library(tidyr)

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

## 4. Exclusion based on performance
# First remove non-responders -> missed more than 1/3 of trials
excl_vec_nr <- NULL
nr <- NULL
for (i in 1:length(VP_data)) {
	nr[i] <- 0
	nr[i] <- sum(VP_data[i][[1]][[1]]$Response_noiselevel == "")
	if ( nr[i] > length(VP_data[i][[1]][[1]]$Response_noiselevel)/3) {
		excl_vec_nr <- c(excl_vec_nr,i)
	} # If loop
	
	} # For loop
	
VP_data[excl_vec_nr] <- NULL

# Then remove bad trials
# No response
# Response too early (<100ms)
# Response too late (>1700ms)
for(i in 1:length(VP_data)) {
	
	# No response
	VP_data[i][[1]][[1]] <- VP_data[i][[1]][[1]][VP_data[i][[1]][[1]]$Response_noiselevel != "",]
	# Fast response
	VP_data[i][[1]][[1]] <- VP_data[i][[1]][[1]][VP_data[i][[1]][[1]]$Value_noiselevel >= 100,]
	# Slow response
	VP_data[i][[1]][[1]] <- VP_data[i][[1]][[1]][VP_data[i][[1]][[1]]$Value_noiselevel <= 1700,]
	
	} # for loop

# 4.1 Exclude based on poor performance in control blocks -> more than 1/3 errors
excl_vec_cb <- NULL
comp <- NULL
for(i in 1:length(VP_data)) {
	
	blocks <- which(VP_data[i][[1]][[1]]$Block_noiselevel == 'Kontrollblock loop +noise+')
	stim <- as.numeric(substring(VP_data[i][[1]][[1]]$Label_noiselevel[blocks],2,2))
	resp <- as.numeric(as.character.factor(VP_data[i][[1]][[1]]$Response_noiselevel[blocks]))
	comp[i] <- sum(stim != resp)
	
		if (comp[i]/(length(stim)) > 0.34) {
			excl_vec_cb <- c(excl_vec_cb,i)
		} # if loop
		
	} # for loop
	
VP_data[excl_vec_cb] <- NULL

# 4.2 Exclude based on poor performance in A0V2 -> more than 1/10 errors
excl_vec_a0v2 <- NULL
comp <- NULL
for(i in 1:length(VP_data)) {
	
	blocks <- which(VP_data[i][[1]][[1]]$Block_noiselevel != 'Kontrollblock loop +noise+')
	trials <- which(VP_data[i][[1]][[1]]$Label_noiselevel[blocks] == "A0V2")
	stim <- as.numeric(substring(VP_data[i][[1]][[1]]$Label_noiselevel[blocks][trials],4,4))
	resp <- as.numeric(as.character.factor(VP_data[i][[1]][[1]]$Response_noiselevel[blocks][trials]))
	comp[i] <- sum(stim != resp)
	
		if (comp[i]/(length(stim)) > 0.10) {
			excl_vec_a0v2 <- c(excl_vec_a0v2,i)
		} # if loop
		
	} # for loop
	
VP_data[excl_vec_a0v2] <- NULL

## 5. Compute Response Rates
alldat <- NULL
for(i in 1:length(VP_data)) {
	# collect the data
	tmpdat <- droplevels(VP_data[i][[1]][[1]],"")
	
	# build empty matrices
	r0 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	r1 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	r2 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	
	rt0 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	rt1 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	rt2 <- matrix(,nrow=length(levels(tmpdat$Block_noiselevel)), ncol=length(levels(tmpdat$Label_noiselevel)))
	
	# loop conditions and responses
	b <- 1 # start at 1
	for(bl in levels(tmpdat$Block_noiselevel)) {
		c <- 1 # start at 1
		for(cond in levels(tmpdat$Label_noiselevel)) {
			# collect the number of responses
			r0[b,c] <- sum(tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 0)
			r1[b,c] <- sum(tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 1)
			r2[b,c] <- sum(tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 2)
			# collect the response times
			rt0[b,c] <- median(tmpdat$Value_noiselevel[tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 0])
			rt1[b,c] <- median(tmpdat$Value_noiselevel[tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 1])
			rt2[b,c] <- median(tmpdat$Value_noiselevel[tmpdat$Block_noiselevel == bl & tmpdat$Label_noiselevel == cond & tmpdat$Response_noiselevel == 2])
			c <- c+1
		} 
		b <- b+1
	}
	# Name the matrices
	rownames(r0) <- levels(tmpdat$Block_noiselevel)
	rownames(r1) <- levels(tmpdat$Block_noiselevel)
	rownames(r2) <- levels(tmpdat$Block_noiselevel)
	
	rownames(rt0) <- levels(tmpdat$Block_noiselevel)
	rownames(rt1) <- levels(tmpdat$Block_noiselevel)
	rownames(rt2) <- levels(tmpdat$Block_noiselevel)
	
	colnames(r0) <- levels(tmpdat$Label_noiselevel)
	colnames(r1) <- levels(tmpdat$Label_noiselevel)
	colnames(r2) <- levels(tmpdat$Label_noiselevel)
	
	colnames(rt0) <- levels(tmpdat$Label_noiselevel)
	colnames(rt1) <- levels(tmpdat$Label_noiselevel)
	colnames(rt2) <- levels(tmpdat$Label_noiselevel)
	
	alldat <- rbind(alldat,r0)
	
} # for loop
## 6. Stats

## 7. Plots