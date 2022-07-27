### Flanker Task + Noise Analysis
# Experiment & preprocessed data by Madeleine Dering
# Analysis script by Merle Schuckart

# Steps:

# 0. load packages
# 1. Get data
# 2. test normality of distribution
# 3. Levene Tests + ANOVAs + post-hoc tests
# 4. Plots

#---- 0. load packages ----
# Get packages

# create a list with needed libraries
pkgs <- c("rstatix", # for anova_test function
          "reshape2", # for reshaping dfs
          "doBy", # for summarizing data
          "stringr", # for getting substrings
          "tidyr", # for gather() function
          "nortest", # for KS-Lilliefors tests
          # those are for the rainclouds:
          "ggplot2", "dplyr",  "plyr", "bitops",
          "cowplot", "lavaan", "readr", "smooth",
          "rmarkdown", "Hmisc", "caTools","DescTools")

# load each listed library, check if it's already installed 
# and install if necessary
for (pkg in pkgs){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#---- 1. Get data ----
# 1.1 Set working directory & load preprocessed data
#setwd("/Users/merle/Desktop/Arbeit/HiWi/Experimente/Madeleine Dering")
setwd("/Users/juliankeil/Documents/Arbeit/Kiel/Abschlussarbeiten/Fertig/Dering/Daten_Madelaine/")
load('alldata.Rda')

# 1.2 Get sample
# only include people without ADHS or reduced attention
data <- subset(cleandata, adhs == 0 & attention == 0)

# How many participants are left?
length(unique(data$id)) # should be 12

# 1.3 get the 4 groups
# 0 as False and 1 as True here
incong_no_noise <- subset(data, congruence == 0 & noise == 0)
cong_no_noise <- subset(data, congruence == 1 & noise == 0)
incong_noise <- subset(data, congruence == 0 & noise == 1)
cong_noise <- subset(data, congruence == 1 & noise == 1)

# compute error rates & median RTs

participants <- unique(data$id) # get IDs
summ_data <- data.frame()

# for each participant, compute error rate, median and SD of RTs in all 4 conditions
for (participant in participants){ # loop participants

  # How many correct answers were there compared to the overall number of trials?
  # divide number of correct responses by number of all responses
  error_rate <- c(round(length(subset(incong_no_noise, cResp == 1 & id == participant)$cResp) /
                          length(subset(incong_no_noise, id == participant)$cResp),
                        digits = 3),
                  round(length(subset(cong_no_noise, cResp == 1 & id == participant)$cResp) /
                          length(subset(cong_no_noise, id == participant)$cResp),
                        digits = 3),
                  round(length(subset(incong_noise, cResp == 1 & id == participant)$cResp) /
                          length(subset(incong_noise, id == participant)$cResp),
                        digits = 3),
                  round(length(subset(cong_noise, cResp == 1 & id == participant)$cResp) /
                          length(subset(cong_noise, id == participant)$cResp),
                        digits = 3))
  
  # For computing the median and SD RTs, use only trials with hits:
  median_RT <- c(round(median(subset(incong_no_noise, id == participant & cResp == 1)$RT), digits = 3),
                 round(median(subset(cong_no_noise, id == participant & cResp == 1)$RT), digits = 3),
                 round(median(subset(incong_noise, id == participant & cResp == 1)$RT), digits = 3),
                 round(median(subset(cong_noise, id == participant & cResp == 1)$RT), digits = 3))
  
  sd_RT <- c(round(sd(subset(incong_no_noise, id == participant & cResp == 1)$RT), digits = 3),
             round(sd(subset(cong_no_noise, id == participant & cResp == 1)$RT), digits = 3),
             round(sd(subset(incong_noise, id == participant & cResp == 1)$RT), digits = 3),
             round(sd(subset(cong_noise, id == participant & cResp == 1)$RT), digits = 3))
             
  CV_RT <- c(round( (sd(subset(incong_no_noise, id == participant & cResp == 1)$RT)/(median(subset(incong_no_noise, id == participant & cResp == 1)$RT)))*100, digits = 3),
             round( (sd(subset(cong_no_noise, id == participant & cResp == 1)$RT)/(median(subset(cong_no_noise, id == participant & cResp == 1)$RT)))*100, digits = 3),
             round( (sd(subset(incong_noise, id == participant & cResp == 1)$RT)/(median(subset(incong_noise, id == participant & cResp == 1)$RT)))*100, digits = 3),
             round( (sd(subset(cong_noise, id == participant & cResp == 1)$RT)/(median(subset(cong_noise, id == participant & cResp == 1)$RT)))*100, digits = 3))
  
  # get conditions & id
  congruency <- c("incongruent", "congruent", "incongruent", "congruent")
  noise <- c("no_noise", "no_noise", "noise", "noise")
  id <- c(rep(participant, times = 4))
  
  # combine data to get df:
  summ_data <- as.data.frame(rbind( summ_data, cbind(id, congruency, noise, error_rate, median_RT, sd_RT, CV_RT)))
  
} # END loop participants

# clean up:
rm(participant, id, congruency, noise, error_rate)

#---- 2. test normality of distribution ----

# 2.1 prepare data

# divide df with error rates, median RTs & sd of RTs by congruency & noise conditions
summ_incong_no_noise <- subset(summ_data, congruency == "incongruent" & noise == "no_noise")

summ_cong_no_noise <- subset(summ_data, congruency == "congruent" & noise == "no_noise")

summ_incong_noise <- subset(summ_data, congruency == "incongruent" & noise == "noise")

summ_cong_noise <- subset(summ_data, congruency == "congruent" & noise == "noise")

# put all dfs in a list
groups <- list(summ_incong_no_noise,
               summ_cong_no_noise,
               summ_incong_noise,
               summ_cong_noise)

# get condition names:
names <- c("incongruent task, no noise",
           "congruent task, no noise",
           "incongruent task with noise",
           "congruent task with noise")


# create empty df for results:
lillie_results <- data.frame()

# 2.2 run Lilliefors tests
for (idx in 1:4){ # loop condition names
  
  group <- groups[[idx]]
  
  # First for the error rates:
  
  # run lilliefors test on dependent variable
  lillie <- lillie.test(as.numeric(group$error_rate))
  # save results:
  D <- round(lillie$statistic, digits = 3)
  p <- round(lillie$p.value, digits = 3)
  group_name <- names[idx]
  
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  variable <- "error rates"
  
  # combine and add to results df:
  lillie_results <- as.data.frame(rbind(lillie_results, cbind(variable, group_name, D, p, significance)))
  
  
  # Again for the RTs:
  
  # run lilliefors test on dependent variable
  lillie <- lillie.test(as.numeric(group$median_RT))
  # save results:
  D <- round(lillie$statistic, digits = 3)
  p <- round(lillie$p.value, digits = 3)
  group_name <- names[idx]
  
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  variable <- "median RTs"
  
  # combine and add to results df:
  lillie_results <- as.data.frame(rbind(lillie_results, cbind(variable, group_name, D, p, significance)))
  
  
  # for the SDs:
  
  # run lilliefors test on dependent variable
  lillie <- lillie.test(as.numeric(group$sd_RT))
  # save results:
  D <- round(lillie$statistic, digits = 3)
  p <- round(lillie$p.value, digits = 3)
  group_name <- names[idx]
  
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  variable <- "SD RTs"
  
  # combine and add to results df:
  lillie_results <- as.data.frame(rbind(lillie_results, cbind(variable, group_name, D, p, significance)))
  
  # and one last time for the CVs:
  
  # run lilliefors test on dependent variable
  lillie <- lillie.test(as.numeric(group$CV_RT))
  # save results:
  D <- round(lillie$statistic, digits = 3)
  p <- round(lillie$p.value, digits = 3)
  group_name <- names[idx]
  
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  variable <- "CV RTs"
  
  # combine and add to results df:
  lillie_results <- as.data.frame(rbind(lillie_results, cbind(variable, group_name, D, p, significance)))
  
} # END loop group names

# clean up:
rm(D, p, variable, significance, lillie, idx)

# Have a look at the results:
# View(lillie_results)

# --> Use rank transformations before the ANOVA and nonparametrical
#     post-hoc tests for the error rates & SDs of the RTs. 
#     Use parametrical tests for the median RTs.


#---- 3. ANOVAs + post-hoc tests ----

# 3.1 ANOVAs

# typecast all variables
summ_data$id <- as.factor(summ_data$id)
summ_data$congruency <- as.factor(summ_data$congruency)
summ_data$noise <- as.factor(summ_data$noise)
summ_data$error_rate <- as.numeric(summ_data$error_rate)
summ_data$median_RT <- as.numeric(summ_data$median_RT)
summ_data$sd_RT <- as.numeric(summ_data$sd_RT)
summ_data$CV_RT <- as.numeric(summ_data$CV_RT)

# rank transform error rates & SDs of RTs:
summ_data$error_rates_rank <- rank(summ_data$error_rate)
summ_data$sd_RT_rank <- rank(summ_data$sd_RT)
summ_data$CV_RT_rank <- rank(summ_data$CV_RT)
# --> median RTs don't have to be rank transformed!

# run 2-way repeated measures ANOVAs
# ANOVA for the median RTs
ANOVA_RTS <- anova_test(data = summ_data,
                        formula = as.numeric(median_RT) ~ congruency * noise
                        + Error(id / (congruency * noise)))

ANOVA_RTS_aov <- aov(data = summ_data,
                     formula = as.numeric(median_RT) ~ congruency * noise
                     + Error(id / (congruency * noise)))
EtaSq(ANOVA_RTS_aov,type=1)

# ANOVA for the SD of the RTs
ANOVA_sd_RTs <- anova_test(data = summ_data,
                           formula = as.numeric(sd_RT_rank) ~ congruency * noise
                           + Error(id / (congruency * noise)))

ANOVA_sd_RTs_aov <- aov(data = summ_data,
                        formula = as.numeric(sd_RT_rank) ~ congruency * noise
                        + Error(id / (congruency * noise)))
EtaSq(ANOVA_sd_RTs_aov, type = 1)

# ANOVA for the CV of the RTs
ANOVA_CV_RTs <- anova_test(data = summ_data,
                           formula = as.numeric(CV_RT_rank) ~ congruency * noise
                           + Error(id / (congruency * noise)))

ANOVA_CV_RTs_aov <- aov(data = summ_data,
                        formula = as.numeric(CV_RT) ~ congruency * noise
                        + Error(id / (congruency * noise)))
EtaSq(ANOVA_CV_RTs_aov, type = 1)


# ANOVA for the error rates
ANOVA_error_rates <- anova_test(data = summ_data,
                                formula = as.numeric(error_rates_rank) ~ congruency * noise
                                + Error(id / (congruency * noise)))

ANOVA_error_rates_aov <- aov(data = summ_data,
                             formula = as.numeric(error_rates_rank) ~ congruency * noise
                             + Error(id / (congruency * noise)))

EtaSq(ANOVA_error_rates_aov,type=1)

# Sphericity necessarily holds for dvs with only 2 levels, 
# so no Mauchly's test here & no need to apply a GG correction.


# 3.2 Post-hoc tests:
# use t-tests for median RTs / 
# Wilcoxon signed rank tests for error rates & SDs

gr1_con <- c("congruent", "congruent", "incongruent", "congruent")
gr2_con <- c("incongruent", "incongruent", "incongruent", "congruent")
gr1_noise <- c("no_noise", "noise", "noise", "noise")
gr2_noise <- c("no_noise", "noise", "no_noise", "no_noise")
post_hoc_res <- data.frame()

for (idx in 1:4){ # loop post hoc tests
  
  # for the median RTs:
  ttest_res <-  t.test(x = as.numeric(subset(summ_data, congruency == gr1_con[idx] & noise == gr1_noise[idx])$median_RT),
                       y = as.numeric(subset(summ_data, congruency == gr2_con[idx] & noise == gr2_noise[idx])$median_RT),
                       alternative = "two.sided", paired = T)
  # get p, T and df:
  T_val <- round(ttest_res$statistic, digits = 3)
  df <- ttest_res$parameter
  p <- round(ttest_res$p.value, digits = 3)*4 # *4 for Bonferroni correction
  
  # add asterix if result is significant:
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  # add description of conducted test:
  test_name <- "       t-test"
  comparison <- paste("Median RTs in", gr1_con[idx], gr1_noise[idx], "vs.", gr2_con[idx], gr2_noise[idx], sep = " ")
  
  # placeholders for Wilcoxon  & ANOVA test statistics:
  W = " "
  F_val <- " "
  
  # combine and add to results df:
  post_hoc_res <- as.data.frame(rbind(post_hoc_res, cbind(test_name, comparison, F_val, T_val, W, df, p, significance)))
  
  
  # Same for the error rates:
  wilc_res <-  wilcox.test(x = as.numeric(subset(summ_data, congruency == gr1_con[idx] & noise == gr1_noise[idx])$error_rate),
                           y = as.numeric(subset(summ_data, congruency == gr2_con[idx] & noise == gr2_noise[idx])$error_rate),
                           alternative = "two.sided", paired = T, exact = F)
  
  # get p, T and df:
  W <- round(wilc_res$statistic, digits = 3)
  p <- round(wilc_res$p.value, digits = 3)*4 # *4 for Bonferroni correction
  
  # add asterix if result is significant:
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  # add description of conducted test:
  test_name <- "       Wilcoxon signed-rank test"
  comparison <- paste("Error rates in", gr1_con[idx], gr1_noise[idx], "vs.", gr2_con[idx], gr2_noise[idx], sep = " ")
  
  # placeholder for t-test statistic & df:
  T_val = " "
  df <- " "
  F_val <- " " # for ANOVA results
  
  # combine and add to results df:
  post_hoc_res <- as.data.frame(rbind(post_hoc_res, cbind(test_name, comparison,  F_val, T_val, W, df, p, significance)))
  
  
  # And the same again for the SDs of the RTs:
  wilc_res <-  wilcox.test(x = as.numeric(subset(summ_data, congruency == gr1_con[idx] & noise == gr1_noise[idx])$sd_RT),
                           y = as.numeric(subset(summ_data, congruency == gr2_con[idx] & noise == gr2_noise[idx])$sd_RT),
                           alternative = "two.sided", paired = T, exact = F)
  
  # get p, T and df:
  W <- round(wilc_res$statistic, digits = 3)
  p <- round(wilc_res$p.value, digits = 3)*4 # *4 for Bonferroni correction
  
  # add asterix if result is significant:
  significance <- " "
  if (p <= 0.05){
    significance <- " * "
  }
  
  # add description of conducted test:
  test_name <- "       Wilcoxon signed-rank test"
  comparison <- paste("SDs of RTs in", gr1_con[idx], gr1_noise[idx], "vs.", gr2_con[idx], gr2_noise[idx], sep = " ")
  
  # placeholder for t-test statistic & df:
  T_val = " "
  df <- " "
  F_val <- " " # for ANOVA results
  
  # combine and add to results df:
  post_hoc_res <- as.data.frame(rbind(post_hoc_res, cbind(test_name, comparison,  F_val, T_val, W, df, p, significance)))
  
} # END loop post hoc tests

# order df so it's more neat:
post_hoc_res <-  post_hoc_res[order( post_hoc_res$test_name),]

# Add ANOVA results:

# For median RTs:
test_name <- "ANOVA"
comparison <- "RTs in all noise and congruency conditions"
F_val <- ANOVA_RTS[1,4]
df <- paste("(",ANOVA_RTS[1,2],", ", ANOVA_RTS[1,3], ")", sep = "")
p <- round(ANOVA_RTS[1,5], digits = 3)
significance <- " "
if (p <= 0.05){
  significance <- " * "
}
W <- ""
T_val <- ""

row_RT_ANOVA <- cbind(test_name, comparison, F_val, T_val, W, df, p, significance)

# again for error rates:
test_name <- "ANOVA (rank transformed)"
comparison <- "error rates in all noise and congruency conditions"
F_val <- ANOVA_error_rates[1,4]
df <- paste("(", ANOVA_error_rates[1,2], ", ", ANOVA_error_rates[1,3], ")", sep = "")
p <- round(ANOVA_error_rates[1,5], digits = 3)
significance <- " "
if (p <= 0.05){
  significance <- " * "
}

row_err_ANOVA <- cbind(test_name, comparison, F_val, T_val, W, df, p, significance)

# and one last time for the SDs of the RTs:
test_name <- "ANOVA (rank transformed)"
comparison <- "SDs of RTs in all noise and congruency conditions"
F_val <- ANOVA_sd_RTs[1,4]
df <- paste("(", ANOVA_sd_RTs[1,2], ", ", ANOVA_sd_RTs[1,3], ")", sep = "")
p <- round(ANOVA_sd_RTs[1,5], digits = 3)
significance <- " "
if (p <= 0.05){
  significance <- " * "
}

row_sd_ANOVA <- cbind(test_name, comparison, F_val, T_val, W, df, p, significance)


# put everything together
ANOVA_res <- rbind(row_RT_ANOVA, post_hoc_res[1:4,], 
                   row_err_ANOVA, post_hoc_res[5:8,],
                   row_sd_ANOVA, post_hoc_res[9:12,])


# get mean and sd of the 3 measures in all groups:

# for median RTs:
# mean:
aggregate(summ_data$median_RT,list(Congruency = summ_data$congruency, Noise = summ_data$noise), mean)
# sd:
aggregate(summ_data$median_RT,list(Congruency = summ_data$congruency, Noise = summ_data$noise), sd)

# for error rates:
# mean:
aggregate(summ_data$error_rate,list(Congruency = summ_data$congruency, Noise = summ_data$noise), mean)
# sd:
aggregate(summ_data$error_rate,list(Congruency = summ_data$congruency, Noise = summ_data$noise), sd)

# for SDs of RTs:
# mean:
aggregate(as.numeric(summ_data$sd_RT), list(Congruency = summ_data$congruency, Noise = summ_data$noise), mean)
# sd:
aggregate(as.numeric(summ_data$sd_RT), list(Congruency = summ_data$congruency, Noise = summ_data$noise), sd)


# clean up
rm(row_RT_ANOVA, row_err_ANOVA, post_hoc_res,
   significance, F_val, T_val, df, W, p,
   test_name, comparison, wilc_res, ttest_res)

# Have a look at the results:
#View(ANOVA_res)

# 3.3. Congruency Effect: How much does the performance change from congruent to incongruent?

# 3.3.1. Error Rates:
# Compute Congruency Effect: Cong - Incong
cong_noise_ER <- summ_data$error_rate[summ_data$congruency == 'congruent' & summ_data$noise == 'noise'] - 
  summ_data$error_rate[summ_data$congruency == 'incongruent' & summ_data$noise == 'noise']
cong_nonoise_ER <- summ_data$error_rate[summ_data$congruency == 'congruent' & summ_data$noise == 'no_noise'] - 
  summ_data$error_rate[summ_data$congruency == 'incongruent' & summ_data$noise == 'no_noise']

# Check distribution
lil_cong_ER <- lillie.test(cong_noise_ER)
lil_incong_ER <- lillie.test(cong_nonoise_ER)

# Lillie-Test significant -> Wilcox
wilcox.test(cong_noise_ER, cong_nonoise_ER, alternative = "two.sided", paired = T, exact = F)

# 3.3.2. Median Reaction Times:
# Compute Congruency Effect: Cong - Incong
cong_noise_RT <- summ_data$median_RT[summ_data$congruency == 'congruent' & summ_data$noise == 'noise'] - 
  summ_data$median_RT[summ_data$congruency == 'incongruent' & summ_data$noise == 'noise']
cong_nonoise_RT <- summ_data$median_RT[summ_data$congruency == 'congruent' & summ_data$noise == 'no_noise'] - 
  summ_data$median_RT[summ_data$congruency == 'incongruent' & summ_data$noise == 'no_noise']

# Check distribution
lil_cong_RT <- lillie.test(cong_noise_RT)
lil_incong_RT <- lillie.test(cong_nonoise_RT)

# Lillie-Test not significant -> t-test
t.test(cong_noise_RT,cong_nonoise_RT,alternative = "two.sided", paired = T, exact = F)

# 3.3.2. Standard Deviation Reaction Times:
# Compute Congruency Effect: Cong - Incong
cong_noise_SD <- summ_data$sd_RT_rank[summ_data$congruency == 'congruent' & summ_data$noise == 'noise'] - 
  summ_data$sd_RT_rank[summ_data$congruency == 'incongruent' & summ_data$noise == 'noise']
cong_nonoise_SD <- summ_data$sd_RT_rank[summ_data$congruency == 'congruent' & summ_data$noise == 'no_noise'] - 
  summ_data$sd_RT_rank[summ_data$congruency == 'incongruent' & summ_data$noise == 'no_noise']

# Check distribution
lil_cong_SD <- lillie.test(cong_noise_SD)
lil_incong_SD <- lillie.test(cong_nonoise_SD)

# Lillie-Test not significant -> t-test
t.test(cong_noise_SD,cong_nonoise_SD,alternative = "two.sided", paired = T, exact = F)

# 3.3.2. CV Reaction Times:
# Compute Congruency Effect: Cong - Incong
cong_noise_CV <- summ_data$CV_RT_rank[summ_data$congruency == 'congruent' & summ_data$noise == 'noise'] - 
  summ_data$CV_RT_rank[summ_data$congruency == 'incongruent' & summ_data$noise == 'noise']
cong_nonoise_CV <- summ_data$CV_RT_rank[summ_data$congruency == 'congruent' & summ_data$noise == 'no_noise'] - 
  summ_data$CV_RT_rank[summ_data$congruency == 'incongruent' & summ_data$noise == 'no_noise']

# Check distribution
lil_cong_CV <- lillie.test(cong_noise_CV)
lil_incong_CV <- lillie.test(cong_nonoise_CV)

# Lillie-Test not significant -> t-test
t.test(cong_noise_CV,cong_nonoise_CV,alternative = "two.sided", paired = T, exact = F)
wilcox.test(cong_noise_CV, cong_nonoise_CV, alternative = "two.sided", paired = T, exact = F)



#---- 4. Plots ----

# 4.1 Summarize data:
# typecast RTs & error rates columns
summ_data$median_RT <- as.numeric(summ_data$median_RT)
summ_data$sd_RT <- as.numeric(summ_data$sd_RT)
summ_data$CV_RT <- as.numeric(summ_data$CV_RT)
summ_data$error_rate <- as.numeric(summ_data$error_rate)

# summarize data
summary_median_RTs <- summaryBy(median_RT ~ noise + congruency,
                                data = summ_data,
                                FUN = function(x) {
                                  c(M = round(mean(x, na.rm = T), digits = 3),
                                    SD = round(sd(x, na.rm = T), digits = 3),
                                    N = length(x))
                                })

summary_sd_RTs <- summaryBy(sd_RT ~ noise + congruency,
                                data = summ_data,
                                FUN = function(x) {
                                  c(M = round(mean(x, na.rm = T), digits = 3),
                                    SD = round(sd(x, na.rm = T), digits = 3),
                                    N = length(x))
                                })
                                
summary_CV_RTs <- summaryBy(CV_RT ~ noise + congruency,
                                data = summ_data,
                                FUN = function(x) {
                                  c(M = round(mean(x, na.rm = T), digits = 3),
                                    SD = round(sd(x, na.rm = T), digits = 3),
                                    N = length(x))
                                })

summary_error_rates <- summaryBy(error_rate ~ noise + congruency,
                                 data = summ_data,
                                 FUN = function(x) {
                                   c(M = round(mean(x, na.rm = T), digits = 3),
                                     SD = round(sd(x, na.rm = T), digits = 3),
                                     N = length(x))
                                 })



#---- 4.2 Create Raincloud Plots ----

# 4.2.1 Plot settings ----
# Set up flat violin function in ggplot ----
# This part is from Tom Rhys Marshall's Github page:
# https://github.com/RainCloudPlots/RainCloudPlots/blob/master/tutorial_R/R_rainclouds.R

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(
                ymin = min(y),
                ymax = max(y),
                xmin = x,
                xmax = x + width / 2
              )
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)
            )
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(
              plyr::arrange(transform(data, x = xminv), y),
              plyr::arrange(transform(data, x = xmaxv), -y)
            )
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1, ])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(
            weight = 1, colour = "grey20", fill = "white", size = 0.5,
            alpha = NA, linetype = "solid"
          ),
          
          required_aes = c("x", "y")
  )



#---- 4.2.2 Change structure of df ----

# rename values in noise column & merge congruency & noise:
summ_data$noise <- as.character(summ_data$noise)
summ_data$noise[summ_data$noise == "no_noise"] <- "no noise"
summ_data$noise <- as.factor(summ_data$noise)
summ_data$condition <- with(summ_data, paste0(congruency, " task, ", noise))

#---- 4.2.3 Get Plots ----

# Plot for the median RTs: ----

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#969cc8", "#d96c6c", "#d96c6c")

# add a formatting style for the axis labels (I need line breaks)
addline_format <- function(x){
  # add linebreak instead of / in the labels
  gsub('/','\n',x)
}


# Create raincloud plot for the RTs:
plot_RTs <- ggplot(summ_data, aes(x = condition,
                                  y = median_RT,
                                  fill = condition)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .7, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = condition, y = median_RT),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("reaction time in ms") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 450,
                                  by = 50),
                     limits = c(0, 470)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.03, .5),
                   # add labels, use / where you need a line break:
                   labels=addline_format(c("congruent,/ no noise",
                                           "congruent,/ white noise",
                                           "incongruent,/ no noise",
                                           "incongruent,/ white noise"))) +
  
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Median reaction times")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_RTs
pdf(file = "Exp1_MedianRTs.pdf", width = 10, height = 5)
plot_RTs
dev.off()


# Plot for the CV SD RTs: ----

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#969cc8", "#d96c6c", "#d96c6c")

# Create raincloud plot for the SD RTs:
plot_SDRTs <- ggplot(summ_data, aes(x = condition,
                                    y = CV_RT,
                                    fill = condition)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .7, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = condition, y = CV_RT),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("normalized reaction time standard deviation in ms") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 40,
                                  by = 5),
                     limits = c(0, 50)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.03, .5),
                 # add labels, use / where you need a line break:
                   labels=addline_format(c("congruent,/ no noise",
                                           "congruent,/ white noise",
                                           "incongruent,/ no noise",
                                           "incongruent,/ white noise"))) +
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend:
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Normalized standard deviations of reaction times")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_SDRTs
pdf(file = "Exp1_SDRTs.pdf", width = 10, height = 5)
plot_SDRTs
dev.off()


# Plot for the error rates: ----

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#969cc8", "#d96c6c", "#d96c6c")

plot_error_rates <- ggplot(summ_data, aes(x = condition,
                                          y = error_rate,
                                          fill = condition)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .8, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = condition, y = error_rate),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("rate of correct answers") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 1.1,
                                  by = .1),
                     limits = c(0, 1.1)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.02, .1),
                   # add labels, use / where you need a line break:
                   labels=addline_format(c("congruent,/ no noise",
                                           "congruent,/ white noise",
                                           "incongruent,/ no noise",
                                           "incongruent,/ white noise"))) +
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Performance in the Flanker task")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_error_rates
pdf(file = "Exp1_ErrorRates.pdf", width = 10, height = 5)
plot_error_rates
dev.off()


# Plots for congruency effects ----
# For the congruency effects - plot error rates from cong_noise_ER & cong_nonoise_ER

# prepare data for plotting:
noise <- c(rep("no noise", times = length(cong_nonoise_ER)), 
           rep("noise", times = length(cong_nonoise_ER)))
error_rate <-  c(cong_nonoise_ER, cong_noise_ER)
cong_err_df <- as.data.frame(cbind(error_rate, noise))
cong_err_df$error_rate <- as.numeric(cong_err_df$error_rate)

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#d96c6c")

plot_cong_ER <- ggplot(cong_err_df, aes(x = noise,
                                        y = error_rate,
                                        fill = noise)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .8, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = noise, y = error_rate),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("change of correct answers") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 0.9,
                                  by = 0.2),
                     limits = c(0, 0.9)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.02, .2)) +
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Performance Congruent - Incongruent")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_cong_ER
pdf(file = "Exp1_ErrorRatesCE.pdf", width = 10, height = 2.5)
plot_cong_ER
dev.off()


# For the congruency effects - plot RTs from cong_noise_RT & cong_nonoise_RT

# prepare data for plotting:
noise <- c(rep("no noise", times = length(cong_nonoise_RT)), 
           rep("noise", times = length(cong_nonoise_RT)))

RT <-  c(cong_nonoise_RT, cong_noise_RT)
cong_RT_df <- as.data.frame(cbind(RT, noise))
cong_RT_df$RT <- as.numeric(cong_RT_df$RT)

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#d96c6c")

plot_cong_RT <- ggplot(cong_RT_df, aes(x = noise,
                                       y = RT,
                                       fill = noise)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .8, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = noise, y = RT),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("change of reaction time in ms") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = -140,
                                  to = 0,
                                  by = 20),
                     limits = c(-140, 0)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.2, .2)) +
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Median Reaction Time Congruent - Incongruent")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_cong_RT
pdf(file = "Exp1_MedianRTsCE.pdf", width = 10, height = 2.5)
plot_cong_RT
dev.off()

# For the congruency effects - plot CV RTs from cong_noise_CV & cong_nonoise_CV

# prepare data for plotting:
noise <- c(rep("no noise", times = length(cong_nonoise_CV)), 
           rep("noise", times = length(cong_nonoise_CV)))

CV <-  c(cong_nonoise_CV, cong_noise_CV)
cong_CV_df <- as.data.frame(cbind(CV, noise))
cong_CV_df$CV <- as.numeric(cong_CV_df$CV)

# set custom colors (set as many colors as clouds):
custom_col <- c("#969cc8","#d96c6c")

plot_cong_CV <- ggplot(cong_CV_df, aes(x = noise,
                                       y = CV,
                                       fill = noise)) +
  # add violins (aka the cloud):
  geom_flat_violin(position = position_nudge(x = .15, y = 0), # push violins a little off center
                   adjust = .8, # smoothing of the density curve
                   alpha = .5, # opacity of the density curve
                   trim = T) + # cut off violins at the ends
  # add scatter points (aka the rain):
  geom_point(position = position_jitter(width = .1, height = 0), # jitter, so points don't overlap
             shape = 21, # just dots as shape
             size = 1) + # set point size here
  # add boxplot:
  geom_boxplot(aes(x = noise, y = CV),
               position = position_nudge(x = .15, y = 0), # move it up a little
               outlier.shape = NA, # don't mark outliers
               alpha = 0.3, # set opacity of boxplot fill color
               width = .1, # set width of box
               colour = "BLACK") +
  # add axes titles:
  ylab("change of normalized reaction time standard deviation in ms") +
  xlab("condition") +
  # change scaling & ticks of y-axis (actually the x-axis
  # because we turn the plot around):
  scale_y_continuous(breaks = seq(from = -40,
                                  to = 40,
                                  by = 10),
                     limits = c(-45, 45)) +
  # change padding under the first cloud so the
  # points don't touch the axis:
  scale_x_discrete(expand = c(0.2, .2)) +
  # flip the violins so they're horizontal:
  coord_flip() +
  # use cowplot theme:
  theme_cowplot() +
  # don't use legend
  guides(fill = "none", colour = "none") +
  # set colors with color palette:
  #scale_colour_brewer(palette = "Dark2") + # border color
  #scale_fill_brewer(palette = "Dark2") + # fill color
  # set colors manually:
  scale_color_manual(values = custom_col) +
  scale_fill_manual(values = custom_col) +
  # set title
  ggtitle("Normalized Reaction Time Standard Deviance Congruent - Incongruent")

# you can also save the plot, just fill in the missing details:
#ggsave("set_path_here", width = w, height = h)

# Look at the plot:
#plot_cong_RT
pdf(file = "Exp1_CVRTsCE.pdf", width = 10, height = 2.5)
plot_cong_CV
dev.off()

