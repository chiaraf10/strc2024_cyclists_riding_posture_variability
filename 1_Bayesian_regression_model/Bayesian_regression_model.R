# If packages not installed, run the commented out codes
# install.packages("brms")
# install.packages("bayesplot")
# install.packages("tidybayes")
# install.packages("dplyr")
library(brms)
library(bayesplot)
library(tidybayes)
library(dplyr)
orig_data = read.csv("cyclist_dataset_annotations_STRC24_updated.csv")
orig_data$torso_angle = round(orig_data$torso_angle)
orig_data$arm_angle = round(orig_data$arm_angle)
data = data.frame(matrix(NA, nrow = length(orig_data$object_id), ncol = 6))
colnames(data) = c("arm_angle","torso_angle","helmet","gender","bike","ebike")
for (i in 1:length(orig_data$object_id)){
  data$torso_angle[i] = orig_data$torso_angle[i]
  data$arm_angle[i] = orig_data$arm_angle[i]
  if(orig_data$helmet[i] == 1){
    data$helmet[i] = "Use helmet"
  }else{
    data$helmet[i] = "no helmet"
  }
  
  if(orig_data$Female[i] == 1){
    data$gender[i] = "Female"
  }else{
    data$gender[i] = "Male"
  }
  
  if(orig_data$classic[i] ==1){
    data$bike[i] = "classic"
  } else if(orig_data$hybrid[i] ==1){
    data$bike[i] ="hybrid"
  } else if (orig_data$race[i] ==1){
    data$bike[i] ="race"
  } 
  
  if(orig_data$ebike[i] == 1){
    data$ebike[i] = "ebike"
  }else{
    data$ebike[i] = "traditional_bike"
  }
}
# Perform chi-square test for independence
data$helmet = as.factor(data$helmet)
data$gender = as.factor(data$gender)
data$bike = as.factor(data$bike)
data$ebike = as.factor(data$ebike)
data$ebike <- relevel(data$ebike, ref = "traditional_bike")
data$gender <- relevel(data$gender, ref = "Male")
multivariate_formula_arm <- bf(arm_angle ~ gender + bike + ebike + bike*gender)
multivariate_formula_torso <- bf(torso_angle ~ gender + bike + ebike + bike*gender)
# Fit the model
fit_arm <- brm(
  formula = multivariate_formula_arm,
  data = data,
  family = gaussian(), # Assuming a normal distribution of errors
  chains = 4,
  iter = 5000,
  warmup = 2000,
  cores = 4,
  seed = 123
)
# extract the posterior samples
arm_posterior_samples <- as_draws_df(fit_arm)
# Convert the samples to a data frame
arm_posterior_df <- as.data.frame(arm_posterior_samples)
arm_posterior_df <- arm_posterior_df %>%
  rowwise() %>%
  mutate(baseline_male_classic_traditional = b_Intercept,
        female_classic_traditional = b_Intercept + b_genderFemale,
        male_hybrid_traditional = b_Intercept + b_bikehybrid,
        male_race_traditional = b_Intercept + b_bikerace,
        male_classic_ebike = b_Intercept + b_ebikeebike,
        female_hybrid_traditional = b_Intercept + b_genderFemale+ b_bikehybrid + `b_genderFemale:bikehybrid`,
        female_race_traditional = b_Intercept + b_genderFemale+ b_bikerace + `b_genderFemale:bikerace`,
        female_classic_ebike = b_Intercept + b_genderFemale+ b_ebikeebike)%>%
  ungroup()
# Write the data frame to a CSV file
write.csv(arm_posterior_df, file = "arm_posterior_prediction.csv", row.names = FALSE)
# Check convergence
plot(fit_arm,ask = FALSE)
fit_torso <- brm(
  formula = multivariate_formula_torso,
  data = data,
  family = gaussian(), # Assuming a normal distribution of errors
  chains = 4,
  iter = 5000,
  warmup = 2000,
  cores = 4,
  seed = 123
)
# extract the posterior samples
torso_posterior_samples <- as_draws_df(fit_torso)
# Convert the samples to a data frame
torso_posterior_df <- as.data.frame(torso_posterior_samples)
torso_posterior_df <- torso_posterior_df %>%
  rowwise() %>%
  mutate(baseline_male_classic_traditional = b_Intercept,
         female_classic_traditional = b_Intercept + b_genderFemale,
         male_hybrid_traditional = b_Intercept + b_bikehybrid,
         male_race_traditional = b_Intercept + b_bikerace,
         male_classic_ebike = b_Intercept + b_ebikeebike,
         female_hybrid_traditional = b_Intercept + b_genderFemale+ b_bikehybrid + `b_genderFemale:bikehybrid`,
         female_race_traditional = b_Intercept + b_genderFemale+ b_bikerace + `b_genderFemale:bikerace`,
         female_classic_ebike = b_Intercept + b_genderFemale+ b_ebikeebike)%>%
  ungroup()
# Write the data frame to a CSV file
write.csv(torso_posterior_df, file = "torso_posterior_prediction.csv", row.names = FALSE)
# Check convergence
plot(fit_torso,ask = FALSE)
