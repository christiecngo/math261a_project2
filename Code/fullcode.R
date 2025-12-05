setwd("C:\\Users\\chris\\Documents\\MAT 261A\\Project 2\\Data")

df = read.csv("Trans Athletes DATA.csv")
df = subset(df, (margin_of_victory != 1) & (margin_of_victory != 0)) # removing uncontested elections
rp = subset(df, republican == 1)

dm = subset(df, democrat == 1)

hist(rp$margin_of_victory)
hist(dm$margin_of_victory)

# both are right skewed

plot(dm$pctnonwhite, sqrt(dm$margin_of_victory))

library(ggplot2)
library(reshape2)

# select numeric columns 

## ERROR with correlation matrix: the standard deviation is zero

#num_cols <- sapply(dm, is.numeric)
#dm_num <- dm[, num_cols]

#num_cols <- sapply(rp, is.numeric)
#rp_num <- rp[, num_cols]

## Compute correlation matrix
#cor_mat_dm <- cor(dm_num, use = "pairwise.complete.obs")
#cor_mat_rp <- cor(rp_num, use = "pairwise.complete.obs")


# Transformation into logit because of clear horizontal line at proportion = 1 

library(car)


# skip transformation of margin of victory (can approach inf)
my_response = "margin_of_victory"
logit_df = df
for (col in names(df)){
  # skip binary values of variables
  if (col == my_response){
    print(col)
    logit_df[[col]] <- df[[col]]
    next
  }
  if (all(df[[col]] %in% c(0, 1), na.rm = TRUE)) {
    logit_df[[col]] <- df[[col]]
    next
  }
  
  if (min(df[[col]],na.rm=TRUE) >= 0 && max(df[[col]],na.rm=TRUE)<=1){
    logit_df[[col]] = car::logit(df[[col]], adjust = 0.01)
  }
  else{
    logit_df[[col]] = df[[col]]
  }
}

# remove party binary variables after subsetting, also dropping 1 dummy variable for each category adding to 1
rp = subset(df, republican == 1)
rp$democrat = NULL
rp$republican = NULL
rp$urban = NULL
rp$under25 = NULL
rp$age20_44 = NULL
rp$c_votes = NULL # reveals election results
rp$d_votes_floser = NULL # reveals election results
rp$independent = NULL
rp$votemargin = NULL # reveals the margin of victory
rp$pctnonwhite = NULL
rp$pctnevermarried = NULL # makes default current marriage status (combine with missing divorce)
rp$mmd = NULL # shows median marginal district 
rp$thrdparty = NULL # mostly missing
rp$legis_ideal = NULL # mostly missing & not demographic

# not relevant to regression
rp$vote_yes = NULL
rp$vote_no = NULL
rp$election_year = NULL
rp$bill_number = NULL
rp$Bill_Prefix = NULL
rp$VoteDate = NULL
rp$lawmaker_name = NULL
rp$state_abbrev = NULL
rp$statename = NULL
rp$Year = NULL
rp$term_length = NULL
rp$supermajority60 = NULL
rp$supermajority66 = NULL
rp$supermajority75 = NULL
rp$district_magnitude = NULL
rp$AllRelig = NULL # not specific nor a proportion out of pop
rp$district = NULL

# repeat for democrats
dm = subset(df, democrat == 1)
dm$democrat = NULL
dm$republican = NULL
dm$urban = NULL
dm$under25 = NULL
dm$age20_44 = NULL
dm$c_votes = NULL # reveals election results
dm$d_votes_floser = NULL # reveals election results
dm$independent = NULL
dm$votemargin = NULL # reveals the margin of victory
dm$pctnonwhite = NULL
dm$pctnevermarried = NULL # makes default current marriage status (combine with missing divorce)
dm$district_magnitude = NULL
dm$thrdparty = NULL # mostly missing
dm$legis_ideal = NULL # mostly missing & not demographic
dm$district = NULL

dm$vote_no = NULL
dm$vote_yes = NULL
dm$election_year = NULL
dm$bill_number = NULL
dm$Bill_Prefix = NULL
dm$VoteDate = NULL
dm$lawmaker_name = NULL
dm$state_abbrev = NULL
dm$statename = NULL
dm$Year = NULL
dm$term_length = NULL
dm$supermajority60 = NULL
dm$supermajority66 = NULL
dm$supermajority75 = NULL
dm$AllRelig = NULL # not specific nor a proportion out of pop
dm$mmd = NULL # shows median marginal district 


dm <- na.omit(dm) # drop 8 rows
rp <- na.omit(rp) # drop 19 rows

allvars_dm <- lm(margin_of_victory ~ ., data = dm)
allvars_rp <- lm(margin_of_victory ~ ., data = rp)


par(mfrow = c(1, 2))
plot(allvars_dm, ,which = 1:2, main="Diagnostic Plots for Democrat Full Model")

plot(allvars_rp, which=1:2, main="Diagnostic Plots for Republican Full Model")

library(MASS)
boxcox(allvars_rp)
boxcox(allvars_dm)

# boxcox reveals best lambda is 0.5
library(dplyr)
dm <- dm %>%
  mutate(margin_of_victory_sqrt = sqrt(margin_of_victory))

rp <- rp %>%
  mutate(margin_of_victory_sqrt = sqrt(margin_of_victory))

allvars_dm <- lm(margin_of_victory_sqrt ~ ., data = dm)
allvars_rp <- lm(margin_of_victory_sqrt ~ ., data = rp)


# residual plots still show a U-curve
par(mfrow = c(1, 2))
plot(allvars_dm, ,which = 1:2, main="Diagnostic Plots for Democrat Full Model")

plot(allvars_rp, which=1:2, main="Diagnostic Plots for Republican Full Model")

resids_dm <- residuals(allvars_dm)
resids_rp <- residuals(allvars_rp)

predictor_columns_dm <- names(dm)[c(1:7,9:21)]
predictor_columns_rp <- names(rp)[c(1:7,9:21)]

set.seed(11)
par(mfrow = c(3, 3), mar = c(4, 4, 1, 1)) # Set up a 3x3 plotting grid
for (col_name in predictor_columns_dm) {
  plot(dm[[col_name]], resids_dm, 
       xlab = col_name, 
       ylab = "Residuals", 
       main = paste("Democrat Residuals vs.", col_name))
  # Add a smoothed line (lowess) to highlight the pattern
  lines(lowess(dm[[col_name]], resids_dm), col = "red", lwd = 2) 
}

for (col_name in predictor_columns_rp) {
  plot(rp[[col_name]], resids_rp, 
       xlab = col_name, 
       ylab = "Residuals", 
       main = paste("Republican Residuals vs.", col_name))
  # Add a smoothed line (lowess) to highlight the pattern
  lines(lowess(rp[[col_name]], resids_rp), col = "red", lwd = 2) 
}

par(mfrow = c(1, 1))

transformed_model_poly_dm <- lm(
  margin_of_victory_sqrt ~ poly(SSMC, 2) + poly(EP, 2) + . - margin_of_victory, # includes all other untransformed columns
  data = dm)

transformed_model_poly_rp <- lm(
  margin_of_victory_sqrt ~ poly(pctbachelordeg, 2) + poly(rural,2) + 
    poly(SSMC,2) + poly(EP,2) + . - margin_of_victory, # includes all other untransformed columns
  data = rp)

par(mfrow = c(1, 2))
plot(transformed_model_poly_dm, which = 1:2)
plot(transformed_model_poly_rp, which = 1:2)
par(mfrow = c(1, 1))

aic_step_dm = step(allvars_dm, direction="both")
aic_step_rp = step(allvars_rp, direction="both")

model_full_dm <- lm(margin_of_victory ~ ., data = dm)
model_full_rp <- lm(margin_of_victory ~ ., data = rp)

library("tidymodels")
# 80% train, 0% validation, 20% test -> will use cross validation 10-folds
dm_split <- initial_split(dm, prop = 0.8)

# Extract the individual datasets
dm_train <- training(dm_split)
dm_test <- testing(dm_split)

rp_split <- initial_split(rp, prop = 0.8)

# Extract the individual datasets
rp_train <- training(rp_split)
rp_test <- testing(rp_split)


library(MASS)
library(glmnet)
library(caret)

dm_final_formula = margin_of_victory_sqrt ~ poly(SSMC, 2) + poly(EP, 2) + . - margin_of_victory
rp_final_formula = margin_of_victory_sqrt ~ poly(pctbachelordeg, 2) + poly(rural,2) + 
  poly(SSMC,2) + poly(EP,2) + . - margin_of_victory

# use trainControl function from caret, use RMSE as the metric
ctrl <- trainControl(
  method = "cv", # cross-validation 
  number = 10 # 10-Fold CV
)

model_full_dm <- train(
  dm_final_formula,
  data = dm,
  method = "lm", # Standard Linear Model
  trControl = ctrl,
  metric="RMSE"
)


model_full_rp <- train(
  rp_final_formula,
  data = rp,
  method = "lm", # Standard Linear Model
  trControl = ctrl,
  metric="RMSE"
)

lasso_model_dm <- train(
  dm_final_formula,
  data = dm,
  method = "glmnet",
  trControl = ctrl,
  preProc = c("center", "scale"), # IMPORTANT for LASSO
  tuneGrid = expand.grid(.alpha = 1, .lambda = 10^seq(-3, 0, length = 50)), # Pure LASSO
  metric="RMSE"
)

lasso_model_rp <- train(
  rp_final_formula,
  data = rp,
  method = "glmnet",
  trControl = ctrl,
  preProc = c("center", "scale"), # IMPORTANT for LASSO
  tuneGrid = expand.grid(.alpha = 1, .lambda = 10^seq(-3, 0, length = 50)),
  metric="RMSE" # Pure LASSO
)



model_stepwise_approx_dm <- train(
  dm_final_formula,
  data = dm,
  method = "leapSeq", # Sequential Search (Forward/Backward)
  trControl = ctrl,
  tuneGrid = expand.grid(.nvmax = 1:10),
  metric="RMSE" # Test models with 1 to N-1 predictors
)


model_stepwise_approx_rp <- train(
  rp_final_formula,
  data = rp,
  method = "leapSeq", # Sequential Search (Forward/Backward)
  trControl = ctrl,
  tuneGrid = expand.grid(.nvmax = 1:10),
  metric="RMSE" # Test models with 1 to N-1 predictors
)

results_dm <- resamples(list(
  Full_Model = model_full_dm,
  LASSO_Model = lasso_model_dm,
  Stepwise_Cp = model_stepwise_approx_dm
))

results_rp <- resamples(list(
  Full_Model = model_full_rp,
  LASSO_Model = lasso_model_rp,
  Stepwise_Cp = model_stepwise_approx_rp
))

# Summarize the average performance (Mean and SD of RMSE)
summary(results_dm)
summary(results_rp)

par(mfrow = c(1, 2))

# Visualize the comparison (lower box means better RMSE)
bwplot(results_dm, metric = "RMSE")
bwplot(results_rp, metric = "RMSE")

best_dm_model = model_full_dm
best_rp_model = lasso_model_rp

dm_predictions_sqrt <- predict(best_dm_model, newdata = dm_test)
rp_predictions_sqrt <- predict(best_rp_model, newdata = rp_test)


# undo sqrt transformation
dm_predictions_original <- dm_predictions_sqrt^2
rp_predictions_original <- rp_predictions_sqrt^2

dm_true_Y <- dm_test$margin_of_victory
rp_true_Y <- rp_test$margin_of_victory

dm_rmse <- sqrt(mean((dm_predictions_original - dm_true_Y)^2))
rp_rmse <- sqrt(mean((rp_predictions_original - rp_true_Y)^2))

# r-squared
dm_r_squared <- cor(dm_predictions_original, dm_true_Y)^2
rp_r_squared <- cor(rp_predictions_original, rp_true_Y)^2


