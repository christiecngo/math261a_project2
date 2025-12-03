setwd("C:\\Users\\chris\\Documents\\MAT 261A\\Project 2\\Data")

df = read.csv("Trans Athletes DATA.csv")
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

plot(dm$pctnonwhite, logit_d_mov)

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


allvars_dm <- lm(margin_of_victory ~ ., data = dm)
allvars_rp <- lm(margin_of_victory ~ ., data = rp)

aic_step_dm = step(allvars_dm, direction="both")
aic_step_rp = step(allvars_rp, direction="both")

model_full_dm <- lm(margin_of_victory ~ ., data = dm)
model_full_rp <- lm(margin_of_victory ~ ., data = rp)


library(car)
vif(aic_step_dm)
vif(aic_step_rp)


library(MASS)
library(glmnet)

dm <- na.omit(dm) # drop 8 rows
rp <- na.omit(rp) # drop 19 rows


X_dm =  model.matrix(margin_of_victory ~ ., data = dm)[, -1] # drop added intercept
y_dm = dm$margin_of_victory

ridge_res_dm <- glmnet(X_dm, y = y_dm, alpha = 0, nfolds=10) # alpha = 0 for ridge
lasso_res_dm <- glmnet(X_dm, y = y_dm, alpha = 1, nfolds=10) # alpha = 1 for LASSO

X_rp =  model.matrix(margin_of_victory ~ ., data = rp)[, -1] # drop added intercept
y_rp = rp$margin_of_victory

ridge_res_rp <- glmnet(X_rp, y = y_rp, alpha = 0, nfolds=10) # alpha = 0 for ridge
lasso_res_rp <- glmnet(X_rp, y = y_rp, alpha = 1, nfolds=10) # alpha = 1 for LASSO

# SELECTED lambda

cv_lasso$lambda.min
cv_ridge$lambda.min

ridge_res_dm <- glmnet(X_dm, y = y_dm, alpha = 0, nfolds=10, lambda = ridge_res_dm$lambda.min) # alpha = 0 for ridge
lasso_res_dm <- glmnet(X_dm, y = y_dm, alpha = 1, nfolds=10, lambda = lasso_res_dm$lambda.min) # alpha = 1 for LASSO

ridge_res_rp <- glmnet(X_rp, y = y_rp, alpha = 0, nfolds=10, lambda=ridge_res_rp$lambda.min) # alpha = 0 for ridge
lasso_res_rp <- glmnet(X_rp, y = y_rp, alpha = 1, nfolds=10, lambda=lasso_res_rp$lambda.min) # alpha = 1 for LASSO

coef(ridge_res_dm)

coef(lasso_res_dm)

coef(ridge_res_rp)

coef(lasso_res_rp)


library("tidymodels")
# 80% train, 10% validation, 10% test
dm_split <- initial_validation_split(dm, prop = c(0.8, 0.10)) 

# Extract the individual datasets
dm_train <- training(dm_split)
dm_validation <- validation(dm_split)
dm_test <- testing(dm_split)

rp_split <- initial_validation_split(rp, prop = c(0.8, 0.10)) 

# Extract the individual datasets
rp_train <- training(rp_split)
rp_validation <- validation(rp_split)
rp_test <- testing(rp_split)

library(leaps)
best_subsets_mod_dm <- regsubsets(margin_of_victory ~ ., data = dm, nvmax = 10)
best_subsets_summary_dm <- summary(best_subsets_mod_dm)
plot(1:10, best_subsets_summary_dm$adjr2,
     type = "l",
     xlab = "Number of predictors",
     ylab = "Adjusted r-squared")
points(1:10, best_subsets_summary_dm$adjr2)

best_subsets_mod_rp <- regsubsets(margin_of_victory ~ ., data = rp, nvmax = 10)
best_subsets_summary_rp <- summary(best_subsets_mod_rp)
plot(1:10, best_subsets_summary_rp$adjr2,
     type = "l",
     xlab = "Number of predictors",
     ylab = "Adjusted r-squared")
points(1:10, best_subsets_summary_rp$adjr2)