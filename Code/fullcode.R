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
rp$dist_votes = NULL # linear combo of votes divided by party
rp$independent = NULL
rp$votemargin = NULL # reveals the margin of victory
rp$pctnonwhite = NULL

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

dm = subset(df, democrat == 1)
dm$democrat = NULL
dm$republican = NULL
dm$urban = NULL
dm$under25 = NULL
dm$age20_44 = NULL
dm$dist_votes = NULL # linear combo of votes divided by party
dm$independent = NULL
dm$votemargin = NULL # reveals the margin of victory
dm$pctnonwhite = NULL

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

allvars_dm <- lm(margin_of_victory ~ ., data = dm)
allvars_rp <- lm(margin_of_victory ~ ., data = rp)

aic_step_dm = step(allvars_dm, direction="both")
aic_step_rp = step(allvars_rp, direction="both")



library(caret)

# Exclude the response
predictors_dm <- dm[, setdiff(names(dm), "margin_of_victory")]

predictors_dm <- predictors_dm[, sapply(predictors_dm, is.numeric)]
predictors_dm[!is.finite(as.matrix(predictors_dm))] <- NA

predictors_clean_dm <- predictors_dm[, colSums(is.na(predictors_dm)) == 0]


# Find linear combinations
combo <- findLinearCombos(predictors_clean_dm)

combo$remove  # columns to remove
dm_final <- dm[, !names(dm) %in% names(predictors_clean_dm)[combo$remove]]



predictors_rp <- rp[, setdiff(names(rp), "margin_of_victory")]

predictors_rp <- predictors_rp[, sapply(predictors_rp, is.numeric)]
predictors_rp[!is.finite(as.matrix(predictors_rp))] <- NA

predictors_clean_rp <- predictors_rp[, colSums(is.na(predictors_rp)) == 0]


# Find linear combinations
combo <- findLinearCombos(predictors_clean_rp)

combo$remove  # columns to remove
rp_final <- rp[, !names(rp) %in% names(predictors_clean_rp)[combo$remove]]


hist(dm_final$margin_of_victory)
hist(rp_final$margin_of_victory)

# both are right skewed

plot(dm$pctnonwhite, dm$margin_of_victory)


# stepwise AIC

allvars_dm <- lm(margin_of_victory ~ ., data = dm_final, singular.ok = FALSE)
allvars_rp <- lm(margin_of_victory ~ ., data = rp_final, singular.ok = FALSE)

aic_step_dm = step(allvars_dm, direction="both")
aic_step_rp = step(allvars_rp, direction="both")


library(Matrix)


# 2. Extract predictors (including the intercept column of 1s)
# Remove the outcome variable 'margin_of_victory'
predictors_dm <- model.matrix(margin_of_victory ~ ., data = dm)

# 3. Calculate the rank
rank_dm <- Matrix::rankMatrix(predictors_dm)[1]
num_cols_dm <- ncol(predictors_dm)

print(paste("DM Predictor Columns:", num_cols_dm))
print(paste("DM Matrix Rank:", rank_dm))

# If rank_dm < num_cols_dm, you must drop (num_cols_dm - rank_dm) more variables.