
# Automatically identify packages that need to be installed:
list.of.packages <- c("grf", "ggplot2", "hdm", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

library(dplyr)
library(ggplot2)
library(hdm)
library(grf)

# IMPORTANT: Set path to directory including the dataset
setwd("SET_PATH")
dt <- read.csv("genderinequality.csv")

# Descriptive statistics
summary(dt)

# Identify the ids which are treated in year 2010
treated_ids <- dt %>%
  filter(year == "2010" & treat == 1) %>%
  pull(id)

# Create treat_full column which also indicates treatment in 2005.
dt <- dt %>%
  mutate(treat_full = ifelse(id %in% treated_ids, 1, 0))

# Group the data by gender and year and take means
summary_dt <- dt %>%
  filter(year %in% c("2005", "2010")) %>%
  group_by(female, year) %>%
  summarise(
    mean_wage = mean(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE)
  )

# Gender wage gap 2005
summary_dt$mean_wage[1] - summary_dt$mean_wage[3]
# and 2010
summary_dt$mean_wage[2] - summary_dt$mean_wage[4]

# Plot the means by gender and year
ggplot(summary_dt, aes(x = year, y = mean_wage, group = as.factor(female), color = as.factor(female))) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_wage - sd_wage, ymax = mean_wage + sd_wage), width = 0.2) +
  labs(
    title = "Mean Wages by Year and Gender",
    x = "Year",
    y = "Mean Wage",
    color = "Gender"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Male", "Female")) +
  theme_minimal()

# Create the same graph grouping by treatment and year: 
summary_dt <- dt %>%
  filter(year %in% c("2005", "2010")) %>%
  group_by(treat_full, year) %>%
  summarise(
    mean_wage = mean(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE)
  )

ggplot(summary_dt, aes(x = year, y = mean_wage, group = as.factor(treat_full), color = as.factor(treat_full))) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_wage - sd_wage, ymax = mean_wage + sd_wage), width = 0.2) +
  labs(
    title = "Mean Wages in Treat and Control Group",
    x = "Year",
    y = "Mean Wage",
    color = "Group"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Control", "Treat")) +
  theme_minimal()

################################################################################
# Employment:


# Identify how many people changed employment status.
emp_change <- dt[dt$year == "2010",]$emp - dt[dt$year == "2005",]$emp 
table(emp_change)
# 4 observations went from employed to unemployed, 25 went from employed to unemployed
# and the rest stayed constant. This is very little variation to work with.

# Create the same graphs for employment:
summary_dt <- dt %>%
  filter(year %in% c("2005", "2010")) %>%
  group_by(female, year) %>%
  summarise(
    mean_emp = mean(emp, na.rm = TRUE),
    sd_emp = sd(emp, na.rm = TRUE)
  )

ggplot(summary_dt, aes(x = year, y = mean_emp, group = as.factor(female), color = as.factor(female))) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_emp - sd_emp, ymax = mean_emp + sd_emp), width = 0.2) +
  labs(
    title = "Mean Employment by Gender",
    x = "Year",
    y = "Employed percentage",
    color = "Gender"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Male", "Female")) +
  theme_minimal()

summary_dt <- dt %>%
  filter(year %in% c("2005", "2010")) %>%
  group_by(treat_full, year) %>%
  summarise(
    mean_emp = mean(emp, na.rm = TRUE),
    sd_emp = sd(emp, na.rm = TRUE)
  )

ggplot(summary_dt, aes(x = year, y = mean_emp, group = as.factor(treat_full), color = as.factor(treat_full))) +
  geom_line() + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_emp - sd_emp, ymax = mean_emp + sd_emp), width = 0.2) +
  labs(
    title = "Mean Employment by treatment group",
    x = "Year",
    y = "Employment percentage",
    color = "Group"
  ) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Control", "Treatment")) +
  theme_minimal()

# Clear environment
rm(list = ls())


################################################################################
################################################################################
# Analysis of effects on wages:

dt <- read.csv("genderinequality.csv")

# Prepare the data:
dt$meduc <- NULL
dt$feduc <- NULL
dt <- na.omit(dt)

# Now calculate first differences: 
# Separate the data by year:
dt_2010 <- dt[dt$year == 2010,]
dt_2005 <- dt[dt$year == 2005, ]

# Calculate the differences between years for each column:
dt_fd <- dt_2010 - dt_2005

# Remove columns that have no variation:
dt_fd <- dt_fd[, which(apply(dt_fd, 2, var) != 0)]

# Change variable names:
names(dt_fd) <- paste0(names(dt_fd), "_diff")

# Add controls from 2005: 
dt_2005 <- dt_2005[-c(1,2,6)]
dt_fd <- cbind(dt_fd, dt_2005)

# Create male and female only datasets
dt_fd_fem <- dt_fd[dt_fd$female ==1, ]
dt_fd_mal <- dt_fd[dt_fd$female ==0, ]

# Employed and unemployed only datasets
dt_fd_emp <- dt_fd[dt_fd$emp ==1, ]
dt_fd_unemp <- dt_fd[dt_fd$emp ==0, ]

# Female and employed datasets
dt_fd_fem_emp <- dt_fd[dt_fd$emp ==1 & dt_fd$fem ==1, ]

# Now a number of different datasets, outcome and treatment vectors will be created
# for different groups of the population: Female, male, employed, unemployed, and
# employed females. This allows for inference within these groups.

# Treatment indicator W:
W <- dt_fd$treat_diff
W_fem <- dt_fd_fem$treat_diff
W_mal <- dt_fd_mal$treat_diff
W_emp <- dt_fd_emp$treat_diff
W_unemp <- dt_fd_unemp$treat_diff
W_fem_emp <- dt_fd_fem_emp$treat_diff
W_interaction <-dt_fd[c("female", "emp")]

# Potential outcome variables:
outcome_vars <- dt_fd[c("wage_diff", "emp_diff", "emp", "hours")]
outcome_vars_fem <- dt_fd_fem[c("wage_diff", "emp_diff", "emp", "hours")]
outcome_vars_mal <- dt_fd_mal[c("wage_diff", "emp_diff", "emp", "hours")]
outcome_vars_emp <- dt_fd_emp[c("wage_diff", "emp_diff", "emp", "hours")]
outcome_vars_unemp <- dt_fd_unemp[c("wage_diff", "emp_diff", "emp", "hours")]
outcome_vars_fem_emp <- dt_fd_fem_emp[c("wage_diff", "emp_diff", "emp", "hours")]

# Covariate Matrix X
X <- dt_fd[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff"))]
X_fem <- dt_fd_fem[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "female"))]
X_mal <- dt_fd_mal[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "female"))]
X_emp <- dt_fd_emp[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "emp"))]
X_unemp <- dt_fd_unemp[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "emp"))]
X_fem_emp <- dt_fd_fem_emp[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "emp", "female"))]

# Demean
demean <- function (x){ x- mean(x) }
X <- as.data.frame(apply(X, 2, FUN=demean))
X_fem <- as.data.frame(apply(X_fem, 2, FUN=demean))
X_mal <- as.data.frame(apply(X_mal, 2, FUN=demean))
X_emp <- as.data.frame(apply(X_emp, 2, FUN=demean))
X_unemp <- as.data.frame(apply(X_unemp, 2, FUN=demean))
X_fem_emp <- as.data.frame(apply(X_fem_emp, 2, FUN=demean))

Y <- demean(outcome_vars$wage_diff)
Y_fem <- demean(outcome_vars_fem$wage_diff)
Y_mal <- demean(outcome_vars_mal$wage_diff)
Y_emp <- demean(outcome_vars_emp$wage_diff)
Y_unemp <- demean(outcome_vars_unemp$wage_diff)
Y_fem_emp <- demean(outcome_vars_fem_emp$wage_diff)

################################################################################

# Double Lasso without interaction terms:
X_lasso <- as.data.frame(cbind(W, X))

# Partialing out:
lasso.fit <- rlassoEffects(X_lasso, Y, index=1, post=FALSE)
summary(lasso.fit)

# Double selection: (Instead of estimating OLS on the residuals, estimate Y on W 
# and the selected covariates, that had nonzero coefficients.
lasso.fit <- rlassoEffects(X_lasso, Y, index=1, method = "double selection", post=FALSE)
summary(lasso.fit)

# Include logical interaction terms: treat X female, and treatXemployment:
X_lasso$W_fem <- W_interaction$female * W
lasso.fit <- rlassoEffects(X_lasso, Y, index=c(1,22), post = FALSE)
summary(lasso.fit)

# Run the regression for males only:
X_lasso_mal <- as.data.frame(cbind(W_mal, X_mal))
lasso.fit <- rlassoEffects(X_lasso_mal, Y_mal, index= 1, post = FALSE)
summary(lasso.fit)
# There appears to be no treatment effect among males

# Now for employment:
X_lasso$W_fem <- NULL
X_lasso$W_emp <- W_interaction$emp *W
lasso.fit <- rlassoEffects(X_lasso, Y, index=c(1,22), method = "double selection", post = FALSE)
summary(lasso.fit)
lasso.fit <- rlassoEffects(X_lasso, Y, index=c(1,22), post = FALSE)
summary(lasso.fit)

# For the unemployed only:
X_lasso_unemp <- as.data.frame(cbind(W_unemp, X_unemp))
lasso.fit <- rlassoEffects(X_lasso_unemp, Y_unemp, index= 1, post = FALSE)
summary(lasso.fit)
# There is no significant effect here either, but the sample is very small (~100 observations) .

# Since there does not appear to be a treatment effect pressent in males, I now turn
# to exploring the effects among females in more detail
# Include interaction terms:

# Now add interaction terms for female observations only:
X_interaction_fem <- X_fem
X_interaction_fem$W_fem <- W_fem
colnames(X_interaction_fem)
for (col in names(X_interaction_fem)) {
  if (col != "W_fem") {
    X_interaction_fem[paste(col, "W_fem", sep="_")] <- X_interaction_fem[, col] * X_interaction_fem$W_fem
  }
}

original_columns <- colnames(X_interaction_fem)[1:19]
# Creating interaction terms for each pair of variables
for (i in 1:(length(original_columns) - 1)) {
  for (j in (i+1):length(original_columns)) {
    new_col_name <- paste(original_columns[i], original_columns[j], sep="_")
    X_interaction_fem[new_col_name] <- X_interaction_fem[, original_columns[i]] * X_interaction_fem[, original_columns[j]]
  }
}
# Squared terms
squared_cols <- c("hours", "IQ", "KWW", "educ", "exper", "tenure", "age")
for (col in squared_cols) {
  X_interaction_fem[paste(col, "squared", sep="_")] <- X_interaction_fem[, col]^2
}

# Now run double lasso on data including the interaction terms:
colnames(X_interaction_fem)
interaction_indices_fem <- c(20:39)
lasso.fit <- rlassoEffects(X_interaction_fem, Y_fem, index=interaction_indices_fem, post=FALSE)
summary(lasso.fit)
lasso.fit <- rlassoEffects(X_interaction_fem, Y_fem, index=interaction_indices_fem, method = "double selection", post=FALSE)
summary(lasso.fit)

################################################################################
# Now once more but with employed and female observations only:
X_interaction_fememp <- X_fem_emp
X_interaction_fememp$emp_diff <- NULL
X_interaction_fememp$W_fem_emp <- W_fem_emp
colnames(X_interaction_fememp)
for (col in names(X_interaction_fememp)) {
  if (col != "W_fem_emp") {
    X_interaction_fememp[paste(col, "W_fem_emp", sep="_")] <- X_interaction_fememp[, col] * X_interaction_fememp$W_fem_emp
  }
}
original_columns <- colnames(X_interaction_fememp)[1:17]
# Creating interaction terms for each pair of variables
for (i in 1:(length(original_columns) - 1)) {
  for (j in (i+1):length(original_columns)) {
    new_col_name <- paste(original_columns[i], original_columns[j], sep="_")
    X_interaction_fememp[new_col_name] <- X_interaction_fememp[, original_columns[i]] * X_interaction_fememp[, original_columns[j]]
  }
}

# Add squared terms
squared_cols <- c("hours", "IQ", "KWW", "educ", "exper", "tenure", "age")
for (col in squared_cols) {
  X_interaction_fememp[paste(col, "squared", sep="_")] <- X_interaction_fememp[, col]^2
}

# Now run double lasso on all data:
colnames(X_interaction_fememp)
interaction_indices_fememp <- c(18:35)
lasso.fit <- rlassoEffects(X_interaction_fememp, Y_fem_emp, index=interaction_indices_fememp, post=FALSE)
summary(lasso.fit)

lasso.fit <- rlassoEffects(X_interaction_fememp, Y_fem_emp, index=interaction_indices_fememp, method = "double selection", post=FALSE)
summary(lasso.fit)


################################################################################
# Grow Causal forest on all covariates:
X_forest <- X

# First estimate propensity scores
propensity.forest <- regression_forest(X_forest, W)

# Predict probability of treatment assignment for each value:
propensity.hat <- predict(propensity.forest)
propensity.importance <- variable_importance(propensity.forest)
propensity.importance.cols <- order(propensity.importance, decreasing = TRUE)
colnames(X_forest)[propensity.importance.cols]

# The female variable appears to be the only strong predictor for propensity.
hist(propensity.hat[, "predictions"], main = "Estimate of propensity score distribution", xlab = "Propensity score")
X_forest$p.hat <- propensity.hat$predictions

# Propensity distribution for female observations
hist(X_forest[X_forest$female > 0,]$p.hat, main = "Estimated female propensity distribution", xlab = "Propensity score")

# Propensity distribution for male observations
hist(X_forest[X_forest$female <= 0,]$p.hat, main = "Estimated male propensity distribution", xlab = "Propensity score")

# Estimate a causal forest:
c.forest <- causal_forest(X_forest, Y, W, tune.parameters = "all")

# Calculate the ATE:
average_treatment_effect(c.forest)

# The function best_linear_projection returns estimates of the linear relation between
# the (conditional) ATE and the covariates. While we have no reason to assume that
# the relation is linear, it presents a good first step to investigate if heterogeneity
# is present and which variables appear to drive it.
blp <- best_linear_projection(c.forest, X_forest)
blp

# Confirm the non treatment effects from double lasso estimators:
# Treatment effect for the female and employed:
average_treatment_effect(c.forest, subset = X_forest$female > 0 & X_forest$emp >0) 
# Treatment effect for the unemployed:
average_treatment_effect(c.forest, subset = X_forest$emp <0)
# Treatment effect for the male
average_treatment_effect(c.forest, subset = X_forest$female <0)


# 2. Causal forest with female and employed only:
# Data preparation
X_fem_emp$Y_fem_emp <- Y_fem_emp
X_fem_emp$W_fem_emp <- W_fem_emp
X_fem_emp <- X_fem_emp[X_fem_emp$emp_diff > 0, ]
Y_fem_emp <- X_fem_emp$Y_fem_emp
W_fem_emp <- X_fem_emp$W_fem_emp
X_fem_emp$Y_fem_emp <- NULL
X_fem_emp$W_fem_emp <- NULL

c.forest <- causal_forest(X_fem_emp, Y_fem_emp,W_fem_emp, tune.parameters = "all")

# Calculate the ATE:
average_treatment_effect(c.forest)
best_linear_projection(c.forest, X_fem_emp)

# At the relatively low observation number the causal forest will struggle to get
# a clear identificatione of treatment effect heterogeneity for this many variables.
# Therefore we will focus on a variable subset:
# Causal forest with variable selection:

# Double Lasso without interaction terms to identify relevant variables:
X_lasso <- as.data.frame(cbind(W_fem_emp, X_fem_emp))
# Partialing out:
lasso.fit <- rlassoEffects(X_lasso, Y_fem_emp, index=1, method = "double selection", post=FALSE)

# The variables Lasso selects (nonzero coefficients):
lasso.fit$selection.matrix

# We will now grow the causal forest on the coefficients that where selected
# by Lasso or appeared to drive heterogeneity in previous specifications:

# Lasso variables + all variables that caused significant heterogeneity on the 10%
# level in at least one of the previous specifications for employed women
full_set <- c("educ", "age", "urban", "IQ", "married_diff", "tenure", "hours", "wage")

## Lasso variables + all variables that caused significant heterogeneity on the 1%
# level in at least one of the previous specifications for employed women
base_set <- c("educ", "age", "urban", "IQ", "wage")
X_full <- X_fem_emp[full_set]
X_base <- X_fem_emp[base_set]

c.forest <-causal_forest(X_base, Y_fem_emp, W_fem_emp, tune.parameters = "all")
average_treatment_effect(c.forest)
blp <- best_linear_projection(c.forest, X_base)
blp

c.forest <-causal_forest(X_full, Y_fem_emp, W_fem_emp, tune.parameters = "all")
average_treatment_effect(c.forest)

blp <- best_linear_projection(c.forest, X_full)
blp

# Now create plots of CATE:
# For age quantiles:
quantiles <- quantile(X_full$age, probs = seq(0.1, 1, 0.1))
quantiles <- as.data.frame(quantiles)
cate_list <- vector("list", length(quantiles))
error_list <- vector("list", length(quantiles))
j= 1
for (i in quantiles$quantiles){
  
  cate_list[j] <- average_treatment_effect(c.forest, subset = X_full$age == i)[1]
  error_list[j] <- average_treatment_effect(c.forest, subset = X_full$age == i)[2]
  j = j +1
}
quantiles$CATE <- cate_list
quantiles$SE <- error_list
quantiles$row<-rownames(quantiles)
quantiles$row <- as.numeric(gsub("%", "", quantiles$row))
quantiles$CATE <- unlist(quantiles$CATE)
quantiles$SE <- unlist(quantiles$SE)


p <- ggplot(quantiles, aes(x = row, y = CATE)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  geom_errorbar(aes(ymin = CATE - SE, ymax = CATE + SE), width = 2) +  # Error bars
  labs(title = "CATE at different age percentiles",
       x = "Age percentiles",
       y = "CATE")

print(p)


# Plot for education years:
educ_unique <- unique(X_full$educ)
educ_unique <- as.data.frame(educ_unique)
educ_unique <- as.data.frame(educ_unique[order(educ_unique$educ_unique), ])

cate_list <- vector("list", length(educ_unique))
error_list <- vector("list", length(educ_unique))
j= 1
for (i in educ_unique$educ_unique){

 cate_list[j] <- average_treatment_effect(c.forest, subset = X_full$educ == i)[1]
 error_list[j] <- average_treatment_effect(c.forest, subset = X_full$educ == i)[2]
  j = j +1
}
educ_unique$CATE <- unlist(cate_list)
educ_unique$SE <- unlist(error_list)

educ_unique$years <- c(9:18)


p <- ggplot(educ_unique, aes(x = years, y = CATE)) + 
  geom_point() + 
  #geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  geom_errorbar(aes(ymin = CATE - SE, ymax = CATE + SE), width = 2) +  # Error bars
  labs(title = "CATE at different years of education",
       x = "Years of education",
       y = "CATE")

# Display the plot
print(p)

# Clear environment
rm(list = ls())




################################################################################
################################################################################
# Analysis of effect on employment:

dt <- read.csv("genderinequality.csv")

# Prepare the data:
dt$meduc <- NULL
dt$feduc <- NULL
dt$brthord <- NULL
dt <- na.omit(dt)

# Now calculate first differences: 
# Separate the data by year:
dt_2010 <- dt[dt$year == 2010,]
dt_2005 <- dt[dt$year == 2005, ]

# Calculate the differences between years for each column:
dt_fd <- dt_2010 - dt_2005

# Remove columns that have no variation:
dt_fd <- dt_fd[, which(apply(dt_fd, 2, var) != 0)]
dt_noswitch <- dt_fd[dt_fd$emp ==0, ]
dt_losejob <- dt_fd[dt_fd$emp == -1, ]
dt_findjob <- dt_fd[dt_fd$emp== 1,]

# Change variable names:
names(dt_fd) <- paste0(names(dt_fd), "_diff")
# Add controls from 2005: 
dt_2005 <- dt_2005[-c(1,2,3,6)]
dt_fd <- cbind(dt_fd, dt_2005)

# Treatment indicator W:
W <- dt_fd$treat_diff

# Potential outcome variables:
outcome_vars <- dt_fd[c("wage_diff", "emp_diff", "emp", "hours")]

# Right now the employment difference variable takes three distinct values: 1, 0, and -1.
# We will map these values into separate binary columns now:
outcome_vars <- outcome_vars %>%
  mutate(
    jobgain = as.integer(emp_diff == 1),
    jobsteady = as.integer(emp_diff == 0),
    jobloss = as.integer(emp_diff == -1)
  )

# There are only 4 observations that gained a job, and 25 that lost one. In other
# words, there is very little variation.
# We will now focus on the jobloss variable as outcome, simply because it has more
# observation, allthough it is still very few.
outcome_vars_fem <- dt_fd_fem[c("wage_diff", "emp_diff", "emp", "hours")]
# Covariate Matrix X
X <- dt_fd[, !(colnames(dt_fd) %in% c("wage_diff", "treat_diff", "emp_diff"))]
demean <- function (x){ x- mean(x) }
X <- as.data.frame(apply(X, 2, FUN=demean))

################################################################################
Y <- unlist(outcome_vars$jobloss)
# Double Lasso:
X_lasso <- as.data.frame(cbind(W, X))
X_lasso$emp <- 0
X_lasso <- as.matrix(X_lasso)
lasso.fit <- rlassologitEffects(X_lasso, Y, index=1, post=FALSE)
summary(lasso.fit)
X_lasso <- as.data.frame(X_lasso)
X_lasso$W <- X_lasso$W * X_lasso$female
X_lasso <- as.matrix(X_lasso)
lasso.fit <- rlassologitEffects(X_lasso, Y, index=1, post=FALSE)
summary(lasso.fit)

# Unsurprisingly, no significant effects can be identified. However, this does not mean that there are none.
# Given the little variation it could very well be that there is an effect, that we
# miss due to the limited sample size.


# Confirm the (non)results once with a causal forest:
c.forest <- causal_forest(X, Y, W, tune.parameters = "all")
average_treatment_effect(c.forest)
# Zero effect as well.



