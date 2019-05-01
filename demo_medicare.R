####### Insight Demonstration - May 2018 ####

## Data downloaded from https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier.html
## with information pdf document.

## The data is prepared by the Centers for Medicare & Medicaid Services (CMS).
## It contains information on services and procedures provided to Medicare
## beneficiaries by physicians and other healthcare professionals such as: 
## information on utilization, payment (allowed amount and Medicare payment),
## and submitted charges organized by National Provider Identifier (NPI),
## Healthcare Common Procedure Coding System (HCPCS) code, and caracteristics of
## patients (age, sex, ethnicity, disease).

## The idea is to explore this dataset, answer some (interesting?) questions and try to 
## fit a few models.

## The methodology document (pdf) contains usefull information on the variables that
## potentially interest us.

#################################  Download and subset data  ########################################

### load libraries
library(stringr) # str_to_lower
library(magrittr) # %>% pipeline
library(randomForest)
# library(gplots) # heatmap
# library(mice) # md.pattern
library(cba) # proximus
library(dplyr) # filter, group_by
library(usmap) # plot_usmap
library(ggplot2) # plot_usmap
library(randomForest)

### Set directory
setwd("/Users/laura/Dropbox/Insight2")

### Download the data
# The data was downloaded in a zip file which I manually opened.
mediall <- read.delim("Medicare-Physician-and-Other-Supplier-NPI-Aggregate_CY2016/Medicare_Physician_and_Other_Supplier_NPI_Aggregate_CY2016.txt")
dim(mediall) # over 1 mio enteries of 70 variables

### Subset dataset with variables of interest (after reading pdf)
colnames(mediall) <- str_to_lower(colnames(mediall))
allvar <- names(mediall)

# String patterns to keep 
patternyes <- str_c(c("npi", "entity", "type", "indicator", "number",
                   "zip", "state", "country",
                   "total",
                   "beneficiary"),
                 collapse = "|") # str_which works for one but not many patterns
# String patterns to avoid
patternno <- str_c(c("drug_", "med_"),
                    collapse = "|")
# Subset
myvar <- allvar[grepl(patternyes, allvar)] %>%
  .[!grepl(patternno, .)]

medi <- mediall[ , myvar] # if data too big, overwrite


#################################  Data 1 st check and clean  ########################################

# Check that the variables are coded correctly
str(medi)
# npi is coded as integer and not factor but that is not a problem (probably will not need it)
nrow(medi) == length(unique(medi$npi)) # there is indeed one unique code per provider, good.
# Note: there are close to 300000 zip codes (should not use that in regression)
# Country: I think it is better to change it to a binary variable (USA vs Other)
summary(medi$nppes_provider_country) # indeed very few counts in other countries.
medi$nppes_provider_country_bin <- as.character(medi$nppes_provider_country) # factors are issue to convert
medi$nppes_provider_country_bin[which(medi$nppes_provider_country_bin !="US")] <- "Other"
medi$nppes_provider_country_bin <- as.factor(medi$nppes_provider_country_bin) # reconvert to factors
summary(medi$nppes_provider_country_bin) # all good
# There are some NA -> check if many.


#################################  NA investigation  ########################################

# Which variables have missing data? and how many.

# Put 1 for data 0 for NA
medina <- sapply(medi, as.numeric )
medina[!is.na(medina)] <- 0
medina[is.na(medina)] <- 1
medina <- as.data.frame(medina)
# head(medi$beneficiary_race_hispanic_count)
# head(medina$beneficiary_race_hispanic_count) # check: it works

# Subset data with only the missing data
miss <- which(medina == 1, arr.ind=TRUE)
datamiss <- medina[unique(miss[ ,1]), unique(miss[ ,2])]

# What data has missing values? What percentage?
names(datamiss)
prop.miss <- apply(datamiss, 2,
                   FUN = function(x) round(sum(x) / nrow(medi) * 100, 0))
sort(prop.miss, decreasing = TRUE)
# There are some very high percentages of missing data in some variables.

# Is the missing data clustered? 
smax <- apply(datamiss, 1, sum)
hist(smax)

# This is what I would normally do to further analyse clustering,
# but the data is too big for such analysis and representation.

# library(gplots)
# heatmap.2(t(datamiss2),
#           Rowv=NA, Colv = NA,
#           dendrogram = "none", col=cols,
#           scale="none", key=FALSE, trace = "none",
#           lwid=c(0.05,0.7), lhei=c(0.1,0.4),
#           margins=c(5,8), cexCol=0.5, cexRow = 0.8)
# 
# library(mice)
# mdm <- md.pattern(medi)

# Solution:
library(cba)
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/Proximus
# The intended area of application is the compression of high-dimensional 
# binary data into representative patterns. 
# The Proximus algorithm cluster the rows of a logical matrix.
# The compression rate of the algorithm can be influenced by the choice of the 
# maximum cluster radius and the minimum cluster size.

# The next part is commented because ran once and saved results.

# datamiss2 <- datamiss[c(small), ] # ordered by number of missingness (not sure if necessary)
# datapr <- sapply(datamiss2, as.logical)
# start_time <- Sys.time()
# proxi <- proximus(datapr,
#                   max.radius = 10,
#                   min.size = round(nrow(datapr)*0.1,0),
#                   min.retry = 10, max.iter = 16, debug = FALSE) 
# end_time <- Sys.time()
# saveRDS(proxi, file ="saved_objects")
# end_time - start_time # 1.8 hours!!

# Ideally repeat this a second time with smaller max.radius.
pr <- readRDS("saved_objects")
summary(pr)
sum(summary(pr)$pattern$Size) == nrow(datamiss) # Has clusterd all data
str(summary(pr))
  # summary(pr)$pattern$Length[17]
# There are only 3 clusters with more than 10% pop.
size.cluster <- sapply(pr$a, FUN =function(i) length(i$x))
my.cluster <- which(size.cluster > round(nrow(datamiss)*0.1,0)) # contain more than 10% data
sum(size.cluster[my.cluster][1]) * 100 / nrow(datamiss)
sum(size.cluster[my.cluster][2]) * 100 / nrow(datamiss)
sum(size.cluster[my.cluster][3]) * 100 / nrow(datamiss)
sum(size.cluster[my.cluster]) * 100 / nrow(datamiss)
# They account for about 10 39 and 14% of observations total 63%.

var.cluster <- lapply(pr$a, FUN =function(i) i$y)
sapply(var.cluster[my.cluster], length)
# this fits with the histogram
# hist(smax)

var.cluster[c(my.cluster, 1, 36)] # 1,36 next two important clusters
names(datamiss)[c(10,12,13, 9,11,30)]
# Meanining that if a data about race is missing it is likely that all data about race are missing.
# Since there is so much missing data I would consider removing it from the analysis
# Another aspect I could investigate is looking per state if there is a correlation with missingness.

# So what are the variables with no missing data?
names(medi)[!names(medi)  %in%  names(datamiss)]
# These are the variables where I can exploit the data set to its maximum

# What are the variables that have less that 15% missing data (arbitrary)
prop.miss[prop.miss <= 15]
# I would still use zip, female and male counts.
# Maybe the disease with less than 15%? But not sure it makes sense from an interpretation
# point of view if you do not include all rows of diseases.

# Lets subset the dataset with the variables we want to keep in analysis and only complete rows.
var.keep <- c(names(medi)[!names(medi)  %in%  names(datamiss)],
              "nppes_provider_zip", "beneficiary_female_count", "beneficiary_male_count")
# 19 varibles left including response variable
medi <- medi[ ,var.keep] %>% 
  na.omit()

# Clean up large files if necessary
# rm(datamiss)
# rm(medina)
# rm(miss)

#################################  Data 2nd check and Visualisation: explanatory variable  ########################################

# Lets have a look at the distribution of some of the data (check for outliers and anomalies).
# names(medi)

### proportion of individuals vs organisation
temp <- summary(medi$nppes_entity_code)
percentlabels <- round(100 * temp / sum(temp), 1)
pielabels <- paste(percentlabels, "%", sep="")
pie(temp, main ="Proportion of Individuals vs organizations",
    labels = pielabels,
    cex=0.9,
    col= topo.colors(2))
legend("topright", names(temp), cex=0.8, fill = topo.colors(2))
# There are a lot more indiviuals than organizations.

### proportion of USA claims vs the world
temp <- summary(medi$nppes_provider_country_bin)
(percent <- 100 * temp / sum(temp))
# The percent of claims outside the US is small.

### Provider type
temp <- sort(summary(medi$provider_type), decreasing = TRUE)
length(unique(medi$provider_type))
# 90 specialities.
temp2 <- temp[1:10]
temp2["Other"] <- sum(temp[11:length(temp)])
percentlabels <- round(100 * temp2 / sum(temp2), 1)
pielabels <- paste(percentlabels, "%", sep="")
par(xpd = T, mar = c(5, 4, 4, 13))
pie(temp2, main ="Top 10 Specilizations",
    labels = pielabels,
    cex=0.9,
    radius = 1.1,
    col= topo.colors(length(temp2)))
legend(2, 1,
       names(temp2), cex = 0.5,
       fill = topo.colors(length(temp2)))
par(mar=c(5, 4, 4, 2) + 0.1)

### Proportion of providers participating in medicare
prop.table(table(medi$medicare_participation_indicator))
# Nearly all of them do.

### Distribution of distinct number of services provided per provider
summary(medi$number_of_hcpcs) # Max is high!
# Are the highest numbers from organizations? 
boxplot(split(log(medi$number_of_hcpcs), medi$nppes_entity_code),
        ylab="Log scale")
  # boxplot(split(medi$number_of_hcpcs, medi$nppes_entity_code))$stat
# doesnt look like it... many outliers
# so there are individuals that perform over 400 different services?!
# isn't that a lot? not sure how to explain these numbers...

### Distribution of total number of services provided per provider
summary(medi$total_services)
boxplot(split(log(medi$total_services), medi$nppes_entity_code),
        ylab="Log scale")
# These are higher numbers than I expected... many outliers
# Again the high numbers can be individuals.

### Number of medicare beneficiaries receiving service
summary(medi$total_unique_benes)
boxplot(log(medi$total_unique_benes),
        ylab = "Log scale")
# So many outliers...

# Do the providers with many distinct services provide more total services?
lm.temp <- lm(log(medi$total_services) ~ log(medi$number_of_hcpcs))
summary(lm.temp) # there is a positive correlation
# plot(log(medi$total_services), log(medi$number_of_hcpcs)) # long to plot
# abline(lm.temp$coefficients[1], lm.temp$coefficients[2], col=2) # For some reason abline doesnt work.. 


# Do the providers with many services also have more clients?
lm.temp <- lm(log(medi$total_services) ~ log(medi$total_unique_benes))
summary(lm.temp) # there is a positive correlation
# plot(log(medi$total_services), log(medi$total_unique_benes)) # long to plot
# abline(lm.temp$coefficients[1], lm.temp$coefficients[2], col=2) # For some reason abline doesnt work..

# average number of service per client per provider
medi$av_service <- medi$total_services / medi$total_unique_benes
summary(medi$av_service)
# Usually clinitians bill 3 services (median) per client in 2016.
# I think there might be an issue with the very high values (ask a doctor?)
boxplot(log(medi$av_service))

# There are many outliers in some variables but I do not have sufficiant knowledge to make a
# decision on that. I will keep them.

#################################  Data 2nd check and Visualisation: response variable  ########################################

# There are 4 payement variables of interest (page 9 method pdf):


# total_submitted_chrg_amt: The total charges that the provider submitted for all services.
# total_medicare_allowed_amt: The Medicare allowed amount for all provider services.
  # This figure is the sum of the amount Medicare pays, the deductible and coinsurance amounts
  # that the beneficiary is responsible for paying, and any amounts that a third party is 
  # responsible for paying.
# total_medicare_payment_amt: Total amount that Medicare paid after deductible and coinsurance
  # amounts have been deducted for all the provider's line item services.
# total_medicare_stnd_amt: Total amount that Medicare paid after deductible and coinsurance amounts
 # have been deducted for the line item service and after standardization of the Medicare payment
  # has been applied. Standardization removes geographic differences in payment rates for individual
  # services, such as those that account for local wages or input prices and makes Medicare payments
 # across geographic areas comparable, so that differences reflect variation in factors such as
 # physicians’ practice patterns and beneficiaries’ ability and willingness to obtain care.

# lets check if there are some discrepancy between the amount the provider charges and the amount
# allowed by medicare.

diff.pay <- medi$total_submitted_chrg_amt - medi$total_medicare_payment_amt
summary(diff.pay)
# WHAT!! median of 100'000$! this is crazy.
# Apparently there is a 15% margin that providers can bill.
diff.pay2 <- medi$total_submitted_chrg_amt - (medi$total_medicare_payment_amt*1.15)
summary(diff.pay2)
# still far to high...

# What percentage of the allowed amount is overcharged?
diff.pay.per <-  diff.pay / (medi$total_medicare_payment_amt+1)
summary(diff.pay.per)
is.numeric(diff.pay.per)
# Most hospitals charge over 2x the allowed amount!!
# explanation: https://strategicdynamicsfirm.com/understanding-hospital-charges-costs-and-payments/


# Probably better to standardize by number of services.
diff.pay.ser <- diff.pay / medi$total_services
summary(diff.pay.ser)
# There is on average 100 $ per service overcharged. 
# I think that this is an interesting parameter and will study it in more depth.

### Visualisation

library(dplyr)

# Lets visualize the excess chargeds by state
# we need to aggregate the data by state
medi$excess.stand <- diff.pay.ser
medi.state <- medi %>%
  filter(nppes_provider_country_bin !="Other") %>%  # remove non US data
  group_by(nppes_provider_state) %>%  #group data by state
  summarise(avg = mean(excess.stand),
            max = max(excess.stand))

# plot the result on map
library(usmap)
library(ggplot2)

nrow(statepop)
nrow(medi.state)# we have extra USA territories that unfortunately cannot be mapped...

medi.state.red <- medi.state %>% 
  filter(nppes_provider_state %in% statepop$abbr)

# lets add our values to the data statepop.
# just checking the states are sorted identically in both dataframes
isTRUE(medi.state.red$nppes_provider_state == statepop$abbr) # no we have some miss match
medi.state.red <- medi.state.red[match(statepop$abbr, medi.state.red$nppes_provider_state), ] # rearrange
statepop$avg <- medi.state.red$avg
statepop$max <- medi.state.red$max

plot_usmap(data = statepop, values = "avg", lines = "white") + 
  scale_fill_continuous(name = "Average excess charges", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = statepop, values = "max", lines = "white") + 
  scale_fill_continuous(name = "Maximum excess charges", label = scales::comma) + 
  theme(legend.position = "right")


###

#################################  Prediction: lm  ########################################

# Subset the data to our use.
var.keep <- c("nppes_entity_code",
              "nppes_provider_state",
              "nppes_provider_country_bin",
              "medicare_participation_indicator",
              "provider_type",
              "number_of_hcpcs",
              "total_services",
              "total_unique_benes",
              "beneficiary_average_age",
              "beneficiary_average_risk_score",
              "beneficiary_female_count",
              # "beneficiary_male_count", # gives NA not sure why
              "excess.stand")
medi.pred <- medi %>%
  .[var.keep]
dim(medi.pred)
# 1 response variable and 11 explanatory variables.
# How many parameters?
str(medi.pred)
# 5 factors with total of 158-5 = 153 levels
# 7 countinuous variables (6 without male_counts)
# 1 intercept
# total : 161 parameters, 71 if take provider type out. 11 if also take country out.

### Split training data (80%) and validation set (20%)
train <- sample(nrow(medi.pred), 0.8*nrow(medi.pred), replace = FALSE)
train.set <- medi.pred[train,]
valid.set <- medi.pred[-train,]

### Linear regression
# The first idea is to use a linear regression to predict the excess charges.
# We can imagine 3 nested models withdrawing the varibles with many parameters.

lin.mod.11 <- lm(excess.stand ~ .,
                  data = subset(train.set, select=-c(provider_type, nppes_provider_state)))

# regression: check:
# Homoscedasticity of residuals or equal variance + normality.

# Check residues: I normally plot them and visually check this. here: too long to plot (crash).
# par(mfrow = c(2,2))
# plot(lin.mod.11)
# plot(lin.mod.71)
# plot(lin.mod.161)
# par(mfrow = c(1,1))

# summary(train.set$beneficiary_male_count)
# plot(x = fitted(lin.mod.11),
#      y = resid(lin.mod.11),
#      main = "Turkey Anscombe Plot")
# abline( h=0,
#         col = "red")
# not sure that this looks good never seen anything like this...

summary(lin.mod.11)

# in model11 all but the provider_country are significant.
# Being an organisation, participating in medicare, offering many different services,
# increasing number of beneficiaries and higher risk increases the amount overcharged.
# this can make sense.
# Problem with male_counts.
# residues not normal

# Try to log transform the response variable
train.set.log <- train.set
train.set.log$excess.stand <- log(train.set.log$excess.stand )
valid.set.log <- valid.set
valid.set.log$excess.stand <- log(valid.set.log$excess.stand )

summary(train.set.log$excess.stand )
lin.mod.11.log <- lm(excess.stand ~ .,
                 data = subset(train.set.log, select=-c(provider_type, nppes_provider_state)))
summary(lin.mod.11.log)
# better distribution of residuals.
# but provider is now negatively influencing the response.
# plot(x = fitted(lin.mod.11.log),
#      y = resid(lin.mod.11.log),
#      main = "Turkey Anscombe Plot")
# abline( h=0,
#         col = "red")
# This also looks better (1min for plot)
# Fit other 2 models
lin.mod.161.log <- lm(excess.stand ~ .,
                  data = train.set.log)
lin.mod.71.log <- lm(excess.stand ~ .,
                  data = subset(train.set.log, select=-provider_type))

summary(lin.mod.71.log)
# same direction for explanatory var. 
# states are not significant

summary(lin.mod.161.log)
# some changes in significance of main predictors(11)
# states still not significant
# some types are significant.

## Predict Validation set
lin.mod.11.log.pred <- predict(lin.mod.11.log,
                               newdata = valid.set.log,
                               type = "response")

lin.mod.71.log.pred <- predict(lin.mod.71.log,
                               newdata = valid.set.log,
                               type = "response")

lin.mod.161.log.pred <- predict(lin.mod.161.log,
                               newdata = valid.set.log,
                               type = "response")
## Score
# I will try two widely used (but appropriate?) scores:
# mean absolute error (MAE)
MAE <-  function(pred, obs){
  mean(abs(pred - obs))
}
# Root mean squared error (RMSE)
RMSE <-  function(pred, obs){
sqrt(mean((pred - obs)^2))
}

# I insert a dummy (predict average excess) to compare performances
excess.std.dummy <- mean(train.set.log$excess.stand)
(mae.dummy <- MAE(excess.std.dummy, valid.set.log$excess.stand)) #1.117074
(mae.lm.11 <- MAE(lin.mod.11.log.pred, valid.set.log$excess.stand))
(mae.lm.71 <- MAE(lin.mod.71.log.pred, valid.set.log$excess.stand))
(mae.lm.161 <- MAE(lin.mod.161.log.pred, valid.set.log$excess.stand))

(rmse.dummy <- RMSE(excess.std.dummy, valid.set.log$excess.stand))
(rmse.lm.11 <- RMSE(lin.mod.11.log.pred, valid.set.log$excess.stand))
(rmse.lm.71 <- RMSE(lin.mod.71.log.pred, valid.set.log$excess.stand))
(rmse.lm.161 <- RMSE(lin.mod.161.log.pred, valid.set.log$excess.stand))

# all models do better than dummy
# The first two models are very similar (state doesnt help much)
# but the last one, most complex is superior in terms of MAE and RMSE
# Globally the error is very small!


#################################  Prediction: Random Forest  ########################################

### Random Forest
# Random forest a limitations: no more than 32 factors in model
# So state and provider type cannot be used.
library(randomForest)

# Need to clear some memory
# rm(list=setdiff(ls(), c("train.set.log", "valid.set.log", "medi.pred",
#                         "mae.dummy", "mae.lm.11", "mae.lm.71",
#                         "mae.lm.161", "rmse.dummy",
#                         "rmse.lm.11", "rmse.lm.71", "rmse.lm.161")))
gc() # garbage collector (frees up RAM)

# start_time <- Sys.time()
# rf1 <- randomForest(excess.stand ~ .,
#                     data = subset(train.set.log, select=-c(provider_type, nppes_provider_state)),
#                     importance = TRUE)
# end_time <- Sys.time()
# error: Error: vector memory exhausted (limit reached?)

train.small <- sample(nrow(train.set.log), 0.2*nrow(train.set.log), replace = FALSE)
train.set.log.small <- train.set.log[train.small,]
# valid.set <- medi.pred[-train,]
# start_time <- Sys.time()
# rf1 <- randomForest(excess.stand ~ .,
#                     data = subset(train.set.log.small, select=-c(provider_type, nppes_provider_state)),
#                     importance = TRUE)
# end_time <- Sys.time()
# # Still too long

# library(ranger)
# system.time(
#   rf1v<- ranger(
#     formula = excess.stand ~ ., 
#     data = subset(train.set.log.small, select=-c(provider_type, nppes_provider_state)),
#     num.trees = 300,
#     mtry      = 3, # we have 10 variables.recommended :a third
#     importance = 'impurity'
#   )
# ) # 304sec
# # try on a smaller dataset.
# # try to install bigrf? (removed from CRAN)
# 
# system.time(
#   rf2<- ranger(
#     formula = excess.stand ~ ., 
#     data = subset(train.set.log.small, select=-c(provider_type, nppes_provider_state)),
#     num.trees = 500,
#     mtry      = 3, # we have 10 variables.recommended :a third
#     importance = 'impurity'
#   )
# )# 506 sec
# 
# system.time(
#   rf3<- ranger(
#     formula = excess.stand ~ ., 
#     data = subset(train.set.log, select=-c(provider_type, nppes_provider_state)),
#     num.trees = 500,
#     mtry      = 3, # we have 10 variables.recommended :a third
#     importance = 'impurity'
#   )
# ) #4100 sec
# saveRDS(rf1v, file ="rf1")
# saveRDS(rf2, file ="rf2")
# saveRDS(rf3, file ="rf3")

rf1 <- readRDS("rf1")
rf2 <- readRDS("rf2")
rf3 <- readRDS("rf3")

rf1.pred <- predict(rf1,
                    valid.set.log,
                    type = "response")

rf2.pred <- predict(rf2,
                    valid.set.log,
                    type = "response")

rf3.pred <- predict(rf3,
                    valid.set.log,
                    type = "response")

excess.std.dummy <- mean(train.set.log$excess.stand)
(mae.dummy <- MAE(excess.std.dummy, valid.set.log$excess.stand))
(mae.rf1 <- MAE(rf1.pred$predictions, valid.set.log$excess.stand))
(mae.rf2 <- MAE(rf2.pred$predictions, valid.set.log$excess.stand))
(mae.rf3 <- MAE(rf3.pred$predictions, valid.set.log$excess.stand))

(rmse.dummy <- RMSE(excess.std.dummy, valid.set.log$excess.stand))
(rmse.rf1 <- RMSE(rf1.pred$predictions, valid.set.log$excess.stand))
(rmse.rf2 <- RMSE(rf2.pred$predictions, valid.set.log$excess.stand))
(rmse.rf3 <- RMSE(rf3.pred$predictions, valid.set.log$excess.stand))

# Interesting!!

rmse.res <- c(rmse.dummy,
              rmse.lm.11, rmse.lm.71, rmse.lm.161,
              rmse.rf1, rmse.rf2, rmse.rf3)
mae.res <- c(mae.dummy,
              mae.lm.11, mae.lm.71, mae.lm.161,
              mae.rf1, mae.rf2, mae.rf3)
results <- cbind(mae.res, rmse.res)
row.names(results) <- c("Dummy",
                        "lm.11", "lm.71", "lm.161",
                        "rand.forest1", "rand.forest2", "rand.forest3")
print(results)

# TO DO: - fine tuning random forest,
#        - look at important variables
#        - check OOB?
#################################  Ideas and Questions  ########################################

# Maybe should have used median excess charges when aggregating

# Can I use a proportion of Female and Male as a independant variable instead of 2 seperate counts?
  # Would say yes but how would you interpret the result over 100?
# Why does male_counts give NA in regression?

# Could investigate NA further.
  # Could sample the data (1 in 100 maybe) to reduce amount of data for missingness.
    # would make things faster and prabably not loose to much info.
  # Replace NA rather than omiting incomplete data: does it improve predictions?
  # We could replace the missing data (mean, median, or regression/tree to predict missing)
  # Ideally repeat proximus this a second time with smaller max.radius.

# Perform cross-validation (K fold, k=5)
# Try other methods (splines? MARS ?PCA? KNN, lasso for type)
# Fine tune random forest (grid search, mtry, num.tree -> look into package h2o)

# Way to test regression assumption (make plots?)
# Why did one explanatory variable changed direction?

# Try other scores?

# Paper: The Importance of the Normality Assumption in Large Public Health Data 
