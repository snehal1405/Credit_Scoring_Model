### install required packages
### load required packages
library(C50)
library(gmodels)

### read and explore the data
credit <- read.csv("credit.csv")
str(credit)
nrow(credit)
ncol(credit)
### look a couple of loan features that seem likely
### to predict a default (currency Deutsche Marks DM)
table(credit$checking_balance)
table(credit$savings_balance)

### some of the loan's features are numeric
summary(credit$months_loan_duration)
summary(credit$amount)

### $default indicates whether the loan applicant
### went or not into default

### change levels of $default
credit$default<- factor(credit$default, levels = c("1", "2"),
                        labels = c("no", "yes"))
### see frequencies of $default
table(credit$default)

### data preparation
### creating random training (90%) and test datasets (10%)

### random sample numbers
set.seed(123)
train_sample <- sample(1:1000, 900)

### split the dataset
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]

### check for about 30% of defaulted loans in each of
### the datasets
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

### training a model on the data

### 17th column in credit_train is the default class
### variable, so we need to exclude it from the training
### data frame, but supply it as the target factor vector
### for classification
###install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

### see some basic data about the tree
credit_model
summary(credit_model)

### evaluating model performance
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### improving model performance
### boosting the accuracy of decision trees
### add an additional trials parameter indicating the
### number of separate decision trees to use in the
### boosted team
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)

credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### penalties to different types of errors
### cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")

matrix_dimensions

### assign the penalty for the types of errors

### if a loan default costs four times as much as
### a missed opportunity

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
### false negative has a cost of 4 versus a false
### positive's cost of 1
error_cost

### now use costs parameter of the C50()
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### false negatives was reduced at the expense of
### increasing false positives



table(credit$checking_balance)
ggplot(data=credit,
       aes(x=checking_balance)) +
  geom_bar() +
  ggtitle("Histogram of checking balances faceted by default") +
  facet_wrap(~default)


table(credit$savings_balance)

library(ggplot2)
ggplot(data=credit,
       aes(x=savings_balance)) +
  geom_bar() +
  ggtitle("Histogram of savings balances faceted by default") +
  facet_wrap(~default)


summary(credit$months_loan_duration)

ggplot(data=credit,
       aes(x=months_loan_duration)) +
  geom_histogram(binwidth=3) +
  ggtitle("Histogram of loan durations faceted by default") +
  facet_wrap(~default) +
  scale_x_continuous(breaks=seq(0, 75, 3), limits=c(0, 75))

summary(credit$amount)


ggplot(data=credit,
       aes(x=amount)) +
  geom_histogram() +
  scale_x_log10() +
  ggtitle("Histogram of loan amounts faceted by default") +
  facet_wrap(~default)

set.seed(1235) # make our results deterministic (useful for reproducing identical results over multiple runs)
credit_randomised = credit[order(runif(nrow(credit))), ]
# assert that we've not messed up the data:
summary(credit_randomised$amount) # compare this result against earlier

head(credit$amount)

head(credit_randomised$amount)

credit.train = credit_randomised[1:900, ]
credit.test  = credit_randomised[901:1000, ]

# if all's good we should still have 30% default rates in both sets:
round(prop.table(table(credit.train$default))*100)


round(prop.table(table(credit.test$default))*100)


#install.packages("C50")
#library(C50)
# it's as easy as calling C5.0:
credit_model = C5.0(credit.train[-21],     # field 21 is the default, and we don't want that as part of the tree
                    credit.train$default)  # and we tell it what the defaults were (using credit[21] errors)
# We can inspect some high level data in the model:
credit_model


# and some VERY good detail:
summary(credit_model)




credit_pred = predict(credit_model, credit.test)
#install.packages("gmodels")
#library(gmodels)
CrossTable(credit.test$default,
           credit_pred,
           prop.chisq = FALSE,
           prop.c     = FALSE,
           prop.r     = FALSE,
           dnn = c('actual default', 'predicted default'))


credit_boost10_model = C5.0(credit.train[-21], credit.train$default, trials = 20) 
# this is an UPPER limit and the generally accepted best value

credit_boost10_pred = predict(credit_boost10_model, credit.test)
CrossTable(credit.test$default,
           credit_boost10_pred,
           prop.chisq = FALSE,
           prop.c     = FALSE,
           prop.r     = FALSE,
           dnn = c('actual default', 'predicted default'))


error_cost = matrix(c(0, 5, 1, 0), nrow = 2) # this could be a bug in the book
error_cost


credit_cost_model = C5.0(credit.train[-21], credit.train$default, trials=20, costs = error_cost)


credit_cost_pred = predict(credit_cost_model, credit.test)
CrossTable(credit.test$default,
           credit_cost_pred,
           prop.chisq = FALSE,
           prop.c     = FALSE,
           prop.r     = FALSE,
           dnn = c('actual default', 'predicted default'))

