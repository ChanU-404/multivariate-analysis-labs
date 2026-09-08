#************************************************************
#
#      Session 15 - Generalized Linear Models
#				            SPAM Filter 
#
#************************************************************

#*****************************
#
# Load the data & source files
#
#*****************************

sourcedir <- "~/Desktop/다변량_실습파일"
datadir <- "~/Desktop/다변량_실습파일"

setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

source("pc.glm.R")
source("PCAplots.R")
source("ROC.R")
source("TestSet.R")

library(ggplot2)
library(ggpubr)
library(ggfortify)

# install.packages("devtools")
# devtools::install_github("goodekat/ggResidpanel")
library(ggResidpanel)

#***********************************************************
#
#	Evaluation of Generalized Linear Models
#
#***********************************************************


#**************************************************
#
# Test and taining sets
#
#**************************************************

# Obtain test and training sets from the spam data
# Make the test set 1/3 of the size of spam
# Use test.set()
set.seed(123)
Spam <- test.set(spam, .33)


# Notice this is a random sample
# so your results will differ from 
# others

# Compare the response variable in 
# the test and training sets to the original
# complete data set

portions <- data.frame(dataset=c("Full","Train","Test",
                                 "Full","Train","Test"),
                       class=c("Spam","Spam","Spam",
                               "Ham","Ham","Ham"),
                       portion=c(sum(spam$V58)/length(spam$V58),
                                 sum(Spam$train$V58)/length(Spam$train$V58),
                                 sum(Spam$test$V58)/length(Spam$test$V58),
                                 1-sum(spam$V58)/length(spam$V58),
                                 1-sum(Spam$train$V58)/length(Spam$train$V58),
                                 1-sum(Spam$test$V58)/length(Spam$test$V58)))

ggplot(portions, aes(x=dataset,y=portion,fill=class)) + geom_bar(stat="identity")


# Predictor Variables
# Look at a random sample of predictors
# and compare training, testing and original data sets

samples = sample(1:58,4)
plots = list()
for(i in samples)
{
  plots[[i]] <- ggplot() + 
    geom_boxplot(data=spam, aes(x=1, y=spam[,i])) + 
    geom_boxplot(data=Spam$train, aes(x=2, y=Spam$train[,i])) +
    geom_boxplot(data=Spam$test, aes(x=3, y=Spam$test[,i])) +
    scale_x_discrete(name="", limits=c(1,2,3), labels=c("Original","Train","Test"))+
      ggtitle(paste("V", i, sep = ""))
}

ggarrange(plots[[samples[1]]],plots[[samples[2]]],
          plots[[samples[3]]],plots[[samples[4]]],ncol=2,nrow=2)


#*****************************
#
# GLM with training data
#
#*****************************

# Obtain a GLM with all predictor variables
# using the training data
# Test the model utility

spam.glm <- glm(V58~., data = Spam$train, family = binomial)

summary(spam.glm)

spam.null <- glm(V58~1, data = Spam$train, family = binomial)

anova(spam.null, spam.glm, test = "Chi")


# Obtain a GLM with the log transform of all predictor variables
# using the training data
# Test the model utility

LSpam.train <- log(Spam$train[,-58] + .1)

LSpam.train$V58 <- Spam$train$V58

Lspam.glm <- glm(V58~., LSpam.train, family = binomial)

summary(Lspam.glm)

Lspam.null <- glm(V58~1, data = LSpam.train, family = binomial)

anova(Lspam.null, Lspam.glm, test = "Chi")


#*****************************************
#	Variable Selection with Stepwise
#****************************************

# main effects stepwise (do at home)

spam.step <- step(spam.glm, trace = 0) 

# How many variables in the step model?
# get this at home

# How many variables?

length(spam.step$coeff) - 1

# Do stepwise with 5 steps

spam.step1 <- step(spam.glm, steps = 5, trace = 0) 


# How many variables in step1? 

length(spam.step1$coeff) - 1

# Do stepwise for the log transform
# of the predictors 
# do it at home till stopping criterion reached.

Lspam.step <- step(Lspam.glm, trace = 0)

# For class do it with 5 steps

Lspam.step1 <- step(Lspam.glm, steps = 5, trace = 0) 

# How many variables in Lspam.step1?

length(Lspam.step1$coeff) - 1

#***********************
# Interactions
#***********************

# Add some interaction terms
# Look at the variables chosen by step
# see if there are any interactions

V1.factor<-spam$V1
V1.factor[which(spam$V1<=median(spam$V1))]<-'low'
V1.factor[which(spam$V1>median(spam$V1))]<-'high'

V38.factor<-spam$V38
V38.factor[which(spam$V38<=median(spam$V38))]<-'low'
V38.factor[which(spam$V38>median(spam$V38))]<-'high'

with(spam, interaction.plot(x.factor=V38.factor, 
                            trace.factor=V1.factor, response=V58, 
                            type="b",ylab="Percent Spam", 
                            main="Interaction Plot",pch=c(1,19)))

ggplot() +
  aes(x=V38.factor, y=spam$V58, group=V1.factor, color=V1.factor) +
  stat_summary(fun = mean, geom="point") + 
  stat_summary(fun = mean, geom="line") + 
  xlab("V38") + ylab("Percent Spam") + 
  labs(color="V1")

#***********************************************************
#
#	GLM Principal Components Regression
#
#***********************************************************

# Here is an example of how to 
# obtain a GLM with principal components
# accounting for 90% of the variance
# using the training data.

spam.pca <- princomp(Spam$train[,-58], cor = T)

spampca.glm90 <- pc.glm(spam.pca, 90, Spam$train[,58])

# Do a model utility test for model that uses 
# PC that account for 90% of the variance

spampca.null <- pc.null(spam.pca, 90, Spam$train[,58])

anova(spampca.null, spampca.glm90, test = "Chi")


# Obtain a GLM with principal components
# accounting for 98% of the variance.
# Use the training data

spampca.glm98 <- pc.glm(spam.pca, 98, Spam$train[,58])

# Do a model utility test starting with pc.null()

spampc.null <- pc.null(spam.pca, 98, Spam$train[,58])

anova(spampc.null, spampca.glm98, test = "Chi")

# Do a partial likelihood test between the 
# 98 % and 90% models

anova(spampca.glm90, spampca.glm98, test = "Chi")


# Compare the AIC for your PC models

AIC(spam.glm)
AIC(spam.step1)
AIC(Lspam.glm)
AIC(Lspam.step1)
AIC(spampca.glm98)
AIC(spampca.glm90)

# Do the comparisons with BIC

BIC(spam.glm)
BIC(spam.step1)
BIC(Lspam.glm)
BIC(Lspam.step1)
BIC(spampca.glm98)
BIC(spampca.glm90)

#*****************************
#
# Diagnostic plots
#
#*****************************

# Get diagnostic plots and comment (for brevity, just comparing spam.glm and Lspam.glm here)

autoplot(spam.glm)

autoplot(Lspam.glm)

resid_compare(list(spam.glm, Lspam.glm))

# only Cook's distance diagnostics

autoplot(spam.glm, which = 4, nrow=1, ncol=1)

autoplot(Lspam.glm, which = 4, nrow=1, ncol=1)

resid_compare(list(spam.glm, Lspam.glm), plots="cookd")


# Influential points
# Find the most influential points
# in both spam.glm and Lspam.glm

which(cooks.distance(spam.glm) > 0.5)

which(cooks.distance(Lspam.glm) > 0.5)

spam[which(cooks.distance(spam.glm) > 0.5),]

summary(spam)



#*****************************
#
# Predictions with test data
#
#*****************************


# Use the test data to obtain predictions for the 
# GLM with all predictor variables


spam.pred <- predict(spam.glm, type = "response", newdata = Spam$test)


# Use the test data to obtain predictions for the  
# GLM with the log transform of all predictor variables

Lspam.test <- log(Spam$test[,-58] +.1)

Lspam.pred <- predict(Lspam.glm, type = "response", newdata = Lspam.test)


# Use the test data to obtain predictions for the  
# GLM with principal components
# accounting for 98% of the variance.

spampca.pred <- predict.pc.glm(spampca.glm98, spam.pca, Spam$test[,1:57] )


# Obtain predictions from the stepwise models

step.pred1 <- predict(spam.step1,  type = "response", newdata = Spam$test)

Lstep.pred1 <- predict(Lspam.step1,  type = "response", newdata = Lspam.test)



#*****************************
#
#  Confusion Matrices
#
#*****************************

# Set the decision threshold to 0.5 
# compute Confusion Matrices for each model

score.table(spam.pred, Spam$test[,58], .5)

score.table(Lspam.pred, Spam$test[,58], .5)

score.table(spampca.pred, Spam$test[,58], .5)

score.table(step.pred1, Spam$test[,58], .5)

score.table(Lstep.pred1, Spam$test[,58], .5)

# Which model would you choose for your filter?



#*****************************
#
#   ROC Curves
#
#*****************************


# Produce an ROC curve with all models


plot.roc(spam.pred, Spam$test[,58], main = "ROC Curve - SPAM Filter", col = "blue")

lines.roc(Lspam.pred, Spam$test[,58], col = "orange")

lines.roc(spampca.pred, Spam$test[,58], col = "green")

lines.roc(step.pred1, Spam$test[,58], col = "red")

lines.roc(Lstep.pred1, Spam$test[,58], col = "cyan")


legend(.6, .57, legend = c("Main Effects", "Log", "PCR.", "Step", "Logstep"), col = c("blue", "orange", "green", "red", "cyan"), lwd = 1)

# with ggplot
roc.plot.gg <- plot.roc.gg(spam.pred, Spam$test[,58], "Main Effects")
roc.plot.gg

# add other models
roc.plot.gg <- lines.roc.gg(roc.plot.gg, Lspam.pred, Spam$test[,58], "Log")
roc.plot.gg <- lines.roc.gg(roc.plot.gg, spampca.pred, Spam$test[,58], "PCR")
roc.plot.gg <- lines.roc.gg(roc.plot.gg, step.pred1, Spam$test[,58], "Step")
roc.plot.gg <- lines.roc.gg(roc.plot.gg, Lstep.pred1, Spam$test[,58], "Logstep")
roc.plot.gg


# which model would you choose for your filter?

