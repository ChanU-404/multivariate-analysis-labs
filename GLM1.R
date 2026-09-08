#************************************************************
#
#				SPAM Filter 1 
#			Graphical Analysis of SPAM 
#
#
#************************************************************


# Load the data, Spam.txt (no headers)
# Descriptions of the variables are in spam.names.txt
# Data pulled from:
# http://archive.ics.uci.edu/ml/datasets/Spambase
# http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names
# Source SPM_Panel.R, PCAplots.R and FactorPlots.R


#*****************************************
# Load Data
#*****************************************
sourcedir <-"/Users/seokhyun/Library/CloudStorage/Dropbox/Korea University/2. Teaching/2026S/IMEN415 Multivariate Analysis/2. R practice/4. GLM"
datadir <- "/Users/seokhyun/Library/CloudStorage/Dropbox/Korea University/2. Teaching/2026S/IMEN415 Multivariate Analysis/2. R practice/data/Spam"

setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

#*****************************************
# Source code
#*****************************************

source("PCAplots.R")
source("FactorPlots.R")

library(ggplot2)
library(ggpubr)
library(psych)

#**************************************************************
#
#				Graphical Analysis
#
#**************************************************************

# Look at the data
dim(spam)
summary(spam)

# Which variable is the response variable?
table(spam$V58)


# What proportion is spam?
sum(spam[,58])/length(spam[,58])

# What do we know about variables 1-48? Variables 49-57?


# What does the SPM tell us? Look at variables 1-10 & 58 
# use png format
dir.create(paste(datadir,"/figures",sep=""))

setwd(paste(datadir,"/figures", sep=""))
png('Image1.png',width = 1920,height = 1080)
pairs.panels(spam[,c(1:10,58)])
dev.off()

png('Image2.png',width = 1920,height = 1080)
pairs.panels(log(spam[,c(1:10,58)]+.00001))
dev.off()

# Look at SPM for variables 48-58.
png('Image3.png',width = 1920,height = 1080)
pairs.panels(log(spam[,c(48:57,58)]+.00001))
dev.off()


# Obtain boxplots with variables 1-9 vs. the response.
# Which variables are more discriminatory?
for(i in 1:9)
{
  assign(paste0("V", i), ggplot(data = spam, aes_string(x=as.factor(spam$V58),y=spam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}

ggarrange(V1,V2,V3,V4,V5,V6,V7,V8,V9,ncol=3,nrow=3)


# Obtain boxplots with variables 49-57 vs. the response.
# Which variable are more discriminatory?
for(i in 49:57)
{
  assign(paste0("V", i), ggplot(data = spam, aes_string(x=as.factor(spam$V58),y=spam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}

ggarrange(V49,V50,V51,V52,V53,V54,V55,V56,V57,ncol=3,nrow=3)


#***************************************************************
#
#		Log of Predictors
#
#***************************************************************

# Repeat the above graphical analysis with a log transform of the
# predictor variables

Lspam <- log(spam[,-58] + .1)
Lspam[,58] <- spam[,58]

#1. Obtain box plots for log transforms of variables 1-9 with variable 58.
png('Image4.png',width = 1920,height = 1080)
for(i in 1:9)
{
  assign(paste0("V", i), ggplot(data = Lspam, aes_string(x=as.factor(Lspam$V58),y=Lspam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}
ggarrange(V1,V2,V3,V4,V5,V6,V7,V8,V9,ncol=3,nrow=3)
dev.off()

#2. Obtain box plots log transforms of variables 49-57 with variable 58.
png('Image5.png',width = 1920,height = 1080)
for(i in 49:57)
{
  assign(paste0("V", i), ggplot(data = Lspam, aes_string(x=as.factor(Lspam$V58),y=Lspam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}
ggarrange(V49,V50,V51,V52,V53,V54,V55,V56,V57,ncol=3,nrow=3)
dev.off()


#****************************************************
#
#		Principal Components
#
#****************************************************


# Obtain the principal components for variables 1-57. 
# Look at the biplot and explain what you see.
spam.pca = princomp(spam[,1:57], cor = T)
biplot(spam.pca)
ggbiplot(spam.pca, varname.size = 5, labels=row(spam)[,1])

# What is the outlier?
barplot(spam.pca$loadings[,2])
summary(spam[,which(spam.pca$loadings[,2] > 0.2)])
spam[1754,which(spam.pca$loadings[,2] > 0.2)]
boxplot(spam$V56)

# remove observation 1754
spam.pca = princomp(spam[-1754,1:57], cor = T)

# Obtain the biplot.fact of your principal components.
# Explain what you see.
biplot.fact(spam.pca, spam[-1754,58])
legend(-30, 10, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue"))

biplot.fact.gg(spam.pca, spam[-1754,58], labels=c("Ham","Spam"))

#3. Obtain the principal components for the log transform of variables 1-57. 
#   Look at the biplot and explain what you see.

Lspam.pc = princomp(Lspam[,1:57], cor = T)
biplot(Lspam.pc)

ggbiplot(Lspam.pc, varname.size = 5, labels=row(Lspam)[,1])

#4. Obtain the biplot.fact of your principal components for the log transformed variables.
#   Explain what you see.

biplot.fact(Lspam.pc, Lspam[,58])
legend(-15, 5, legend = c("Spam", "Ham"), col = c("red", "blue"), pch = c(18, 19))

biplot.fact.gg(Lspam.pc, Lspam[,58], labels=c("Ham","Spam"))

