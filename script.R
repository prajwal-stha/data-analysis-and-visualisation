# Setting up the working directory
setwd('~/Desktop/Sandbox/data-analysis-and-visualisation')


# Removing the variables
remove(list=ls())


# Loading Tidyverse library
library(tidyverse)


# Installing corrplot package
install.packages('corrplot')


# Loading corrplot library
library(corrplot)


# Installing Hmisc package
install.packages('Hmisc')


# Loading Hmisc library
library(Hmisc)


# Installing skimr package
install.packages('skimr')


# Loading skimr package
library(skimr)


victims_df <- read_csv('Nepal_Individual_Level_Replication_Data.csv')


skim(victims_df)


str(victims_df)
