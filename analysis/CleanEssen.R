#======================================================================================================
# Clean Essen Abstracted Features
#--------------------------------------------------
# This script takes the 38 features abstracted from FANTASTIC on the Essen Corpus and cleans it up for
# the classification script
#--------------------------------------------------
# Written by : David John Baker
# Last Modified : Dec 22nd 2017
#======================================================================================================
# Import Libraries, Load Data
library(data.table)
library(stringr)
setwd("data/")
essen <- fread("EssenFeatures.csv")
attach(essen)
#--------------------------------------------------
# Create New Variable that is only country
unique(file.id)
essen$country <- str_replace_all(string = file.id, pattern = "[:digit:]", replacement = "" )
table(essen$country)
#--------------------------------------------------
# Need to try to combine groups based on region 
# According to http://kern.humdrum.org/cgi-bin/browse?l=/
# We have  
# Africa : just one arabica1 
# America : Mexico, Misc, USA 
# Asia :
# EsAc :
# Europa :
#======================================================================================================
