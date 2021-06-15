#######################################
# Natural Language for Social Science #
#        Prof Michael Yeomans         #
#######################################

library(tidyverse) # Contains MANY useful functions
library(quanteda)  # many useful text analysis tools
library(politeness) # structural features from text
library(doc2concrete) # Contains the ngramTokens function - mostly a wrapper around quanteda tools
library(glmnet) # A simple machine learning model (the LASSO)
library(ggrepel) # useful for plotting later on
library(stm)  # High-performance topic modelling
library(syuzhet) # a benchmark for sentiment detection
library(sentimentr) # a benchmark for sentiment detection
library(doMC) # to speed up some code with parallel processing


# source("dataload.R") # to trim yelp data from raw JSON files... don't run, it's slow! (but if you're curious.... )

# These are the full datasets - don't load them for now! But they will come in handy later
# busses<-readRDS("data/businessset.RDS") # 35,086 restaurants from Yelp
# reviews<-bind_rows(readRDS("data/reviewset1.RDS"), # 584,137 restaurant reviews
#                    readRDS("data/reviewset2.RDS")) # it's big! had to split in two for github
#
# There are way too many examples for us... Let's trim it down
# This is the only city I've lived in from the data (no London or Toronto, alas!)
# We'll also only look at mid-range restaurants (2/4 on the price range scale)
#
# bus_small<-busses %>%
#   filter(city=="Cambridge" & RestaurantsPriceRange2==2)
# rev_small <- reviews %>%
#   filter(business_id%in%bus_tiny$business_id)

# Save data as we go
# saveRDS(rev_small,file="data/rev_small.RDS")
# saveRDS(bus_small,file="data/bus_small.RDS")

# Let's load it from memory
rev_small<-readRDS("data/rev_small.RDS")
bus_small<-readRDS("data/bus_small.RDS")


source("basicNLP.R")      # ngrams, model training, dictionaries
source("structuralNLP.R") # topic models, vectors

source("politeness.R") # politeness example

##################### Notes for Project Ideas 
#
#
## Main Effects
# Do features of reviews differ by restaurant type?
#   (price range, kid friendly, hipster, casual, trendy, breakfast…)
# 
## Interaction Effects
# Do different features predict star ratings within different restaurant types?
#
#
##### External Datasets to merge based on location
# 
# Convert zip codes to counties
# 
# https://www.unitedstateszipcodes.org/zip-code-database/
#   
#   education/economic data
# https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
#   
#   Or political data
# https://electionlab.mit.edu/data
# 
