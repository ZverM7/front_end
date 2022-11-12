###################################################################################################
# Author: xx
# Project: xx
# File: init__.R
# Description: project initialisation
###################################################################################################
# Input: config.R
# Output: front end running logic
###################################################################################################

# install necessary packages --- it would be better to create a function
library(shiny)
library(httr)
library(dplyr)
library(jsonlite)
library(shinythemes)
library(fresh)
library(dipsaus)
library(shinyWidgets)

# sourcing necessary files ----
source("src/config.R")
source("src/app.R")

# running the front-end ---
run_shiny_front(external_ip, port)
