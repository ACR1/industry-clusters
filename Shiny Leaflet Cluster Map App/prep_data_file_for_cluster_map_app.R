## This script is used to assemble an R.Data file that can be included
## with the "Cluster Map" Shiny App
## this doesn't handle any significant data prep, just the final packaging

## This R file has been released under CC-BY-SA 4.0. any data files assembled
## under implementation of this code are not included in that release. This
## is packaged with files for example only, so that the code will compile
## when downloaded. Any content using U.S. Cluster Mapping terminology and
## definitions is the property of the U.S. Cluster Mapping Project. The author
## of this code has no relationship to the U.S. Cluster Mapping Project.


# this is extensively commented specifically to help Economic Developers
# use R and Shiny for data visualization


# SET WORKING DIRECTORY TO THIS PATH ---------------------

# if in rstudio doing an interactive run from the IDE...
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# using R proper, running from command line or when sourcing this file...
# setwd(getSrcDirectory()[1])

# This file expects all file paths below to be accurate. There are
# no quality checks in this script.



## CLEAR GLOBAL ENVIROMENT ---------------------
rm(list = ls())

# clear the Global Environment.
# this script just loads up all the data that the app needs and saves
# everything in the global environment to an Rdata (binary, smallish) file.


# LOAD libraries ---------------------
# if you don't have these installed, you'll need to do so:
# by typing: install.packages("library name") at the console
library(readxl) # reads excel files
library(tidyverse)
library(magrittr) # %<>%


# SET OUTPUT FILENAME ---------------------
# This will be sourced by the app.R file
out_file <- paste0("cluster_map_data_", Sys.Date(), ".Rdata")

## SET OPTIONAL UNLOCK CODE ---------------------
# if you want to publish this on Shinyapps.io,
# but won't be authenticating users, you
# may want to keep casual visitors from accessing your app by creating an
# "unlock code" that you can share with all of your users.
unlock_code <- "YOUR_OWN_PASSWORD"

# LIST U.S. CLUSTER MAPPING DEFINITIONS TO EXCLUDE FROM YOUR APP-------------
# This is a list of clusters that are too small in our region to map
# we exclude them from the map on purpose.
# each item needs to must match (case sensitive) the official definitions in the cluster lists.
cluster_exclusions <- c(
  "Fishing and Fishing Products", "Footwear", "Forestry",
  "Coal Mining", "Jewelry and Precious Metals",
  "Metal Mining", "Textile Manufacturing",
  "Paper and Packaging", "Music and Sound Recording",
  "Tobacco", "Water Transportation"
)

# VARIOUS LISTS OF CLUSTERS -------------------------
# These form selection options in the app
# LIST any priority clusters
# our group defined clusters that were "priorities to learn about"
# see file for expected format
priority.clusters.list <- unname(unlist(
  read_excel("../Shared_Files/lists_of_clusters.xlsx",
              sheet = "priority")[, 1]))

# a list of traded (not local) clusters, check the file for format
traded.clusters.list <- unname(unlist(
  read_excel("../Shared_Files/lists_of_clusters.xlsx",
              sheet = "traded")[, 1]))
traded.clusters.list <- traded.clusters.list[which(!(traded.clusters.list %in%
  cluster_exclusions))]


# a list of all clusters
all.clusters.list <- unname(unlist(
  read_excel("../Shared_Files/lists_of_clusters.xlsx", sheet = "all")[, 1]))
all.clusters.list <- all.clusters.list[which(!(all.clusters.list %in% cluster_exclusions))]


# initial cluster choice in app
choice_list <- priority.clusters.list

# initialize a variable used in the app.
linked.clusters.list <- c(" ")

# LINKED CLUSTERS ---------------------
# this is an excel file with the cluster links defined on
# the 'related cluster' page at U.S. Cluster Mapping Project
# generated from "Full Portfolio View" at https://clustermapping.us/cluster
clusterLinks <- read_xlsx("../Shared_Files/ClusterLinkage.xlsx")
clusterLinks.list <- unique(clusterLinks$`Core Cluster`)

# CLUSTER POINTS TO MAP ---------------------
# this file represents point data of aggregated business establishments
# this is the output file from the "Create_Cluster_Map_Data.RMD file
# that steps through the process
cluster_points <- read_csv("cluster_gravity_points.csv")


# this is a message that appears below the map in the app.
# it describes how the map data was aggregated.
# it's formatted using HTML.
map_text <- paste0(
  "<br /><br /><p>",
  'Map shows "centers of gravity" for clusters in the region.',
  "</p><p>",
  "Click on any point to see details on employees + ",
  "establishments, as well as radius included in each point.",
  "</p><p>",
  "Many small clusters indicate more small businesses ",
  "within the cluster.",
  "</p><p>",
  "One or two single clusters means that large employers",
  "dominate the total employment.",
  "</p><p>",
  "Clusters are calculated based upon the employment center by",
  " Census tract.</p>",
  '<p>Each cluster center of gravity "attracts" additional Census tracts until it meets publication criteria.</p>',
  "<p>The map highlights geographic centers for multiple businesses, not individual worksites.</p>"
)


# this text explains what linked clusters are and shares a link
# to learn more. appears at the base of the left-side menu
# again, formatted using HTML
linked_cluster_text <- paste0(
  "<small><i>linked, or related, clusters are",
  "traded clusters that have close connections ",
  "which may span co-location, shared workforce,",
  "shared supply chain, or other interactions.</i>",
  "</small><small><i>",
  "A map showing these relationships can be viewed",
  ' here: <a href="https://clustermapping.us/',
  'cluster#related-clusters" target="_blank"> U.S.',
  " Cluster Mapping, Related Clusters </a>.",
  "</i> </small>"
)

# this text describes the clusters that are excluded from the app
exclusion_text <- paste0(
  "This map excludes these clusters due to their limited",
  " importance in our region: ",
  paste(cluster_exclusions, collapse = ", "), "."
)

disclaimer_text <- paste0(
  "The code for this Shiny App has been released under",
  " CC-BY-SA 4.0. Data, text, images and cluster",
  " definitions displayed through implementation of",
  " this app is not included under that agreement."
)



# this adds a field to the cluster points that contains descriptive
# text to the pop-up window when a point is clicked.
# it uses HTML to format display text in Leaflet map. <b> == bold, <br /> == line break
cluster_points$content <- paste0(
  "<b>", cluster_points$Cluster,
  "</b><br />Employed: ",
  cluster_points$nice_emp_bin,
  "<br />Establishments: ",
  cluster_points$estabs
)


# WRITE ENVIROMENT TO FILE FOR APP -------------
save.image(file = out_file)
