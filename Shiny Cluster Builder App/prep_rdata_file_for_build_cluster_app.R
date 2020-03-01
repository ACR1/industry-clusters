# This R script assembles prepared data files and variables for the
# "Build a Cluster" Shiny App.

# It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop
# by A.C. Repella. Extensively commented to support novice R-shiny users.

# Code for the App (this file only) is released as CC-BY-SA 4.0.
# Data files are not included in this license and any content that
# crosswalks or otherwise aligns data with other sources (such as
# Emsi, U.S. Cluster Mapping, or the other cluster systems bundled here)
# can be considered the property of the organization that produced it.


# LOAD LIBRARIES
# if you don't have these installed, you'll need to do so:
# by typing: install.packages("library name") at the console
library(tidyverse) # overkill here, I think I'm just using dplyr and readr
library(readxl) # reads excel files
library(rlist)


## SET WORKING DIRECTORY TO THIS PATH

# if in rstudio doing an interactive run from the IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# using R proper, running from command line or when sourcing this file from another file.
# setwd(getSrcDirectory()[1])

# This file expects all file paths below to be accurate. There are
# no quality checks in this script.


## CLEAR GLOBAL ENVIROMENT AND DEFINE OUTPUT FILE NAME
rm(list = ls())


# this is the name of the output file.
# this script saves the entire global environment to a .Rdata file
# or else you'll save whatever stuff is loaded in the environment already
output_file <- paste0("build_a_cluster_data_", Sys.Date(), ".Rdata")

# INPUT PATH FOR FILES THAT ARE USED BY MULTIPLE APPS
shared_input_path <- "../Shared_Files/"

## SET SOME APP VARIABLES

# this is the unlock code used in the app.
# set it to whatever string you want.
# you'll need to uncomment some code in the app to limit access
# to content
unlock_code <- "UNLOCK CODE"

# "Nice Names" for your regions. we use 2. you can modify code to use more
# these regions need to be identically named in 2 different scripts
# (see imported data secton below)
region1 <- "Small Region"
region2 <- "Medium Region"

regions <- c(region1, region2)

# LOAD DATA PREPPED IN ANOTHER SCRIPT
# subcluster files. they are prepped in a different script
#     (provided as "make_cluster_subcluster_tables_from_emsi_industry_table.R")
# our2 example files are 100% fake (real subclusters, synthetic numbers)
# we use two different subcluster geographies in this app:
subclusters1 <- read_csv(paste0(
  shared_input_path, 
  "Small.Region-by-subcluster-final-2019.3-2019-11-27.csv"))

subclusters2 <- read_csv(paste0(
  shared_input_path,
  "Medium.Region-by-subcluster-final-2019.3-2019-11-27.csv"
))


# what's the current data year ?
# (max year to display -- needs to be in files above. no logic checks here.
the_year <- 2018

subclusters <- rbind(subclusters1, subclusters2)

# this CSV contains total jobs for any regions in the subcluster tables and the US
# its used to recalculate Location Quotient on the fly.
# prepped in the same script as subclusters
#     (provided as "make_cluster_subcluster_tables_from_emsi_industry_table.R")
totaljobs <- read_csv(paste0(shared_input_path, "total_jobs.csv"))



# these are the scale factors used in the bubble chart in the app
# if you don't like the chart scaling of bubbles --- change it by change these values
# see the app for the scaling function.
minjobs <- 1
maxjobs <- max(subclusters$jobs) * 2


# this is our short list of clusters our members initially were particularly interested in
# based upon output from Emsi's default "Cluster Identification' tool.
# you should replace it with the clusters you are particularly interested in
# see file for formatting requirements
# it can be copy and pasted from the appropriate columns in the crosswalk file.
shortlist <- read_csv(paste0(shared_input_path, "shortlist.csv"))



# a list of companies crosswalked to clusters.
# we assembled ours from a variety of sources, including our own BRE data.
# you can use library accessible databases (Reference USA? D&B?) or the Business Table in Emsi
# this example file is totally fake (you don't need to know our companies...)
# we've replaced company names with some famous artists and scientists
# (thanks Wikipedia lists).
# we've replaced the county where the company is based with a random letter
# on our list, we selected companies that represented larger orgs + niches
# for subclusters with more Naics or more diverse companies across the naics,
companies <- read_excel(paste0(
  shared_input_path,
  "representative-companies.xlsx"
))



# This is our crosswalk file for matching Emsi NAICS to U.S. Cluster Mapping NAICS.
# we renamed a couple of subclusters subtly because there are duplicate
# subcluster names across a few Clusters (see "metals"!)
xwalk <- read_xlsx(paste0(
  shared_input_path,
  "Emsi-NAICS-to-Porter-Cluster.xlsx"
))


# create vectors that are basically lists of the subclusters we want.
# this requires using a modified subcluster list (like we did above) so that
# each subcluster is a unique name and can be seperated from their Cluster
# without issue
priority.subclusters.list <- split(shortlist$Subcluster, shortlist$Cluster)
x <- subclusters2[which(subclusters2$year == 2018), ]
all.subclusters.list <- split(x$SubclusterName, x$Cluster)
x <- xwalk[which(xwalk$TradedLocal == "Traded"), ]
traded.subclusters.list <- split(x$SubclusterName, x$Cluster)
x <- xwalk[which(!(xwalk$TradedLocal == "Traded")), ]
local.subclusters.list <- split(x$SubclusterName, x$Cluster)

# this removes variables that are no longer needed.
rm(subclusters1, subclusters2, x, shortlist)

# this sets the initial choice list in the app.
# we set it to the priority subclusters, because that's
# the default selection in the app
choice_list <- priority.subclusters.list

# Data needs to be formatted as in the example excel file, but you can replace this with whatever systems you'd like to use for comparison
# FYI - R doesn't always like it when you try to open a file that is open in Excel,
# so close the XLSX file before attempting to run this script, or else you will get
# errors.
AL_clusters_df <- read_excel(paste0(
  shared_input_path,"Other_Cluster_Definitions.xlsx"),
  sheet = "AL_Clusters", col_types = c(
    "text",
    "text", "text", "text", "text",
    "text", "text", "text"
  )
)

MDEDC_clusters_df <- read_excel(paste0(
  shared_input_path,"Other_Cluster_Definitions.xlsx"),
  sheet = "MetroDenver", col_types = c(
    "text",
    "text", "text", "text", "text",
    "text", "text", "text"
  )
)


FC_clusters_df <- read_excel(paste0(
  shared_input_path,"Other_Cluster_Definitions.xlsx"),
  sheet = "FC_Clusters", col_types = c(
    "text",
    "text", "text", "text", "text",
    "text", "text", "text"
  )
)


OEDIT_clusters_df <- read_excel(paste0(
  shared_input_path,"Other_Cluster_Definitions.xlsx"),
  sheet = "OEDIT", col_types = c(
    "text",
    "text", "text", "text", "text",
    "text", "text", "text"
  )
)



# this is used in a chart. It's a medium gray. You can change the color with any HEX color
linecolor <- "#404040"
# this is used in the bubble chart. Bubble Colors for each region.
color_choices <- c('#E31737', '#787878')

# CHUNKS OF STATIC TEXT SHOWN IN THE APP --
## Any of this text can be styled with standard HTML.

# this is disclaimer text shown near the "Representative Companies" table in the app.
rep_cos_text <- c("These companies are representative of organizations with a presence in the region and an establishment that falls within this subcluster. This is not a comprehensive list. It is here to give concrete examples of the companies or types of companies that would appear in this particular cluster definition. Subclusters with very few establishments in the region are excluded, ")

# this is descriptive text below the bubble chart
# same deal, styles with HTML
bubble_chart_text <- c('This chart is a "Perdue Style" Bubble Chart that shows Location Quotient on the vertical axis and 5-year job growth on the horizontal axis. The size of the bubble represents total employment as of the data year. <br/><br/>The <b>top right</b> quadrant is typically viewed as a place of strength, a combination of job growth and regional specialization. <br />The <b>bottom right</b> indicates cluster that have strong job growth, but are not regional specializations (low LQ). <br />The <b>top left</b> indicates a decline in job growth, but a strong regional specialization.</br />The <b> bottom left </b> indicates a low specialization and job declines.<br/>                      The time series is presented because some clusters do show particular sensitivity to business cycles. <br /> ')


axis_font_style <- list(
  family = "'Roboto Condensed', 'Arial Narrow', sans-serif",
  size = 11,
  color = "#404040"
)

## THIS SAVES ALL VARIABLES IN THE GLOBAL ENVIRONMENT TO A FILE

save.image(output_file)
