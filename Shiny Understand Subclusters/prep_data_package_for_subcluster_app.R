# libs for acccessing data

library(readxl)
library(tidyverse)
library(magrittr)



## SET WORKING DIRECTORY TO THIS PATH

# if in rstudio doing an interactive run from the IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# using R proper, running from command line or when sourcing this file from another file.
# setwd(getSrcDirectory()[1])

# This file expects all file paths below to be accurate. There are
# no quality checks in this script.


## CLEAR GLOBAL ENVIROMENT AND DEFINE OUTPUT FILE NAME
rm(list = ls())

output_file_name <- paste0("understand_subclusters_app_data_", Sys.Date(), ".Rdata")

# INPUT PATH FOR FILES THAT ARE USED BY MULTIPLE APPS
shared_input_path <- "../Shared_Files/"

# We use three regions, so charts need 3 distinct colors
THREE_COLORS <- c("#787878", "#E31737", "#989898")

# these are your region names
region1 <- "Small Region"
region2 <- "Medium Region"
region3 <- "Large Region"

region_list <- c(region1, region2, region3)

# OPTIONAL set unlock code for app.
unlock_code <- "YOUR_OWN_UNLOCK_CODE"


companies <- read_xlsx(paste0(shared_input_path, "representative-companies.xlsx"))
xwalk <- read_xlsx(paste0(shared_input_path, "Emsi-NAICS-to-Porter-Cluster.xlsx"))



# This is a list of clusters that do not appear in any significant way in our region
# it must match the official definitions in the cluster lists.
cluster_exclusions <- c(
  "Fishing and Fishing Products", "Forestry",
  "Coal Mining", "Jewelry and Precious Metals",
  "Metal Mining", "Textile Manufacturing",
  "Paper and Packaging", "Music and Sound Recording",
  "Tobacco", "Water Transportation"
)

# these are the cluster lists (vectors)
# priority -- those previously described as "priorities to learn about" by
# our group
priority.clusters.list <- unname(unlist(
  read_excel(paste0(shared_input_path, "lists_of_clusters.xlsx"),
    sheet = "priority"
  )[, 1]
))

# all traded clusters
traded.clusters.list <- unname(unlist(
  read_excel(paste0(shared_input_path, "lists_of_clusters.xlsx"),
    sheet = "traded"
  )[, 1]
))
traded.clusters.list <- traded.clusters.list[
  which(!(traded.clusters.list %in% cluster_exclusions))
]

# all local and traded
all.clusters.list <- unname(unlist(read_excel(paste0(shared_input_path, "lists_of_clusters.xlsx"),
  sheet = "all"
)[, 1]))
all.clusters.list <- all.clusters.list[which(!(
  all.clusters.list %in% cluster_exclusions))]


# This should be replaced with output from
# script: combine_multiple_shift_share_tables.R
# these example/placeholder values are randomly generated and truly make no sense
# (components will not sum to 100%)
# file is named in the script mentioned, so update each input file name here to match.
# these would be different files where each sublcuster shift share file
# matched one of the custom geographies

shiftshare_region3 <- read_csv("FAKE_SHIFT_SHARE_DATA_TABLE.csv")
shiftshare_region3$region <- region3

shiftshare_region2 <- read_csv("FAKE_SHIFT_SHARE_DATA_TABLE.csv")
shiftshare_region2$region <- region2

shiftshare_region1 <- read_csv("FAKE_SHIFT_SHARE_DATA_TABLE.csv")
shiftshare_region1$region <- region1

shiftshare <- rbind(shiftshare_region1, shiftshare_region2, shiftshare_region3)


# initial cluster choice in app
choice_list <- priority.clusters.list

# these are the regions assembled in
# make_cluster_subcluster_tables_from_emsi_table.R
subclusters_r1 <- read_csv(
  paste0(
    shared_input_path,
    "Small.Region-by-subcluster-final-2019.3-2019-12-04.csv"
  )
)
subclusters_r1$region <- region1
subclusters_r2 <- read_csv(
  paste0(
    shared_input_path,
    "Medium.Region-by-subcluster-final-2019.3-2019-12-04.csv"
  )
)
subclusters_r2$region <- region2
subclusters_r3 <- read_csv(
  paste0(
    shared_input_path,
    "Large.Region-by-subcluster-final-2019.3-2019-12-04.csv"
  )
)
subclusters_r3$region <- region3



all_subclusters <- rbind(subclusters_r1, subclusters_r2, subclusters_r3)
all_subclusters %<>%
  select(c(
    "Cluster", "SubclusterName", "year", "jobs.pct.change", "LQ",
    "jobs", "average_earnings", "jobs.change", "region"
  ))

all_subclusters %<>% mutate(jobs.pct.change = as.numeric(jobs.pct.change))

all_subclusters %<>%
  mutate_at(vars(jobs, average_earnings, jobs.change), ~ (round(., 0)))

all_subclusters %<>%
  mutate_at(vars(jobs.pct.change), ~ (round(. * 100, 1)))


all_subclusters %<>%
  mutate_at(vars(LQ), ~ (round(., 2)))


subclusters.list <- as.character(unique(
  all_subclusters_simple$SubclusterName[which(
    all_subclusters_simple$Cluster == priority.clusters.list[1]
  )]
))

region.list <- as.character(unique(all_subclusters_simple$region))




all_subclusters %<>%
  left_join(shiftshare[, c(
    "Cluster", "SubclusterName", "region",
    "Competitive.Effect", "end.year"
  )],
  by = c(
    "Cluster" = "Cluster",
    "SubclusterName" = "SubclusterName",
    "region" = "region",
    "year" = "end.year"
  )
  )

all_subclusters %<>%
  mutate(Competitive.Effect = round(
    all_subclusters$Competitive.Effect,
    2
  ))


# Introductory Text (HTML FORMATTED) for each tab in mainPanel tabSet
industries_text <- "<h1>NAICS Industries Included in Cluster Definition</h1>"
subclusters_text <- '<h1>Subcluster Metrics</h1><div style="margin:20px;">Explore subcluster metrics in chart/table. Use the date slider (above) or "play through" a timeseries.</div>'
companies_text <- '<h1>Representative Companies</h1><div style="margin:20px;">Concrete examples of 5 county region businesses that would be classifed in industries that fall within subclusters. Subclusters with low/no representation in the 5 county region are generally excluded.</div>'



save.image(output_file_name)
