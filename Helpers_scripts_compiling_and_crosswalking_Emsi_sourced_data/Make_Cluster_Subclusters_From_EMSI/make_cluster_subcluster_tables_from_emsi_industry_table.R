## This R script takes a specifically formatted Emsi data table and
## crosswalks it to Emsi versions of the U.S. Cluster Mapping Industry
## Cluster definitions for use in other scripts packaged in this project.
## This file just takes a compiled industry table (see xlsx for format)
## it calculates summaries by cluster and subcluster for an arbitrary number of
## Emsi industry tables.

## It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop
## by A.C. Repella. Extensively commented to support novice R and Shiny users.

## Code for this script only is released as CC-BY-SA 4.0.
## Data files are not included in this license and any content that
## crosswalks or otherwise aligns data with other sources (such as
## Emsi, U.S. Cluster Mapping, or others)
## can be considered the property of the organization that produced it.



# LOAD LIBRARIES
# any libraries not on system will need to be added with
# command: 'install.packages("library name")'
library(tidyverse) # general data tidying summarizing
library(readxl) # reads XLSX files
library(stringr) # for formatting strings nicely
library(rlist) # for append

## SET WORKING DIRECTORY TO THIS PATH

# if in rstudio doing an interactive run from the IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# using R proper, running from command line or when sourcing this file from another file.
# setwd(getSrcDirectory()[1])


# This file expects all file paths below to be accurate. There are
# no quality checks in this script.

# output files can be dropped into the shared file folder 
# (if you intend to keep the current directory structure of this example)
# currently writes up one higher in directory structure
output.path <- "../"

# clear environment
rm(list = ls())

# SET REGION NAMES
# these are nice names for your regions you want to assemble data for
# these definitions are used throughout our project.
# we used 3 nested regions for our 2 county, 5 county, and BEA (2004) 45 county regions
# Names have been changed below to reflect generic data used here.
# you can use as many regions as you'd like, just stack up the variables here

region1 <- "Small Region"
region2 <- "Medium Region"
region3 <- "Large Region"

# gather all your regions into a vector of region names
regions <- c(region1, region2, region3)
n_regions <- length(regions)

## IMPORT DATA FILES

# These are Emsi Industry Tables. exported as CSV from Emsi.
# CSV as export is important for 2 reasons
#      1) all values that are <10 or insf. data are 0 in Csv
#      2) no total row is present
#              (this script assumes each row is unique NAICS, not total)
# Data is expected to be formatted in identically
# named and formatted columns as in example files
# ALL NUMERIC VALUES IN THESE EXAMPLE FILES ARE FAKE.
# 'emsi_table1' better be the same region as 'region1', and so on....
emsi_table1 <- read_csv("6d_NAICS_Industry_Table_export_for_Small_Region.csv")
emsi_table2 <- read_csv("6d_NAICS_Industry_Table_export_for_Medium_Region.csv")
emsi_table3 <- read_csv("6d_NAICS_Industry_Table_export_for_Large_Region.csv")

# drop them into a list for easy iteration.
# ORDER MUST match the regions vector defined above
emsi_tables <- list(emsi_table1, emsi_table2, emsi_table3)
n_tables <- length(emsi_tables)

# the regions and datatables need to match.
if (n_regions != n_tables) {
  stop("number of regions and number of data tables don't match")
}


# THIS CROSSWALKS EMSI NAICS to U.S. Cluster Mapping Project Cluster/Subcluster defs
xwalk <- read_excel("Emsi-NAICS-to-Porter-Cluster.xlsx")



# puts the current date on file names
DATE <- Sys.Date()

# this is the number of years for multi-year averages. it's hardcoded into
# variable names so if you are using a different period, check/change the
# var names below where cluster and subcluster dataframes are created.
n_years_period <- 5

all_total_jobs <- list()

# LOOP through each table to summarize by cluster and subcluster, write to file.
for (i in (1:n_tables)) {
  region_name <- regions[i]
  emsi_table <- emsi_tables[[i]]
  # FORCE NAICS TO CHAR
  emsi_table$NAICS <- as.character(emsi_table$NAICS)

  colnames(emsi_table) <- make.names(colnames(emsi_table))

  # This defines the unique filenames for each regions summarized data
  output_cluster_filename <- paste0(
    make.names(regions[i]),
    "-cluster-final-2019.3-", DATE, ".csv"
  )
  output_subcluster_filename <- paste0(
    make.names(regions[i]),
    "-by-subcluster-final-2019.3-", DATE, ".csv"
  )

  ########################################################
  ## PROCESS THE DATA


  # CONVERT TO LONG FORMAT
  x <- emsi_table %>% gather(key = "Variable.Name", value = "value", -NAICS, -Description)

  # EXTRACT VARIABLE YEAR from Variable.Name
  x$year <- str_match(x$Variable.Name, "(\\d\\d\\d\\d)")
  x$year <- x$year[, 1] # str_match returned 2 items per row. we want the first

  # Extract actual Variable name (no year) from Variable.Name
  x$Variable.Name2 <- sub("(\\d\\d\\d\\d)", "", x$Variable.Name, perl = TRUE)

  # do some more substituting to get meaningful standard names
  x$Variable.Name2 <- ifelse(grepl("Earnings", x$Variable.Name2), "Earnings", x$Variable.Name2)
  x$Variable.Name2 <- ifelse(grepl("National", x$Variable.Name2), "National.Jobs", x$Variable.Name2)
  x$Variable.Name2 <- ifelse(grepl("X.Jobs", x$Variable.Name2), "Jobs", x$Variable.Name2)
  x$Variable.Name2 <- ifelse(grepl("Payrolled", x$Variable.Name2), "Establishments", x$Variable.Name2)

  # grab only the columns  needed
  x <- x[, c("NAICS", "Description", "year", "Variable.Name2", "value")]


  # reformat from long to wide
  emsi_table_tidy <- spread(x, Variable.Name2, value)


  ## Calculate aggregate variables
  emsi_table_tidy <- group_by(emsi_table_tidy, NAICS) %>%
    mutate(job_change_5yr = Jobs - lag(Jobs, n_years_period))

  emsi_table_tidy <- group_by(emsi_table_tidy, NAICS) %>%
    mutate(national_change_5yr = National.Jobs - lag(National.Jobs, n_years_period))

  # MINIMUM YEAR For Data is really the minimum year for an n_years_period summary
  min_year <- min(as.numeric(emsi_table_tidy$year)) + n_years_period

  emsi_table_tidy <- subset(emsi_table_tidy, emsi_table_tidy$year >= min_year)

  emsi_table_tidy$total_earnings <- emsi_table_tidy$Earnings * emsi_table_tidy$Jobs

  # avoid doing math on suppressed values by zeroing out
  # "earnings calc" jobs when earnings is NA
  emsi_table_tidy$EarningsJobs <- ifelse(is.na(emsi_table_tidy$total_earnings),
    0, emsi_table_tidy$Jobs
  )


  ## calculate total jobs as sum of all industry jobs
  ## FYI: these totals may have small differences from the Emsi published totals
  ## that appear online or in the Excel file, this is due to values <10 being
  ## forced to 0 in CSV files, but those values are included in table totals.
  ## if this creates an unusual discrepancy in your results for LQ calculations..
  ## you might be using way too small of a region for cluster analysis.
  ## you can check to see if this is particularly different from your actual total
  ## estimates by checking values in this data frame (which is exported)
  ## vs. what you see in Emsi for your totals.

  total_jobs <- emsi_table_tidy %>%
    group_by(year) %>%
    summarize(
      region_jobs = sum(Jobs),
      national_jobs = sum(National.Jobs)
    )



  ## CREATE DFs  summarized by cluster or subcluster name.

  cluster_df <- left_join(xwalk, emsi_table_tidy, by = "NAICS") %>%
    group_by(Cluster, year) %>%
    summarize(
      jobs = sum(Jobs),
      national.jobs = sum(National.Jobs),
      jobs.change = sum(job_change_5yr),
      national.jobs.change = sum(national_change_5yr),
      jobs.pct.change = jobs.change / jobs,
      Sum_Earnings = sum(total_earnings, na.rm = TRUE),
      earnings.jobs = sum(EarningsJobs)
    )



  subcluster_df <- left_join(xwalk, emsi_table_tidy, by = "NAICS") %>%
    group_by(Cluster, SubclusterName, year) %>%
    summarize(
      jobs = sum(Jobs),
      national.jobs = sum(National.Jobs),
      jobs.change = sum(job_change_5yr),
      national.jobs.change = sum(national_change_5yr),
      jobs.pct.change = jobs.change / jobs,
      Sum_Earnings = sum(total_earnings, na.rm = TRUE),
      earnings.jobs = sum(EarningsJobs)
    )


  # Recalculate average earnings
  subcluster_df$average_earnings <- subcluster_df$Sum_Earnings / subcluster_df$earnings.jobs
  cluster_df$average_earnings <- cluster_df$Sum_Earnings / cluster_df$earnings.jobs

  # Recalculate Location Quotient
  # first, initialize columns to 0.
  cluster_df$LQ <- 0
  subcluster_df$LQ <- 0

  ## USE TOTAL JOBS (calculated above) to calculate Location Quotient by Cluster/Sub

  start_year <- min(cluster_df$year)
  end_year <- max(cluster_df$year)

  # subset total jobs to years in cluster/subcluster data frames
  total_jobs <- total_jobs[which(total_jobs$year >= start_year & total_jobs$year <= end_year), ]

  # Initialize a starting value for new columns before looping.
  cluster_df$total.local.jobs <- 0
  cluster_df$total.national.jobs <- 0
  subcluster_df$total.local.jobs <- 0
  subcluster_df$total.national.jobs <- 0

  # iteratively calculating
  for (i in 1:dim(total_jobs)[1]) {
    t_emp_local <- as.numeric(total_jobs$region_jobs[i])

    t_emp_national <- as.numeric(total_jobs$national_jobs[i])

    n1 <- which(cluster_df$year == total_jobs$year[i])
    n2 <- which(subcluster_df$year == total_jobs$year[i])

    cluster_df$total.local.jobs[n1] <- t_emp_local
    cluster_df$total.national.jobs[n1] <- t_emp_national

    subcluster_df$total.local.jobs[n2] <- t_emp_local
    subcluster_df$total.national.jobs[n2] <- t_emp_national
  }


  cluster_df$LQ <- (ifelse(cluster_df$jobs >= 1,
    cluster_df$jobs, 1
  ) / cluster_df$total.local.jobs) /
    (cluster_df$national.jobs / cluster_df$total.national.jobs)

  subcluster_df$LQ <- (ifelse(subcluster_df$jobs >= 1,
    subcluster_df$jobs, 1
  ) / subcluster_df$total.local.jobs) /
    (subcluster_df$national.jobs / subcluster_df$total.national.jobs)

  # TACK ON REGION NAME
  cluster_df$region <- region_name
  subcluster_df$region <- region_name


  # FINAL CLEAN UP - this deletes unneeded columns
  cluster_df$total.local.jobs <- NULL
  cluster_df$total.national.jobs <- NULL

  subcluster_df$total.local.jobs <- NULL
  subcluster_df$total.national.jobs <- NULL

  ## COLLECT TOTAL JOBS FOR WRITING LATER
  all_total_jobs %<>% list.append(data.frame(total_jobs))

  ## WRITE FILES
  write_csv(cluster_df, paste0(output.path, output_cluster_filename))
  write_csv(subcluster_df, paste0(output.path, output_subcluster_filename))
}


## create a df of job totals by geography for use in app.

total_job_df <- all_total_jobs[[1]]
total_job_df <- total_job_df[, c(1, 3, 2)] # reorder the columns for general human reading
colnames(total_job_df) <- c("year", "total.us.jobs", regions[1])

for (i in (2:n_tables)) {
  total_job_df[[regions[i]]] <- all_total_jobs[[i]]$region_jobs
}

write_csv(total_job_df, paste0(output.path, "total_jobs.csv"))
