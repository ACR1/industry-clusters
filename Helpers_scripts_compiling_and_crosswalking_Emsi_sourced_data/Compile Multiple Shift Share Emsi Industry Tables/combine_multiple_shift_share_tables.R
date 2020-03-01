## This R script combines any number of identical 
## industry tables exported from Emsi Developer.
## It was specifically created to combine multiple years worth of shift share tables
## that were identically organized

## It was prepared for NoCO REDI's 2019 Cluster Exploration Workshop


## It has been extensively commented to support novice R and Shiny users.

## Code for this script only is released as CC-BY-SA 4.0. 
## Data files are not included in this license and any content that
## crosswalks or otherwise aligns data with other sources (such as 
## Emsi, U.S. Cluster Mapping, or others)
## can be considered the property of the organization that produced it.


## LOAD LIBRARIES - if they aren't installed: install.packages("NameInQuotes")
library(tidyverse)
library(rlist)
library(readxl)
library(magrittr) # for the %<>% "use then assign" operator
library(english) # to turn a number into a word

# Download some 6 digit industry tables from Emsi as CSVs using these columns:
#   NAICS code, Description, Jobs Change (#), Competitive Effect
#   Expected Change, Industry Mix Effect, National Growth Effect, 
#   Start year jobs, End Year Jobs  (IN THIS EXACT ORDER!!!)

# choose the SAME overlapping year interval for each 
# e.g. two years? 2010-2012, 2011-2013; 5 yrs? 2010-2015, 2011-2016;
# choose the same geography for each as well. 
# We used our "medium" region out of our "three nested regions" approach


# if in rstudio doing an interactive run from the IDE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# using R proper, running from command line or when sourcing this file from another file.
#setwd(getSrcDirectory()[1])

# READ files

xwalk <- read_xlsx("../Shared_Files/Emsi-NAICS-to-Porter-Cluster.xlsx")

# download all the years that you want for the files and drop them in the same
# folder as this file and put them here. 
# this is an example list of file names. Please replace them.
# For this, we only included 1 empty file with the default columns as an example
file_list <- c("Industry_Table4463.csv",
               "Industry_Table1423.csv","Industry_Table2687.csv",
               "Industry_Table1421.csv","Industry_Table904.csv", 
               "Industry_Table7384.csv",
               "Industry_Table4985.csv","Industry_Table7623.csv",
               "Industry_Table8693.csv","Industry_Table4792.csv",
               "Industry_Table8514.csv","Industry_Table4593.csv",
               "Industry_Table5012.csv")


# Name output files however you see fit.
cluster_output_file <- "moving_average_shift_share_cluster.csv"
subcluster_output_file <- "moving_average_shift_share_subcluster.csv"
output_dir <- "../Shared_Files/"

# columns will eventually be renamed to these
df_col_names <- c('NAICS','Description','X.year.job.change',
                 'Competitive.Effect','Expected.Change','Ind.Mix.Effect',
                 'Natl.Growth.Effect','start.jobs','end.jobs','start.year', 
                 'end.year')

# import all those files, extract end year from 

len <- length(file_list)
df_list <- list()
for (i in c(1:len)){
     df <- read.csv(file_list[i])
     if (dim(df)[2] != 9 & substr(colnames(df)[9], 7,10) != "Jobs"){
       # just a simple check to make sure the file isn't totally wrong.
       # this doesn't check if the interior columns are out of order.
       stop("emsi file format doesn't match expected. instr.")
     }

     df$start.year <- as.numeric((substr(colnames(df)[8], 2,5)))
     df$end.year <- as.numeric((substr(colnames(df)[9], 2,5)))
     # last ditch effort to make sure that this makes sense to a human reader.
     print(paste0(
       as.numeric(substr(colnames(df)[8], 2,5)), " - ", 
       as.numeric((substr(colnames(df)[9], 2,5)))
     ))
     
       
     colnames(df) <- df_col_names
     df_list <- list.append(df_list, df)
     }


shift_share_df <- df_list[[1]]

for (i in c(2:len)){
  shift_share_df <- rbind(shift_share_df, df_list[[i]])
}

shift_share_df %<>% 
  mutate(n_years = as.numeric(end.year) - as.numeric(start.year),
         NAICS = as.character(NAICS))


if ( min(shift_share_df$n_years) != max(shift_share_df$n_years) ){
  stop("industry table years are not identical intervals.")
}

n_years <- shift_share_df$n_years[1]

shift_share_df$NAICS <- as.character(shift_share_df$NAICS)

# NEXT Crosswalk to Clusters/Subs and summarize by Cluster/Sub

shift_share_df_cluster_ma <- left_join(xwalk, shift_share_df, by = "NAICS") %>%
  group_by(Cluster, end.year) %>%
  summarize(
            X.year.change = (sum(X.year.job.change)) / first(n_years),
            Competitive.Effect = sum(Competitive.Effect)/ first(n_years),
            Expected.Change = sum(Expected.Change)/ first(n_years),
            Ind.Mix.Effect = sum(Ind.Mix.Effect)/ first(n_years),
            Natl.Growth.Effect = sum(Natl.Growth.Effect)/first(n_years),
            start.jobs = sum(start.jobs),
            end.jobs = sum(end.jobs)
  )

shift_share_df_subcluster_ma <- left_join(xwalk, shift_share_df, by = "NAICS") %>%
  group_by(Cluster, SubclusterName, end.year) %>%
  summarize(
    X.year.change = sum(X.year.job.change)/ first(n_years),
    Competitive.Effect = sum(Competitive.Effect)/first(n_years),
    Expected.Change = sum(Expected.Change)/first(n_years),
    Ind.Mix.Effect = sum(Ind.Mix.Effect/ first(n_years)),
    Natl.Growth.Effect = sum(Natl.Growth.Effect)/ first(n_years),
    start.jobs = sum(start.jobs),
    end.jobs = sum(end.jobs)
    
  )

# Rename the X.years column to plain english name
new_jobs_change_name  <- paste0(as.english(n_years),".year.job.change")

shift_share_df_cluster_ma %<>% 
  rename(!!new_jobs_change_name := "X.year.change" )

shift_share_df_subcluster_ma %<>% 
  rename(!!new_jobs_change_name := "X.year.change" )


# OUTPUT FILES

write_csv(shift_share_df_cluster_ma,
          paste0(output_dir, cluster_output_file))
write_csv(shift_share_df_subcluster_ma, 
          paste0(out_putdir, subcluster_output_file))


