library(data.table)
library(R.utils)
library(ggplot2)
library(stringr)
library(tabulizer)

#------------------- download LODES WAC file -------------------------------- #

download.file(url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/wac/va_wac_S000_JT00_2017.csv.gz",
  destfile =  "/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_wac_S000_JT00_2017.csv.gz")

#gunzip("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_wac_S000_JT00_2017.csv.gz")


#--------------------- download va xwalk file --------------------------------#

download.file( url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/va_xwalk.csv.gz", 
  destfile =  "/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_xwalk.csv.gz")

#gunzip("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_xwalk.csv.gz")


#--------------- test that tables read in correctly ------------------------------#

wac <- read.csv("data/original/va_wac_S000_JT00_2017.csv",
                colClasses = c("w_geocode" = "factor"))

xwalk <- read.csv("data/original/va_xwalk.csv", 
                 colClasses = c("tabblk2010" = "factor"))

fairfax <- merge(wac, xwalk[xwalk$cty == 51059, c("tabblk2010","cty", "trct", "bgrp")], by.x = "w_geocode", by.y = "tabblk2010")


#--------------------- create readable names table ------------------------------#
# I had to do this locally because tabulizer requires rJava which is not downloaded in Rivanna

location <- "https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.0.pdf"

out <- extract_tables(location)

df_1 = data.frame(out[[4]],stringsAsFactors = FALSE)
colnames(df_1) <- c("Pos", "Variable", "Type", "Explanation")

df_2 <- as.data.frame(out[[5]],stringsAsFactors = FALSE)
colnames(df_2) <- c("Pos", "Variable", "Type", "Explanation")


df_1[df_1$Explanation == "Number of jobs in NAICS sector 56 (Administrative and Support and Waste", "Explanation"] <- "Number of jobs in NAICS sector 56 (Administrative and Support and Waste Management and Remediation Services)"

df_1 <- df_1[-c(1:2, 25),  ]


readable_names <- rbind(df_1, df_2)

readable_names$Explanation <- str_replace_all(string=readable_names$Explanation, pattern=" ", repl="")


readable_names$Explanation <- gsub("Numberofjobsforworkers", "", readable_names$Explanation)
readable_names$Explanation <- gsub("Numberofjobswith", "", readable_names$Explanation)
readable_names$Explanation <- gsub("Numberofjobsin", "", readable_names$Explanation)
readable_names$Explanation <- gsub("atfirmswith", "", readable_names$Explanation)
readable_names$Explanation <- gsub("with", "", readable_names$Explanation)

readable_names <- readable_names[, c("Variable", "Explanation")]

#write.csv(readable_names, "data/original/lodes_wac_cols.csv")

#-------------- read in readable column names and apply them to table------------------------- #
readable_col <- read.csv("data/original/lodes_wac_cols.csv")

colnames(fairfax)[!(colnames(fairfax) %in% readable_col$Variable)]

cols <- c(readable_col$Explanation, colnames(fairfax)[!(colnames(fairfax) %in% readable_col$Variable)])

colnames(fairfax) <- cols


