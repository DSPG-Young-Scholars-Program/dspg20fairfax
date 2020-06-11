library(data.table)
library(R.utils)
library(ggplot2)
library(stringr)
#https://lehd.ces.census.gov/data/lodes/LODES7/va/wac/
#download.file(url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/wac/va_wac_S000_JT00_2017.csv.gz",
#destfile =  "/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_wac_S000_JT00_2017.csv.gz")

#gunzip("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_wac_S000_JT00_2017.csv.gz")

#download.file(
 # url = "https://lehd.ces.census.gov/data/lodes/LODES7/va/va_xwalk.csv.gz", 
  #destfile =  "/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_xwalk.csv.gz")

#gunzip("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/original/va_xwalk.csv.gz")

tbl <- read.csv("data/original/va_wac_S000_JT00_2017.csv",
                colClasses = c("w_geocode" = "factor"))

tbl2 <- read.csv("data/original/va_xwalk.csv", 
                 colClasses = c("tabblk2010" = "factor"))

tbl3 <- merge(tbl, tbl2[tbl2$cty == 51059, c("tabblk2010","cty", "trct", "bgrp")], by.x = "w_geocode", by.y = "tabblk2010")

library(tabulizer)

readable_col <- read.csv("src/lodes_cols.csv")

colnames(tbl3)[!(colnames(tbl3) %in% readable_col$Variable)]

cols <- c(readable_col$Explanation, colnames(tbl3)[!(colnames(tbl3) %in% readable_col$Variable)])


colnames(tbl3) <- cols

age <- colSums(tbl3[, 3:5])
age
pie(age)


earnings <- colSums(tbl3[, 6:8])
earnings
pie(earnings)

naic <- colSums(tbl3[, 9:28])
naic <- data.frame(NAIC=names(naic), value=naic, row.names=NULL)


x <- str_extract_all(naic$NAIC,  "(?<=\\().+?(?=\\))")

naic$NAIC <- unlist(x)

ggplot(naic, aes(x = value, y = NAIC)) +
  geom_bar(stat= "identity")


race <- colSums(tbl3[, 29:34])
pie(race)

ethnicity <- colSums(tbl3[, 35:36])
pie(ethnicity)

education <- colSums(tbl3[, 37:40])
pie(education)

sex <- colSums(tbl3[, 41:42])
pie(sex)

firm_age <- colSums(tbl3[, 43:47])
# no data
firm_age

firm_size <- colSums(tbl3[, 48:52])
# no data
firm_size