
rm(list = ls())

library(tidyverse)
setwd("/sfs/qumulo/qhome/kb7hp/dspg20fairfax/data/od")
raw_data <- read_csv("jobsall.csv")



raw_data %>% 
  rename(from = h_geocode_tract, to = w_geocode_tract, 
         S000, S)



# https://github.com/uva-bi-sdad/oss-2020/blob/master/src/github-network-analysis/02_international-collaboration/03_country-networks/03_ctry-nets-cum-analysis.Rmd

# best to filter in/out of ffx county 


