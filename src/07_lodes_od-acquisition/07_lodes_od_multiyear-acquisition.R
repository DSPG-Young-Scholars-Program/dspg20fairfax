library(R.utils)
library(data.table)
library(tidyr)
library(dplyr)




for(year in 2010:2017){
  
  
  if (year > 2015){
    
    vars <- c("JT00", "JT02")
  
  } else{
    
    vars <- c("JT00", "JT02","JT04")
  
  }
  
  
  for(var in vars){ 
    
    for(type in c("main", "aux")){
    
  download.file(
   url = paste("https://lehd.ces.census.gov/data/lodes/LODES7/va/od/va_od_", type, "_", var, "_", year, ".csv.gz", sep = ""), 
    destfile = paste("data/original/va_od/va_od_", type, "_", var, "_", year, ".csv.gz", sep = ""))
    
    gunzip(paste("data/original/va_od/va_od_", type, "_", var, "_", year, ".csv.gz", sep =""))
    
    data <- fread(paste("data/original/va_od/va_od_", type, "_", var, "_", year, ".csv", sep = ""), 
          colClasses = c("w_geocode" = "character",
                         "h_geocode" = "character"))
    
    assign(paste(type), data)
    
    }
    
    data <- rbind(main, aux)

    
    fairfax <- data %>% 
      filter(w_geocode %in% data$w_geocode[substr(data$w_geocode, 1, 5) == "51059"]|h_geocode %in% data$h_geocode[substr(data$h_geocode, 1, 5) == "51059"])
    
    
    fairfax$w_geocode_tract <- substr(fairfax$w_geocode, start = 1, stop = 11)
    
    fairfax$h_geocode_tract <- substr(fairfax$h_geocode, start = 1, stop = 11)
    
    
    if(var == "JT00"){
      label <- "all"
    } else if(var == "JT02"){
      label <- "private"
    } else if(var == "JT04"){
      label <-  "federal"
    }
    
    jobs <- fairfax %>% select(-w_geocode, - h_geocode) %>%
      group_by(createdate, w_geocode_tract, h_geocode_tract) %>% 
      summarise(S000 = sum(S000), 
                  SA01 = sum(SA01), 
                  SA02 = sum(SA02), 
                  SA03 = sum(SA03), 
                  SE01 = sum(SE01), 
                  SE02 = sum(SE02), 
                  SE03 = sum(SE03), 
                  SI01 = sum(SI01), 
                  SI02 = sum(SI02), 
                  SI03 = sum(SI03))
    write.csv(jobs, paste("data/od/jobs_", label, "_", year, ".csv", sep = ""))
    
    
  }
  
}


