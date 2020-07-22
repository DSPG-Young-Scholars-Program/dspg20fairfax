library(colortools)
theme_dspg <-c("#232D4B",
               "#2C4F6B", 
               "#0E879C", 
               "#60999A", 
               "#D1E0BF",
               "#D9E12B",
               "#E6CE3A", 
               "#E6A01D",
               "#E57200",  
               "#FDFDFD")

wheel <- function(col, radius = 1, ...)
  
pie(rep(1, length(col)), col=col, radius=radius)

wheel(theme_dspg)

library(RColorBrewer)
pal <- colorRampPalette(theme_dspg[c(10, 3)])(10)
pie(rep(1, length(pal)), col = pal , main="") 



# navy
navy <- c('#232d4b', '#374368', '#4f5b83', '#69749d', '#858eb4', '#a2a9c9', '#c0c5dd', '#dfe1ef', '#ffffff')
pie(rep(1, length(navy)), col = navy , main="") 
# teal
teal <- c('#0e879c', '#4eb2c6', '#9cdbe9', '#ffffff')
pie(rep(1, length(teal)), col = teal , main="") 
# yellow
yellow <- colorRampPalette(theme_dspg[c(10, 6)])(8)
pie(rep(1, length(yellow)), col = yellow , main="") 
# orange
orange <- colorRampPalette(theme_dspg[c(10, 9)])(8)
pie(rep(1, length(orange)), col = orange , main="") 







