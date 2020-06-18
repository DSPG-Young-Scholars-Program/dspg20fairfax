url <- "https://opendata.arcgis.com/datasets/a4d813c396934fc09d0b801a0c491852_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"

download.file(url, destfile = "data/fortune/fortune.zip")

unzip("data/fortune/fortune.zip", exdir = "data/fortune")
