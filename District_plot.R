
#Create a map of the administrative areas of Afghanistan
#========================================================
#Original methods explained in stackoverflow in following link:

#http://stackoverflow.com/questions/17723822/administrative-regions-map-of-a-country-with-ggmap-and-ggplot2

#http://stackoverflow.com/questions/19718814/how-to-draw-ggmap-with-two-different-administrative-boundaries

#Data accessed on http://www.gadm.org/ on 9th October 2014



#Load the libraries needed
library(ggplot2)
library(rgdal)
library(RCurl)

#Download the data if it doesn't exist.

read.url <- function(url, ...){
  dataFile <- tempfile()
  download.file(url, destfile = dataFile, method = "curl")
  return(dataFile)
}

if (file.exists("AFG_adm2.RData")) {
  print("Loading existing data from file")
  load("AFG_adm2.RData")
} else {
  print("Downloading Data from Web URL:")
  URL <- "http://data.biogeo.ucdavis.edu/data/gadm2/R/AFG_adm2.RData"
  print(URL)
  tempfile = read.url(URL)
  save(tempfile,file="AFG_adm2.RData")
  load(tempfile)
}


afghanistan.adm2.spdf <- get("gadm")
#print(afghanistan.adm2.spdf)

country = FALSE
#Extracting names and centoids of administrative areas for plotting

# Get centroids of spatialPolygonDataFrame and convert to dataframe
# for use in plotting  area names. 

afghanistan.adm2.centroids.df <- data.frame(long = coordinates(afghanistan.adm2.spdf)[, 1], 
   lat = coordinates(afghanistan.adm2.spdf)[, 2]) 

# Get names and id numbers corresponding to administrative areas
afghanistan.adm2.centroids.df[, 'ID_2'] <- afghanistan.adm2.spdf@data[,'ID_2']
afghanistan.adm2.centroids.df[, 'NAME_2'] <- afghanistan.adm2.spdf@data[,'NAME_2']
afghanistan.adm2.centroids.df[, 'NAME_1'] <- afghanistan.adm2.spdf@data[,'NAME_1']

#Create ggplot with labels for administrative areas

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

UIinput_province <- function(){
  
  #Ask for user input
  x <- readline(prompt = "Please enter the name of the province you would like to plot: ")
  
  #Can it be converted?
  x <- toString(x)
  
  return(x)
}

UIinput_districts <- function(){
  
  #Ask for user input
  x <- readline(prompt = "Please enter the name of the districts you would like to plot, separated by commas: ")
  
  x <- unlist(strsplit(x, ","))
  
  x <- unlist(lapply(x, trim))
  
  return(x)
}

UIinput_colour <- function(){
  
  #Ask for user input
  x <- readline(prompt = "Please specify the colour you would like to use, if you are feeling lucky. Red is the default option: ")
  
  #Can it be converted?
  x <- toString(tolower(x))
  
  if(x == ""){
    x <- "red"
  }
  return(x)
}

UIinput_country<- function(){
  
  #Ask for user input
  x <- readline(prompt = "Would you like to print a country-wide map [y/n]?: ")
  
  #Can it be converted?
  x <- toString(tolower(x))
  
  if(x == "y"){
    country = TRUE
  }
  
  return(country)
}

province <- UIinput_province()

#Subset just the province required for level 2 data
sub.shape <- afghanistan.adm2.spdf[afghanistan.adm2.spdf$NAME_1 == province,]

afghanistan.adm1.df <- fortify(afghanistan.adm2.spdf, region = "NAME_1")
afghanistan.adm2.df <- fortify(sub.shape, region = "NAME_2")

toplot0 <- afghanistan.adm1.df <- fortify(afghanistan.adm2.spdf, region = "NAME_0")

print(paste("You have selected the", province, "province."))

print(paste("Districts in", province, ":"))

print(levels(as.factor(afghanistan.adm2.df$id)))

districts <- UIinput_districts()

if (districts == "ALL") {districts <- levels(as.factor(afghanistan.adm2.df$id))}
UIcolour <- UIinput_colour()

country <- UIinput_country()

toplot1 = subset(afghanistan.adm1.df, id == province)

toplot2 <- replicate(length(districts), data.frame())

for (i in 1:length(districts)) {
  toplot2[[i]] <- subset(afghanistan.adm2.df, id == districts[i])
}

#district_cent <- unlist(lapply(districts,FUN=gsub, pattern = "\\.[0-9]",replacement = ""))

cent2 <-subset(afghanistan.adm2.centroids.df, (NAME_2 %in% districts))

cent2 <-subset(cent2, NAME_1 == province)

if (country) {
  p <- ggplot(toplot1, aes(x = long, y = lat, group = group)) + geom_polygon(fill="white") +
  geom_polygon(data=toplot0, aes(x=long, y=lat, group=group), fill="white", color = "black") +
  geom_path(data = toplot1, aes(x = long, y = lat), color = "black")+ 
  lapply(toplot2,geom_polygon, mapping=aes(x=long, y=lat), fill= UIcolour) + 
  lapply(toplot2,geom_path, mapping=aes(x=long, y=lat))+
  labs(x=" ", y=" ") + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())
} else {
  p <- ggplot(toplot1, aes(x = long, y = lat, group = group)) + geom_polygon(fill="white") +
    geom_path(data = toplot1, aes(x = long, y = lat), color = "black")+ 
    lapply(toplot2,geom_polygon, mapping=aes(x=long, y=lat), fill= UIcolour) + 
    lapply(toplot2,geom_path, mapping=aes(x=long, y=lat))+
    geom_text(data = cent2, aes(label = NAME_2, x = long, y = lat, group = NAME_2), size = 5) +
    labs(x=" ", y=" ") + 
    theme_bw() + 
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
    theme(panel.border = element_blank())
}
#Plot and save the result

print(p)

ggsave(paste("Afghanistan",province,"all3.png",sep="_"), width=4, height=4, dpi=300)
