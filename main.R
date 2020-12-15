# read in packages and function
library(jpeg)
library(gridExtra)
library(exifr)
library(imager)

source('r/function_get_gcc.R')

# read aoi file
aoi.df <- read.csv('data/Dates_Positions.csv',stringsAsFactors = F)
aoi.df$Date <- as.Date(aoi.df$Date,'%d/%m/%Y')


# get gcc for pot
pot.nm <- unique(aoi.df$Pot_ID)[1]#you will need to loop
aoi.pot <- aoi.df[aoi.df$Pot_ID==pot.nm,]

# read in picture date 
pic.folder <- 'data/Pictures/1/'# sprintf('pic/%s/',pot.nm)

pictures.fn <- list.files(pic.folder,pattern = '.JPG',full.names = T)

# loop through the pictures
tmp.ls <- list()
for (i in seq_along(pictures.fn)) {
  
  date.pic<- read_exif(pictures.fn[i])$CreateDate
  date.pic <- as.Date(date.pic,'%Y:%m:%d')
  ROIDates <- aoi.pot$Date
  index.aoi <- ROI.Date.func(dd=date.pic,n=length(aoi.pot$Date))
  
  aoi.pot.xy <- aoi.pot[index.aoi,c('Xmin','Xmax','Ymin','Ymax')]
  aoi.pot.xy <- as.numeric(aoi.pot.xy)
  tmp.df <- processImage.new(pictures.fn[i],ROI = aoi.pot.xy)
  tmp.df$Date <- date.pic
  
  tmp.ls[[i]] <- tmp.df
}

pot.gcc.df <- do.call(rbind,tmp.ls)

pot.gcc.df$Pot_ID <- pot.nm
