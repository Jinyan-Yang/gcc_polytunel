source('r/functions.R')

ROI.Date.func<-function(dd,n=length(ROIDates))
{
  if (n==1) return(1) #if the date falls before the first ROI.Date.func return that.
  if (dd<ROIDates[n]) {ROI.Date.func(dd,n-1)}
  else {return(n)}
}
# modifed from Remko's function

# fn <- 'pic/ng/9/WSCT1262.jpg'

processImage.new <- function(fn, ROI=NULL){
  
  phot <- read_and_crop(fn,NULL)
  if(is.null(phot)){
    return(data.frame(filename=NA, GCC=NA, RCC=NA, BCC=NA, RGBtot=NA))
  }
  
  if(!is.null(ROI)){
    xmin <- ceiling(max(phot$x) * ROI[1])
    xmax <- ceiling((max(phot$x) * ROI[2]))
    
    ymin <- ceiling(max(phot$y) * ROI[3])
    ymax <- ceiling(max(phot$y) * ROI[4])
    
    phot <- phot[phot$x >= xmin & phot$x <= xmax &
                   phot$y >= ymin & phot$y <= ymax,]
  }
  library(raster)
  
  RDN <- mean(phot$R)
  GDN <- mean(phot$G)
  BDN <- mean(phot$B)
  bn <- RDN+GDN+BDN
  
  RI <- RDN/bn
  GI <- GDN/bn
  BI <- BDN/bn
  
  #GEI <- 2*GDN - (RDN + BDN)
  
  # Convention
  rgbtot <- bn * 255
  
  print(paste0(fn,' finished'))
  
  return(data.frame(filename=fn, GCC=GI, RCC=RI, BCC=BI, RGBtot=rgbtot))
}


get_gcc_func <- function(fn.vec, ROI=NULL){
  
  date.vec <- read_exif(fn.vec)$CreateDate
  tmp.ls <- list()
  for(i in seq_along(fn.vec)){
   tmp.ls[[i]] <- processImage.new(fn.vec[i], ROI=ROI)
  }
  
  # put gcc into a data frame
  gcc.day.df <- do.call(rbind,tmp.ls)
  
  # gcc.day.df$DateTime <- as.Date(as.character(date.vec),'%Y%m%d')
  gcc.day.df$DateTime <- date.vec
  gcc.day.df$Date <-  as.Date(date.vec,'%Y:%m:%d')
  
  return(gcc.day.df)
  
  
}

get.smooth.gcc.func = function(Date.vec,gcc.vec){
  library(mgcv)
  library(lubridate)
  gam.frdm = round(length(Date.vec)/3)
  
  gam.in.df = data.frame(x = as.numeric(Date.vec),
                         y = gcc.vec)
  fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
  
  out.df = predict(fit.gam,gam.in.df)
  return(out.df)
}

