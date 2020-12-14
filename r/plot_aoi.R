library(imager)
t1 <- load.image('pic/ng/9/WSCT0793.jpg')


plot((t1),ann=F,axes=F)
polygon(c(0.,1,1,0)*nrow(t1),c(0.1,0.1,0.8,0.8)*ncol(t1),border = 'red')

