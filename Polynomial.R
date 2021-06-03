install.packages("corrplot")
install.packages("ggplot")
install.packages("gridExtra")
library(corrplot)
library(ggplot2)
library(gridExtra)

##reading the data
backdata <- read.csv("data/Vertebral.csv", stringsAsFactors = T)

##visualization data
View(backdata)
summary(backdata)
pairs(backdata)
cors <- cor(backdata[c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope","pelvic_radius","degree_spondylolisthesis")])
paris <- pairs(backdata[c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope","pelvic_radius","degree_spondylolisthesis")])
str(backdata) 
corrplot(backdata[c("pelvic_incidence", "pelvic_tilt", "lumbar_lordosis_angle", "sacral_slope","pelvic_radius","degree_spondylolisthesis")],method = "color")


                          ##linear Model###
##Modelling
lindata<-lm(lumbar_lordosis_angle~sacral_slope,data=backdata)
lindata
summary(lindata)
## Coefficients sacr_slope 0.8271
##linear fuction
linfun<-function(x){
  return(0.8271*x+16.4030)
}

x<-backdata$sacral_slope
y<-linfun(x)
meanslinear<-data.frame(x,y)
meanslinear$group<-1:310
linear<-ggplot(data=backdata,aes(x=sacral_slope,y=lumbar_lordosis_angle))+geom_point()+
  stat_function(aes(sacral_slope),fun=linfun)+ggtitle("Linear")+ geom_point(data=meanslinear,aes(x=x,y=y),color="red")
linear

                                ##polynomial degree = 2
poly2<-lm(lumbar_lordosis_angle~poly(sacral_slope,2,raw=T),data=backdata)
poly2
summary(poly2)
##Coefficients
funpoly2<-function(x){
  return(-0.009581*x^2+1.751516*x-3.90134)
}

xpoly2<-backdata$sacral_slope
ypoly2<-funpoly2(x)
meanspoly2<-data.frame(xpoly2,ypoly2)
meanspoly2$group<-1:310

poly2<-ggplot(data=backdata,aes(x=sacral_slope,y=lumbar_lordosis_angle))+geom_point()+
  stat_function(aes(sacral_slope),fun=funpoly2)+ggtitle("Polynomial degree 2")+geom_point(data=meanspoly2,aes(x=xpoly2,y=ypoly2),color="red")
poly2
summary(poly2)

                  ##polynomial degree = 3

poly3<-lm(lumbar_lordosis_angle~poly(sacral_slope,3,raw=T),data=backdata)
poly3
summary(poly3)
funpoly3<-function(x){
  return(-0.0002009*x^3+0.0275658*x^2-0.2326161*x+27.0618805)
}

xpoly3<-backdata$sacral_slope
ypoly3<-funpoly3(x)
meanspoly3<-data.frame(xpoly3,ypoly3)

poly3<-ggplot(data=backdata,aes(x=sacral_slope,y=lumbar_lordosis_angle))+geom_point()+
  stat_function(aes(sacral_slope),fun=funpoly3)+ggtitle("Polynomial degree 3")+geom_point(data=meanspoly3,aes(x=xpoly3,y=ypoly3),color="red")
poly3

              ##polynomial degree = 5
poly5<-lm(lumbar_lordosis_angle~poly(sacral_slope,5,raw=T),data=backdata)
poly5
summary(poly5)
funpoly5<-function(x){
  return((1.442*10^-07)*x^5+(-4.011*10^-05)*x^4+(3.764*10^-03)*x^3+(-1.466*10^-01)*x^2+(3.126*10^00)*x+(4.845*10^00))
}

xpoly5<-backdata$sacral_slope
ypoly5<-funpoly5(x)
meanspoly5<-data.frame(xpoly5,ypoly5)

poly5<-ggplot(data=backdata,aes(x=sacral_slope,y=lumbar_lordosis_angle))+geom_point()+
  stat_function(aes(sacral_slope),fun=funpoly5)+ggtitle("Polynomial degree 5")+geom_point(data=meanspoly5,aes(x=xpoly5,y=ypoly5),color="red")
poly5

##Review of graphs
grid.arrange(linear,poly2,poly3,poly5,top="Overall Review of Fitting Data")
