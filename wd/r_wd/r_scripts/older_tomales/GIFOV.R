
#http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization


# GIFOV calculation
#


h<-seq(0.5, 2, .001)
#h<-.85



alpha<-4
beta<-30
conv<-(pi/180)
alpha<-alpha*conv
beta<-beta*conv 


delta<-(beta-alpha)
D<-h*tan(delta)
Z<-h*(tan(beta) + tan(beta + alpha))
W<-h*tan(beta)
major<-round((((W-D)+Z)/2),4) # changed from excel desination
x<-sqrt(h^2 +w^2)
y<-x*tan(alpha)
area<-pi*major*y

#lm.area<-lm(area ~ h)
plot(h, area, xlab="height in meters", ylab=" meters squared", main=" Area of elipse by height above water", sub="FOV 8 degree. 30 degree angle")
abline(v=.68, col="red")
abline(v=1, col="red")

plot(h, major, xlab="height in meters", ylab="major axis length meters", main=" Major axis elipse by height above water", sub="FOV 8 degree. 30 degree angle", col="blue")
abline(v=.68, col="red")
abline(v=1, col="red")

plot(h, y, xlab="height in meters", ylab="minor axis length meters", main=" Minor axis elipse by height above water", sub="FOV 8 degree. 30 degree angle", col="green")
abline(v=.68, col="red")
abline(v=1, col="red")


