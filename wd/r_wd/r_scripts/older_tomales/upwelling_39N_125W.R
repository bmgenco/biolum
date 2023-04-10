#plotting daily upwelling for 39N 125W
#differe from dropbox version....

# cuti and beauo
#http://www.mjacox.com/wp-content/uploads/2020/02/CUTI_daily.csv


## automatically generate ch-l form website:
# http://erddap.cencoos.org/erddap/tabledap/documentation.html

x<-read.csv("https://www.pfeg.noaa.gov/products/PFELData/upwell/daily/p09dayac.all", header=T, skip = 5, sep="")
#x[x == -9999] <- NA # NAs break time series!!!



# 15 day delay fo bakun

y<-read.csv("http://www.mjacox.com/wp-content/uploads/2020/02/CUTI_daily.csv") # look like I will need to edit per month
y<-y[,c(1:3,12)]# hardcoded, improve

#y<-


# set plot lengh here
days<-30


start<-x[(length(x$YYYYMMDD)-days),1]
end<-(x[length(x$YYYYMMDD), 1])

names(y)



times<-ts(x, start=start, frequency = 1, end=end)
times<-ts(x, start=start, end=end)
labs<-as.character(x[(which(x==start):which(x==end)),1])


plot(times[,2], col= "red", 
     ylab="Index: (+) = upwelling, (-) = downwelling", lwd=4, 
     main="Bakun Upwelling Index: 39 N, 125 W",  xaxt = "n"
     )
axis(side = 1, tck=1, las=2, labels=labs, at=labs)

        #### hack


View(x)

t<-100
l<-length(x$YYYYMMDD)
start<-(l-t)

sub<-x[start:(l-5),]



plot(sub$Index ~ sub$YYYYMMDD, col= "red", xaxt = "n",
     ylab="Index: (+) = upwelling, (-) = downwelling", lwd=4, 
     main="Bakun Upwelling Index: 39 N, 125 W", xlab=""  
)

labs<-sub$YYYYMMDD
axis(side = 1, tck=1, las=2, labels=labs, at=labs)


