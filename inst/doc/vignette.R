### R code from vignette source 'vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignette.Rnw:40-46
###################################################
# load waterData package, assuming it has already been installed on the system
library(waterData)
q05054000<-importDVs("05054000", code="00060", stat="00003", sdate="2000-01-01", 
                                             edate="2010-12-31")
# return first 6 rows for new data set to view a subset of the data
head(q05054000)


###################################################
### code chunk number 2: vignette.Rnw:52-54
###################################################
my.URL<-tellMeURL("05054000", code="00060", stat="00003", sdate="2000-01-01",
                                       edate="2010-12-31")


###################################################
### code chunk number 3: vignette.Rnw:57-58
###################################################
cat("\\url{", my.URL, "}")


###################################################
### code chunk number 4: vignette.Rnw:67-70
###################################################
data(exampleWaterData, package="waterData")
my.plot<-plotParam(badDataSet)
print(my.plot)


###################################################
### code chunk number 5: vignette.Rnw:79-80
###################################################
cleanUp(badDataSet, task="view")


###################################################
### code chunk number 6: vignette.Rnw:88-89
###################################################
q05054000Fix<-cleanUp(badDataSet, task="fix", replace=0.1)


###################################################
### code chunk number 7: vignette.Rnw:95-97
###################################################
my.plot<-plotParam(q05054000Fix, code="00060", stat="00003", logscale=TRUE)
print(my.plot)


###################################################
### code chunk number 8: vignette.Rnw:103-104
###################################################
q05054000Fix<-cleanUp(badDataSet, task="fix", replace=10)


###################################################
### code chunk number 9: vignette.Rnw:110-112
###################################################
my.plot<-plotParam(q05054000Fix, code="00060", stat="00003", logscale=TRUE)
print(my.plot)


###################################################
### code chunk number 10: vignette.Rnw:121-122
###################################################
summary(misQ05054000)


###################################################
### code chunk number 11: vignette.Rnw:130-132
###################################################
my.newdata <- fillMiss(misQ05054000, block=30, pmiss=50, model="trend", 
                       smooth=TRUE, log="y")


###################################################
### code chunk number 12: vignette.Rnw:141-152
###################################################
library(xtable)
my.xtable<-xtable(summaryStats(q05054000Fix, staid="05054000"),
             cap="Summary statistics for daily streamflow series.  
             Begin, the beginning date for the series; End, the ending 
             date for the series; n, the number of observations; NA, the 
             number of missing values; Neg, the number of negative values; 
             Min, the minimum value; Q1, the first quartile or 25th percentile; 
             Med, the median value; Mean, the mean value; Q3, the third 
             quartile or 75th percentile; Max, the maximum value; StdDev, 
             the standard deviation; IQR, the interquartile range.")
print.xtable(my.xtable, size=c("scriptsize"))


###################################################
### code chunk number 13: vignette.Rnw:162-165
###################################################
par(cex.lab=.9, las=1, tcl=0.5, xaxs="r", yaxs="r", cex.axis=0.8)
qqnorm(q05054000Fix$val)
qqline(q05054000Fix$val)


###################################################
### code chunk number 14: vignette.Rnw:173-176
###################################################
par(cex.lab=.9, las=1, tcl=0.5, xaxs="r", yaxs="r", cex.axis=0.8)
qqnorm(log10(q05054000Fix$val))
qqline(log10(q05054000Fix$val))


###################################################
### code chunk number 15: vignette.Rnw:185-190
###################################################
my.sites <- c("05054000","05082500","05061000","05050000","05058700", "05267000",
                        "06342500", "06478000", "06414000")
my.siteInfo <- siteInfo(my.sites)
xtable(my.siteInfo[order(my.siteInfo$staid),], cap="Information for select 
       U.S. Geological Survey streamgage sites, sorted in downstream order.")


###################################################
### code chunk number 16: vignette.Rnw:198-222
###################################################
library(maps)
library(mapdata)
par(las=1,tck=0.02,mar=c(0,0,0,0))
map('state', region=c('minnesota', '.*dakota'))
map('rivers',add=TRUE,col=4)
# label centered over gage site, jitter added to differentiate sites close 
# together
mindif<-0
maxiterations<-30
iteration<-1
while (mindif<0.085) {
  y.offset<-as.numeric(my.siteInfo$lat)+runif(length(my.siteInfo$lat),
                                                           0.12,0.45)
  mindif <- min(diff(unique(sort.int(round(as.numeric(y.offset),digits=3)))))
  iteration<-iteration + 1
  if ( iteration >= maxiterations ) {
    mindif<-0.09
    message("No ideal jitter found.  Some labels may conflict")
  }
}
points(my.siteInfo$lng, my.siteInfo$lat, pch=19, col="green")
  text(xy.coords(my.siteInfo$lng,y.offset),my.siteInfo$staid,cex=0.55)
box()
map.axes()


###################################################
### code chunk number 17: vignette.Rnw:229-230
###################################################
siteInfoURL<-tellMeSiteURL("05054000")


###################################################
### code chunk number 18: vignette.Rnw:233-234
###################################################
cat("\\url{", siteInfoURL, "}")


###################################################
### code chunk number 19: vignette.Rnw:276-280
###################################################
anoms365.30.1 <- compAnom(q05054000, which=1)
anoms100.10.1 <- compAnom(q05054000, which=2)
anoms30.1 <- compAnom(q05054000, which=3)
anomsLT <- compAnom(q05054000, which=4)


###################################################
### code chunk number 20: vignette.Rnw:288-289
###################################################
plotAnoms(anoms365.30.1)


###################################################
### code chunk number 21: vignette.Rnw:297-298
###################################################
plotAnoms(anoms100.10.1)


###################################################
### code chunk number 22: vignette.Rnw:306-307
###################################################
plotAnoms(anoms30.1)


###################################################
### code chunk number 23: vignette.Rnw:315-320
###################################################
q05054000LT<-importDVs("05054000", code="00060", stat="00003", sdate="1949-10-01", 
                                                  edate="2010-9-30")
my.xtable<-xtable(summaryStats(q05054000LT, staid="05054000"),
                                  cap="Summary statistics for daily streamflow series.")
print.xtable(my.xtable, size=c("footnotesize"))


###################################################
### code chunk number 24: vignette.Rnw:325-327
###################################################
q05054000LT<-cleanUp(q05054000LT, task="fix")
anomsLT <- compAnom(q05054000LT, which=4)


###################################################
### code chunk number 25: vignette.Rnw:333-334
###################################################
plotAnoms(anomsLT)


