% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fillMiss.R
\name{fillMiss}
\alias{fillMiss}
\title{Fill-in missing hydrological values}
\format{The returned data frame has the following columns: \cr
\tabular{lll}{
Name \tab Type \tab Description \cr 
staid \tab factor \tab USGS station identification number \cr
val \tab numeric \tab The value of the hydrologic variable \cr
dates \tab Date \tab Date of daily value \cr
qualcode \tab factor \tab Qualification code
}}
\usage{
fillMiss(dataset, block = 30, pmiss = 40, model = "trend",
  smooth = TRUE, ...)
}
\arguments{
\item{dataset}{is a data frame in the format of the data frame returned by
\link{importDVs}, with missing values indicated by NA.}

\item{block}{is the size of the largest block of missing data that the 
function will fill-in.}

\item{pmiss}{is the maximum amount of the missing data that can be missing
in the dataset for fill-in procedure to be performed.}

\item{model}{is the type of structural time series model, see 
\link{StructTS}.  The default value is trend.  If level is used, the results 
of \link{fillMiss}, which by default applies a fixed-interval smoothing to 
the time series, \link{tsSmooth}, will be very close to linear interpolation.}

\item{smooth}{a logical that indicates whether or not to apply 
\link{tsSmooth} to the structured time series.}

\item{...}{further arguments to be passed to plotting method (see \link{par}).}
}
\value{
a data frame with NAs in the "val" column replaced by 
estimated values and a plot showing observed and estimated data.  If there 
are too many missing values, based on default or user defined limits, the
unaltered dataset is returned as well as a message, such as "Error 
in fillMiss(misQ05054000) : Too much missing data.  Cannot fill in missing 
values."
}
\description{
Function to fill in missing time series data.
}
\details{
This function will check the percent of missing values and the size
of the largest missing block of data.  By default, if less than 40 percent 
of the data are missing and the largest block is less than 30 days, the 
data will be filled-in by using a structural time series, \link{StructTS} 
from the base stats  package in R (R Development Core Team, 2012).  The 
fitted structural time series is then smoothed via a state-space model, 
\link{tsSmooth} from the base stats package in R.
}
\note{
Many methods have been suggested for estimating missing hydrological 
data.  However, experiments showed that the functions in the base stats 
package  worked very well if the blocks of missing data were not long.  
Users with larger blocks of missing data may want to explore other methods 
including using nearby gages to estimate missing values at a streamgage.  
Additional methods for filling in missing hydrological data are summarized 
in Beauchamp and others (1989) and Elshorbagy and others (2000).

To indicate which values have been replaced, the qualcode field is
populated with 'fM' for those values that were estimated using the
fillMiss function.
}
\examples{
data(exampleWaterData)
my.newdata <- fillMiss(misQ05054000, block=30, pmiss=50, log="y")
summary(misQ05054000)
summary(my.newdata)
# ph example
 \dontrun{
pH05082500<-importDVs("05082500", code="00400", stat="00008", 
sdate="2000-01-01", edate="2011-12-31")
plotParam(pH05082500)
pHfilled<-fillMiss(pH05082500, block=45, ylim=c(7.5,9), yaxs="i")
 }
}
\references{
Beauchamp, J.J., 1989, Comparison of regression and time-series methods for
synthesizing missing streamflow records: Water Resources Bulletin, v. 25, 
no. 5, p. 961--975.

Elshorbagy, A.A., Panu, U.S., Simonovic, S.P., 2000, Group-based
estimation of missing hydrological data---I. Approach and general
methodology: Hydrological Sciences Journal, v. 45, no. 6, p. 849--866.

R Development Core Team, 2012, R---A language and environment for statistical
computing: Vienna, Austria, R Foundation for Statistical Computing, [ISBN
3-900051-07-0].  (Also available at \url{https://www.r-project.org}.)
}
\seealso{
\link{StructTS}, \link{tsSmooth}, \link{cleanUp}
}
\keyword{NA}
\keyword{datagen}
\keyword{hplot}
\keyword{smooth}
\keyword{ts}

