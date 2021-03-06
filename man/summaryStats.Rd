% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryStats.R
\name{summaryStats}
\alias{summaryStats}
\title{Calculate summary statistics}
\format{The returned matrix has the following columns, which are formatted
for putting in a report or table. \cr
\tabular{lll}{
Name \tab Type \tab Description \cr 
Begin \tab character \tab The beginning date of the time series \cr
End \tab character \tab The ending date of the time series \cr
n \tab character \tab Number of rows \cr
NA \tab character \tab Number of missing values \cr
Neg \tab character \tab Number of negative values \cr
Min \tab character \tab The minimum value \cr
Q1 \tab character \tab The first quartile, 25th percentile \cr
Med \tab character \tab The median \cr
Mean \tab character \tab The mean \cr
Q3 \tab character \tab The third quartile, 75th percentile \cr
Max \tab character \tab The maximum value \cr
StdDev \tab character \tab The standard deviation \cr
IQR \tab character \tab The interquartile range \cr
}}
\usage{
summaryStats(dataset, staid = 1)
}
\arguments{
\item{dataset}{is the data frame containing hydrologic data}

\item{staid}{is used to label the output}
}
\value{
a data frame containing a number of summary statistics of the daily 
hydrologic data series
}
\description{
Function to calculate summary statistics for daily hydrologic time series.
}
\details{
The summary statistics returned are useful for exploratory data analysis 
and for describing the date set.
}
\note{
Hydrologic data are often skewed (Helsel and Hirsch, 2002).  Summary 
statistics help describe the degree of skewness and help to determine
the degree of applicability of hypothesis tests.  Some data, in particular
streamflow, may need to be transformed to produce approximately normal
data.
}
\examples{
data(exampleWaterData)
summaryStats(pH05082500, staid="05082500")
}
\references{
Helsel, D.R. and Hirsch, R. M., 2002, Statistical methods in water resources: 
U.S. Geolgical Survey Techniques of Water Resources Investigations, book 4, 
chap. A3, 522 p. (Also available at \url{http://pubs.usgs.gov/twri/twri4a3/}).
}
\keyword{arith}

