\name{tellMeURL}
\alias{tellMeURL}
\title{USGS Daily Values Site Service URL}
\usage{
  tellMeURL(staid, code = "00060", stat = "00003",
    sdate = "1851-01-01",
    edate = as.Date(Sys.Date(), format = "\%Y-\%m-\%d"))
}
\arguments{
  \item{staid}{is the USGS site identification number,
  which is usually eight digits long, but can be longer.
  Users may search for surface-water sites and obtain
  station identification numbers using the USGS Site Web
  Service,
  \url{http://waterservices.usgs.gov/rest/Site-Service.html}
  (U.S. Geological Survey, 2012d); using the National Water
  Information System: Mapper,
  \url{http://wdr.water.usgs.gov/nwisgmap/} (U.S.
  Geological Survey, 2012a); or using the National Water
  Information System: Web Interface to daily surface-water
  data,
  \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}
  (U.S. Geological Survey, 2012e).  The site identification
  number needs to be entered as a character, that is in
  quotes, because many USGS streamgage numbers begin with
  zero and the leading zero is necessary.}

  \item{code}{is the USGS parameter code, a 5-digit number
  used in the USGS computerized data system, National Water
  Information System (NWIS), to uniquely identify a
  specific hydrologic property or constituent.  A list of
  paramater codes is available at
  \url{http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes}
  (U.S. Geological Survey, 2012b).}

  \item{stat}{is the USGS statistics code, a 5-digit number
  used in the USGS computerized data system, NWIS, to
  uniquely identify specific statistics, such as daily
  mean, daily maximum, and daily minimum. The default,
  00003, is the mean daily value.  A list of statistics
  codes is available at
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
  (U.S. Geological Survey, 2012c). Not all statistics are
  available at every gage.}

  \item{sdate}{is the start date of the time series, in the
  format yyyy-mm-dd.}

  \item{edate}{is the end date of the time series, in the
  format yyyy-mm-dd.}
}
\value{
  URL for USGS data
}
\description{
  Function that returns USGS Daily Values Site Service URL
  for troubleshooting or building a URL for other purposes.
}
\examples{
tellMeURL("05054000", code="00060", stat="00003", sdate="2000-01-01",
 edate=as.Date(Sys.Date(), format="\%Y-\%m-\%d"))
}
\references{
  U.S. Geological Survey, 2012a, National Water Information
  System: Mapper, accessed September 7, 2012, at
  \url{http://wdr.water.usgs.gov/nwisgmap/}.

  U.S. Geological Survey, 2012b, Parameter code definition,
  National Water Information System: Web Interface,
  accessed September 7, 2012, at
  \url{http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes}.

  U.S. Geological Survey, 2012c, Stat codes (stat_cd),
  National Water Information System: Web Interface,
  accessed September 7, 2012, at
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}.

  U.S. Geological Survey, 2012d, USGS site web service:
  REST Web Services, accessed September 7, 2012, at
  \url{http://waterservices.usgs.gov/rest/Site-Service.html}.

  U.S. Geological Survey, 2012e, USGS surface-water daily
  data for the Nation: National Water Information System:
  Web Interface, accessed September 7, 2012, at
  \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
}
\keyword{utilities}

