\name{siteInfo}
\alias{siteInfo}
\title{Retrieve site information}
\format{a data frame with the following columns:
  \tabular{llll}{
Name \tab Type \tab Description \cr
staid \tab factor \tab USGS station identification number \cr
staname \tab character \tab USGS station name \cr
lat \tab numeric \tab Decimal latitude \cr
lng \tab numeric \tab Decimal longitude
}}
\usage{
  siteInfo(staid)
}
\arguments{
  \item{staid}{is a character vector containing USGS site
  identification number(s).  USGS site numbers are usually
  eight digits long, but can be longer.  Users may search
  for surface-water sites and obtain station identification
  numbers using the USGS Site Web Service,
  \url{http://waterservices.usgs.gov/rest/Site-Service.html}
  (U.S. Geological Survey, 2012b); using the National Water
  Information System: Mapper,
  \url{http://wdr.water.usgs.gov/nwisgmap/} (U.S.
  Geological Survey, 2012a); or using the National Water
  Information System: Web Interface to daily surface-water
  data,
  \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}
  (U.S. Geological Survey, 2012c).  The site identification
  number needs to be entered as a character, that is in
  quotes, because many USGS streamgage numbers begin with
  zero and the leading zero is necessary.}
}
\value{
  a data frame containing the station identification
  number(s), the USGS streamgage name(s), the decimal
  latitude(s), and decimal longitude(s).
}
\description{
  Function to retrieve information about a USGS streamgage
  site
}
\details{
  This provides some limited metadata about the USGS
  streamgage site.
}
\note{
  Information retrieved using this function can be used to
  create a map of multiple streamgage sites---see package
  vignette.
}
\examples{
 \dontrun{
staInfo <- siteInfo("05054000")
staInfo
staInfo <- siteInfo(c("05054000", "05082500", "06342500"))
staInfo
# a list with an invalid station identification number
staInfo <- siteInfo(c("05054000", "05082500", "06342501"))
  }
}
\references{
  U.S. Geological Survey, 2012a, National Water Information
  System: Mapper, accessed September 7, 2012, at
  \url{http://wdr.water.usgs.gov/nwisgmap/}.

  U.S. Geological Survey, 2012b, USGS site web service:
  REST Web Services, accessed September 7, 2012, at
  \url{http://waterservices.usgs.gov/rest/Site-Service.html}.

  U.S. Geological Survey, 2012c, USGS surface-water daily
  data for the Nation: National Water Information System:
  Web Interface, accessed September 7, 2012, at
  \url{http://waterdata.usgs.gov/nwis/dv/?referred_module=sw}.
}
\keyword{datagen}

