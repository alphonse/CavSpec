\name{reflectSpec}
\alias{reflectSpec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate a Reflectivity Spectrum
}
\description{
Calculate mirror reflectivity spectra for cavity enhanced data
}
\usage{
reflectSpec(refData, litData, Rmethod = 'HeCO2', correction = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{refData}{
The intensity for the measurement imported with \code{\link{batchImport()}}.
}
  \item{refLit}{A data frame of literature values for the gas extinction in cm^-1 from \code{\link{reflit()}}}
  \item{Rmethod}{A charcter string specifying combination of gases to use; can be 'HeCO2', 'HeN2', 'N2CO2', or 'All'}
  \item{correction}{A numeric scaling factor added to the reflectivity value.}
}

\value{
A data frame containig the mirror reflectivity spectra.
}
\references{
Washenfelder, R. a., Flores, J. M., Brock, C. a., Brown, S. S., & Rudich, Y. (2013). Broadband measurements of aerosol extinction in the ultraviolet spectral region. Atmospheric Measurement Techniques, 6(4), 861–877. doi:10.5194/amt-6-861-2013}
\author{
Al Fischer <al@uga.edu>
}

\seealso{
\code{\link{batchImport()}}
\code{\link{reflit()}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
