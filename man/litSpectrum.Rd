\name{litSpectrum}
\alias{litSpectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve Extinction Cross Sections
}
\description{
Retrieves extinction cross sections for select species.  Most come from the Mainz Spectral database, though some are from other references (see References, below).
}
\usage{
litSpectrum(species, version)
}
\arguments{
  \item{species}{
Chemical species for which to retrieve spectrum.  Can be \code{'N2'}, \code{'NO2'}, \code{'He'}, \code{'CO2'}, \code{'O3'}, or \code{'O2'}.
}
\item{version}{
Only used for \code{NO2}, can be \code{'Bogumil'} or \code{'Davidson'}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing wavelength in Column 1 (in nm) and absorption/scattering cross section in Column 2 (in cm^2 molec^-1).
}

\references{

\link{http://satellite.mpic.de/spectral_atlas/cross_sections/}

Washenfelder, R. A. et al. Broadband measurements of aerosol extinction in the ultraviolet spectral region, AMTD 6: 113-157 (2013).

NASA. (1977). Absolute rayleigh scattering cross sections of gases and freons of stratospheric interest in the visible and ultraviolet regions.

}

\author{
Al Fischer <al@uga.edu>
}

\examples{
#  retrieve NO2 spectrum of Bogumil 2003
NO2 <- lit.spectrum('NO2', 'Bogumil')

#  calculate He scattering cross section
He <- lit.spectrum('He')
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
}