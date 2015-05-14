\name{reflit}
\alias{reflit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Literature Values for Reflectivity Measurement
}
\description{
Gets Rayleigh scattering cross sections for He, N2, and CO2, and converts them into extinction based on Loschmidt's constant.}
\usage{
reflit(lambda, T = 273, p = 101325)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{A numeric vector containing the wavelengths for which data is desired}
  \item{T}{
The sample temperature in Kelvin
}
  \item{p}{The sample pressure in Pascals}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame of Rayliegh extinction for He, N2, and CO2.
}
\references{
\link{http://satellite.mpic.de/spectral_atlas/cross_sections/}
\link{http://en.wikipedia.org/wiki/Loschmidt_constant}
}
\author{
Al Fischer <al@uga.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{litSpectrum}}
\code{\link{reflect}}
}

\keyword{ Rayleigh Scattering }