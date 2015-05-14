\name{refPlot}
\alias{refPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Reflectivity Spectrum
}
\description{
Create an interactive plot of mirror reflectivity spectra.
}
\usage{
refPlot(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A data frame containing the columns 'R' (reflectivity values), 'lambda' (wavelengths), and 'Gases' (the gas combination used for the measurement).}
}
\value{
A plot of reflectivity vs. wavelength.
}
\author{
Al Fischer <al@uga.edu>
}

\seealso{
\code{link{intPlot}}
\code{link{reflect}}
\code{link{reflectSpec}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reflectivity }
