\name{intPlot}
\alias{intPlot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an Intensity Spectrum
}
\description{
Create an interactive plot of intensity spectra.
}
\usage{
intPlot(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A data frame containing the columns 'I' (intensity values), 'lambda' (wavelengths), and 'Gas' (the gas used for the measurement).}
}
\value{
A plot of intensity vs. wavelength.
}
\author{
Al Fischer <al@uga.edu>
}

\seealso{
\code{link{refPlot}}
\code{link{setRange}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spectrum }

