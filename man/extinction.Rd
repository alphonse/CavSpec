\name{extinction}
\alias{extinction}

\title{
Calculate Total Light Extinction
}

\description{
Calculates total light extinction based on BBCES or PSCRDS measurements.
}

\usage{
extinction(I, I0, method = "BB", R = 0.9998, Rl = 1.33, d = 50.3, f = 100000, bg.ext = 0)
}

\arguments{
  \item{I}{Sample intensity (BB) or phase (PS)}
  \item{I0}{Blank intensity (BB) or phase (PS)}
  \item{method}{PS for phase shift cavity ringdown, CEAS for cavity enhanced absorption}
  \item{R}{Mirror reflectivity}
  \item{Rl}{Ratio of total cavity length to occupied length}
  \item{d}{Total cavity length}
  \item{f}{Modulation frequency, for PS only}
  \item{bg.ext}{Extinction due to blank/I0 gas (usually N2)}
}

\value{
A vector of total light extinction in cm^-1.
}

\author{
Al Fischer <al@uga.edu>
}

\seealso{
\code{\link{reflect}, \link{lit.spectrum}}
}
\examples{
#  set arbitrary I, I0 for example
I  <- 25000  # sample intensity, arbitrary units
I0 <- 30000  # blank intensity, arbitrary units
   
#  compute total light extinction
alpha <- extinction(I, I0, method = 'CEAS')

# works for PSCRDS, too:
I0 <- -45  # phi0, degrees
I <-  -25  # phi(samp), degrees
alpha <- extinction(I, I0, method = 'PSCRDS')
}

\keyword{ light extinction }
\keyword{ cavity enhanced spectroscopy }
