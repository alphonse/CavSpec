\name{CavSpec}
\alias{CavSpec}
\alias{CavSpec}
\docType{package}
\title{
Tools for working with cavity enhanced spectroscopy data
}
\description{
A set of functions designed for processing data from cavity enhanced spectroscopy (BBCES, PSCRDS, and CRDS).  Has functions for calculating mirror reflectivity, total extinction, plotting, and looking up literature spectra from web.

}
\details{
\tabular{ll}{
Package: \tab CavSpec\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-11-18\cr
License: \tab GPL-3\cr
}
l
\tabular{ll}{lit.spectrum  \tab look up literature spectrum from Mainz Spectral Database\cr
plot.all \tab plot columns of data frame by first column (as in spectra set)\cr
reflect \tab calculate mirror reflectivity\cr
extinction \tab caluclate total light extinction\cr
rm.odd \tab  remove odd columns of data frame (for spectrophotometer that outputs wavelength every other column)\cr
}}
\author{
Al Fischer <al@uga.edu>
}
\references{
Vallance, C. (2005). Innovations in cavity ringdown spectroscopy. New Journal of Chemistry, 29(7), 867. doi:10.1039/b504628a   
}

\keyword{ package }
\seealso{
   Optional links to other man pages, e.g.   
   \code{\link[<pkg>:<pkg>-package]{<pkg>}}   
}
\examples{
   I  <- 25000  # sample intensity, arbitrary units
   I0 <- 30000  # blank intensity, arbitrary units
   
   # compute total light extinction
   alpha <- extinction(I, I0, method = 'CEAS')
   
   # or, compute mirror reflectivity with known extinction
   alpha.NO2 <- lit.spectrum('NO2', version = 'Bogumil')
   alpha.NO2 <- mean(subset(alpha, alpha > 532 & alpha < 533))
   r <- reflect(I, I0, method = 'CEAS', alpha = alpha.NO2)
   
   # works for PSCRDS, too
   I0 <- -45  # phi0, degrees
   I <-  -25  # phi(samp), degrees
   alpha <- extinction(I, I0, method = 'PSCRDS')
   
   
}
