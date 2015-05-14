\name{setRange}
\alias{setRange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set Enhanced Regions
}
\description{
Set all non-enhanced data in a cavity-enhanced spectrum to 'NA'}
\usage{
setRange(X, xmins=c(350, 415, 515, 660), xmaxs=c(400, 465, 565, 700))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A data frame containg the spectra to filter.
}
  \item{xmins}{A numeric vector specifying the minimum values for each range}
  \teim{xmaxs}{A numeric vector specifying the maximum values for each range}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame identical to \code{X} but with values for I outside of the specified ranges set to 'NA'.
}

\author{
Al Fischer <al@uga.edu>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
