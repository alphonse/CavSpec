\name{batchImport}
\alias{batchImport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a Spectral Files
}
\description{
Import spectrum from Ocean Optics SpectraSuite output text file.  Imports all text files in a folder.}
\usage{
batchImport(sample.path, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample.path}{
The folder containing the spectra to import
}
  \item{...}{Other arguments passed to \code{\link{read.table}}}
}

\value{
A data frame containing the spectra saved in the given directory with column 1 corresponding to the file name.
}

\author{
Al Fischer <al@uga.edu>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spectrum }
\keyword{ Ocean Optics }
\keyword{ spectrophotometer }
