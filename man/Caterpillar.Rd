\name{Caterpillar}
\alias{Caterpillar}
\title{
Caterpillar Plots
}
\description{
	Creates caterpillar plots from the output of \code{A3Model} and \code{A5Model}. 
	Loops over a values i:j and all items in the list supplied to create caterpillar plots illustrating parameter 
	values ai:aj for all species in the list. Caterpillar plots are described further in the guidance 
	documentation: "Guide to List Length Analysis and the Liszt package". \code{references} below.  
}
\usage{
Caterpillar(data, avals = 1:5, nPerPage = 40)
}
\arguments{
  \item{data}{
	Data with which to make caterpillar plots. Must be a list with a table including parameter estimates and 
	summaryinformation for all parameter values specified. Designed to be used directly with the output of 
	\code{extract}. 
}
  \item{avals}{
	Numeric values between 1 and 5, specifies which parameter values are to be plotted. 
	By default 1:5. You will need to specify 1:3 for use with the output of \code{A5Model}	
      }
 \item{nPerPage}{Number of items to be plotted on one image.}
}
\value{
	Returns caterpillar plots for each of the specified parameter values. 
}
\references{
  Barnes, M and Szabo, JK. Guide to List Length Analysis and the Liszt package. 
  Available Online at: \url{http://www.edg.org.au/free-tools/LLA.html}

  Further guidance on using this package and associated functions and list length analysis in general
  is available at: \url{http://www.edg.org.au/free-tools/listlength.html}
}

\author{
	Megan Barnes <megan.barnes@uq.edu.au>
}

\examples{
\dontrun{
	Caterpillar(data = res, avals = 1:5)  ## needs to be given res
}
}

\keyword{ plots }
