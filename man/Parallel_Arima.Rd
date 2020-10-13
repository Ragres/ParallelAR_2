\name{Parallel_Arma}
\alias{Parallel_Arma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parallel_Arma
%%  ~~This function run a parallel cycle ~~
}
\description{
%%  ~~ With 3 parameters final_i (maximun p steps),final_j (maximum q) steps  and data (the time series to use) this function itereates over all the combinations of the p and q's in parallel to find the model with the lowest AIC~~
}
\usage{
Parallel_Arma(final_i,final_j_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{final_i}{
%%     ~~final_i is the maximun p steps on the AR model ~~
}
 \item{final_j}{
%%     ~~final_j is the maximun q steps on the MA model ~~
}
\item{data}{
%%     ~~data is the time series that will be modelled  ~~
}
}
\details{
%%  ~~ ~~
}
\value{
%%  ~it return a list of two elements
%%  \item{comp1}{This is the best model }
%%  \item{comp2 }{This is the AIC of the best model}
%% ...
}
\references{
%% ~ ~
}
\author{
%%  ~~Victor Alanis~~
}
\note{
%%  ~~If aclarations needed please contact me~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
