\name{ARMA_Optimizado}
\alias{ARMA_Optimizado}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Optimiza el proceso de doble ciclo ARMA
%%  ~~function to do ... ~~
}
\description{
  Esta funcion corre el doble ciclo para encontrar el mejor ARMA de manera paralela en tu computadora haciendo que el tiempo de computo sea entre 50 a 80 porciento mas rapido dependiendo del modelo de tu computadora
}
\usage{
ARMA_Optimizado(final_i,final_j_datos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{final_i}{
Es el ultimo valor al que llegara el parametro p de la busqueda del ciclo ARMA
}
 \item{final_j}{
 Es el ultimo valor al que llegara el parametro q de la busqueda del ciclo ARMA
 }
 \item{Datos}{
 Es la serie de tiempo estacionaria donde se buscara el mejor modelo ARMA
 }
}
\details{
}
\value{

\item{1}{Este es el mejor modelo de todos los revisados}
\item{2}{Este es el AIC del mejor modelo}

}
\references{

}
\author{
Victor Alanis
}
\note{
Aunque el ciclo esta optimizado arriba de 20 sigue tardandose

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(TSA)
library(ParallelAR)
Iniciacion()
data(larain)
c.la=diff(larain)
Arma_optimizado(15,15,c.la)
}

