\name{executeQuery}
\alias{executeQuery}
\title{
executeQuery method
}
\description{
Sends a SQL query to Skyserver web services and retrieves the result table as a dataframe.
}
\usage{executeQuery(sql,context,token)}
\arguments{
  \item{sql}{a string containing the sql query}
  \item{context}{Database context where the sql query is executed}
  \item{token}{Sciserver's authentication token for the user}
}

\details{
no detailes required
}
\value{ the returned object is of type 'dataframe'.}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\references{
No references for now.
}
\examples{
dt <- executeQuery(sql="select 1")
}
\keyword{sciserver}