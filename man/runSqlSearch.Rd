\name{runSqlSearch}
\alias{runSqlSearch}
\title{
runSqlSearch method
}
\description{
Sends a SQL query to Skyserver web services and retrieves the result table as a dataframe.
}
\usage{runSqlSearch(sql,limit,token)}
\arguments{
  \item{sql}{a string containing the sql query}
  \item{limit}{maximum number of rows in the result table}
  \item{token}{Sciserver's authentication token for the user}
}

\details{
no detailes required
}
\value{ the returned object is of type 'dataframe'.}

\author{
Manuchehr Taghizadeh-Popp
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\references{
No references for now.
}
\examples{
dt <- runSqlSearch(sql="select 1")
}
\keyword{sciserver}