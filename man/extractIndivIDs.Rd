\name{extractIndivIDs}
\alias{extractIndivIDs}
\title{Function to extract unique IDs for individuals from a DisMELS model run.}
\usage{
extractIndivIDs(indivConn = NULL, onlySuccessful = FALSE,
  nurseryZones = c("NurseryArea_000to050m", "NurseryArea_050to150m"),
  lhsTypeInfo = NULL)
}
\arguments{
  \item{indivConn}{= individual connectivity file (or
  dataframe)}

  \item{onlySuccessful}{= flag (T/F) to pull out only
  successful individuals}

  \item{nurseryZones}{= vector of names of zones used as
  nursery areas in the IBM (req'd if onlySuccessful=TRUE)}

  \item{lhsTypeInfo}{= life stage info list for IBM (req'd
  if onlySuccessful=TRUE)}
}
\value{
data frame with single column (ID) of unique ids.
}
\description{

}
\details{
If onlySuccessful=TRUE, then it is assumed that
'successful' individuals are those which are in one of the
nursery areas in the final life stage.
}

