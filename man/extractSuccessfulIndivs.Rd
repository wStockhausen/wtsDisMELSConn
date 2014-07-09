\name{extractSuccessfulIndivs}
\alias{extractSuccessfulIndivs}
\title{Function to extract tracks of successful individuals from a DisMELS model run.}
\usage{
extractSuccessfulIndivs(indivConn = NULL, results = NULL,
  newResType = FALSE, nurseryZones = c("NurseryArea_000to050m",
  "NurseryArea_050to150m"), lhsTypeInfo = NULL, outDir = "./",
  outBaseCSV = "successfulIndivs")
}
\arguments{
  \item{indivConn}{= individual connectivity file (or
  dataframe)}

  \item{results}{= DisMELS results file (or dataframe)}

  \item{newResType}{- flag (T/F) indicating whether (T) or
  not (F) results are from a "new"-type IBM}

  \item{nurseryZones}{- vector of names of zones used as
  nursery areas in the IBM}

  \item{lhsTypeInfo}{= life stage info list for IBM}

  \item{stdVars}{= vector of standard model variables}

  \item{outDir}{= folder for output}

  \item{outBaseCSV}{= base name for output csv file}
}
\description{

}

