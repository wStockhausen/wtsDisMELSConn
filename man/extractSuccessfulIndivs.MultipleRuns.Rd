\name{extractSuccessfulIndivs.MultipleRuns}
\alias{extractSuccessfulIndivs.MultipleRuns}
\title{Extracts and re-orders a series of DisMELS model results files for only successful individuals.}
\usage{
extractSuccessfulIndivs.MultipleRuns(YYYYs = NULL, MMs = NULL, DDs = NULL,
  newResType = FALSE, nurseryZones = c("NurseryArea_000to050m",
  "NurseryArea_050to150m"), lhsTypeInfo = NULL, inpDir.IndivConn = NULL,
  basename.IndivConn = "IndivConn", inpDir.Results = NULL,
  basename.Results = "Results", outDir = NULL,
  basename.Out = "SuccessfulIndivs")
}
\arguments{
  \item{YYYYs}{= years to extract (can be NULL)}

  \item{MMs}{= months to extract (can be NULL)}

  \item{DDs}{= days to extract (can be NULL)}

  \item{newResType}{- flag (T/F) indicating whether (T) or
  not (F) results are from a "new"-type IBM}

  \item{nurseryZones}{= vector of nursery area names}

  \item{inpDir.IndivConn}{= folder with input individual
  connectivity matrix files}

  \item{basename.IndivConn}{= base name for individual
  connectivity matrix files}

  \item{inpDir.Results}{= folder with input DisMELS model
  results files}

  \item{basename.Resuls}{= base name for DisMELS model
  results files}

  \item{outDir}{= output folder}

  \item{basename.Out}{= base name for output files}
}
\description{
Function to xtracts and re-orders a series of DisMELS model
results for only successful individuals.
}
\details{


This function assumes files are named something like
'basenameYYYYMMDD.csv'. Note that at least one of YYYYs,
MMs, and DDs must be a non-null vector.
}

