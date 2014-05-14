\name{calcConnectivityMatrices}
\alias{calcConnectivityMatrices}
\title{Calculate connectivity matrices for a set of DisMELS model runs.}
\usage{

  calcConnectivityMatrices(resDir = "C:\\\\Projects\\\\GOA_IERP\\\\IBM_Runs\\\\ATF\\\\FullSeries",
  connResBase = "ConnYYYYMMDD.csv", cellsTbl = NULL,
  cellsFile = file.path(resDir, "ATF_ClassifiedCGOAGridCells.csv"),
  years = as.character(1996:2011), months = c("01"), days = c("01"),
  spawningZones = c("SpawningArea_300to600m"),
  nurseryZones = c("NurseryArea_000to050m", "NurseryArea_050to150m"),
  lifeStages = c("egg01", "small.yolk.sac.larva", "large.yolk.sac.larva",
  "small.feeding.preflexion.larva", "large.feeding.preflexion.larva",
  "postflexion.larva", "settlement.stage.larva", "benthic.juvenile"))
}
\arguments{
  \item{resDir}{- path to base directory for DisMELS
  results files}

  \item{connResBase}{- file name template for connectivity
  results files from DisMELS (e.g., "ConnYYYYMMDD.csv")}

  \item{cellsTbl}{- dataframe based on classified grid
  cells file (can be NULL)}

  \item{cellsFile}{- name of classified grid cells file
  (can be NULL)}

  \item{years}{- vector of years corresponding to model
  simulation names (YYYY)}

  \item{months}{- vector of months corresponding to model
  simulation names (MM)}

  \item{days}{- vector of days corresponding to model
  simulation names (DD)}

  \item{spawningZones}{- vector of names of zones used as
  spawning areas in the IBM}

  \item{nurseryZones}{- vector of names of zones used as
  nursery areas in the IBM}

  \item{lifeStages}{- vector of names of life stages in the
  IBM}
}
\value{
list w/ 2 elements:\cr allRes - list of connectivity matrix
results\cr zmax - max connectivity value (useful for
scaling plots)
}
\description{
Function to calculate a set of connectivity matrices
corresponding to a set of DisMELS model runs.
}
\details{


If the 'cellsTbl' dataframe is given, it will be used
rather than the cellsFile. If cellsTbl is NULL, the
cellsFile will be read. If cellsFile is also null, the user
can select the classified grid cells file using a file
dialog.\cr\cr

Connectivity results files from the DisMELS model runs
should be of the form "ConnYYYYMMDD.csv", where "Conn" is
arbitrary (but onsiistent among results files) and YYYY
indicates the year (if applicable), MM the month (if
applicable), and DD the day (if applicable) for a
particular model run. If YYYY, MM, or DD is **not** part of
the file name, use "" as the function input for the
corresponding years, months, or days.
}

