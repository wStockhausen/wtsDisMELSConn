\name{unzipDisMELSResults}
\alias{unzipDisMELSResults}
\title{Unzips and renames a series of DisMELS results or connectivity files.}
\usage{
unzipDisMELSResults(dir.inp = getwd(), dir.out = getwd(), YYYY = NULL,
  MM = NULL, DD = NULL, basename = "Results")
}
\arguments{
  \item{dir.inp}{= folder containing the zip files}

  \item{dir.out}{= folder where unzipped files will be
  written}

  \item{YYYY}{= years to extract (can be NULL)}

  \item{MM}{= months to extract (can be NULL)}

  \item{DD}{= days to extract (can be NULL)}

  \item{basename}{= base name for zipped/unzipped files}
}
\description{
Function to unzip and rename a series of DisMELS results or
connectivity files.
}
\details{


This function assumes zip files are named something like
'basenameYYYYMMDD.csv.zip' and that the associated zipped
csv file is 'basename.csv'. When the csv file is unzipped
in the folder specified by dir.out, it will be renamed
'basenameYYYYMMDD.csv'. Note that at least one of YYYY, MM,
and DD must be a non-null vector.
}

