\name{plotConnectivityMatrix.dataframe}
\alias{plotConnectivityMatrix.dataframe}
\title{Plot a connectivity matrix from a dataframe.}
\usage{
plotConnectivityMatrix.dataframe(tbl, zmax = NULL, zmin = 0, title = NULL,
  xlab = xlab, ylab = ylab, devtype = "win", cex = 1 * (devtype ==
  "win") + 1.5 * (devtype != "win"), colorscale = c("hot", "cold", "coldhot"),
  plotfile = "ConnectivityMatrix.png", plotsize = c(970, 780))
}
\arguments{
  \item{tbl}{- dataframe or matrix representation of a
  connectivity matrix (can be NULL)}

  \item{csvfile}{- csv file to read connectivity matrix
  from (can be NULL)}

  \item{zmax}{- max value for color scale}

  \item{zmin}{- min value for color scale}

  \item{title}{- title for plot}

  \item{xlab}{- label for x axis}

  \item{ylab}{- label for y axis}

  \item{cex}{- character expansion factor for plot}

  \item{colorscale}{- color scale to use for plot ('hot',
  'cold' or 'coldhot')}

  \item{plotfile}{- file to save plot to (if devtype !=
  'win')}

  \item{plotsize}{- 2 element vector giving plot size in
  pixels (c(W,H))}

  \item{devtype}{- device type ('win' or 'png')}
}
\description{
Plot a connectivity matrix from a dataframe.
}
\details{

}

