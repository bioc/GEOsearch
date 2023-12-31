% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/SampleDetail.R
\name{SampleDetail}
\alias{SampleDetail}
\title{SampleDetail}
\usage{
SampleDetail(GSEid)
}
\arguments{
\item{GSEid}{A character vector of GSE accession ID.}
}
\value{
A data frame containing the search results returned from NCBI GEO. First column: GEO Series Accesion Number; Second column: GEO Sample Accesion Number; Third column: Title; Fourth column: Type of experiment; Fifth column: Source; Sixth column: Organism; Seventh column: Characteristic; Eighth column: Description.
}
\description{
Details of GSM samples given GSE accession ID.
}
\details{
This function returns an integrated table containing details of all GSM samples for a list of GSE accession ID.
}
\examples{
SampleDetail(c("GSE69322","GSE64008"))
}
\author{
Zhicheng Ji, Hongkai Ji <zji4@zji4.edu>
}

