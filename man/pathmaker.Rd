% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathmaker.R
\name{pathmaker}
\alias{pathmaker}
\title{Title Create output path/folder for loaded dataset}
\usage{
pathmaker(out.path, path1, path2 = NULL)
}
\arguments{
\item{out.path}{Output directory}

\item{path1}{Subfolder of output}

\item{path2}{Subsubfolder of output}
}
\value{
Return output path
}
\description{
Title Create output path/folder for loaded dataset
}
\examples{
pathmaker(out.path,Dataset_Names[i])
pathmaker(out.path,Dataset_Names[i], paste("/",marker,"/caos",sep=""))
}
