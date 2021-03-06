% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selection.R
\name{selection}
\alias{selection}
\title{Identify the case/control based on ICD9 codes or Phecodes criteria}
\usage{
selection(pheno_dat, code_case, code_control, icd9_phe)
}
\arguments{
\item{code_case}{ICD9 codes or Phecodes used to define cases}

\item{code_control}{ICD9 codes or Phecodes used for exclusion of controls}

\item{icd9_phe}{Parameter to define whether the criteria decided by ICD8 codes or Phecodes}

\item{pheno_data}{Phenotype data(sparse matrix)}

\item{demo_data}{Demographics data}
}
\description{
Identify the case/control based on ICD9 codes or Phecodes criteria
}
\examples{
selection("GRID_ICD9.Aug2017.pivot.RData",c("238.72","238.73","238.74","238.75"),
                         c("238.72","238.73","238.74","238.75","238.76","205.10","205.11","205.12",
                           "238.71","238.4","205","205.0","205.00","205.01","205.02","205.2","205.20","205.21",
                           "205.22","205.8","205.80","205.81","205.82","205.9","205.92","V10.62","40.0",
                           "41.04","41.05","41.06","41.07","41.08","996.88","V42.82"),"icd9")
}
