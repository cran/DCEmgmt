% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DCEmgmt.R
\name{DCEmgmt}
\alias{DCEmgmt}
\title{DCEmgmt function}
\usage{
DCEmgmt(data, droprow, keepvar, create.id, blocks, sets, alts, options, design)
}
\arguments{
\item{data}{Data.frame with the survey data in wide data format. See 'data(survey)'}

\item{droprow}{Rows to be deleted. Specify a single row (droprow = x) or more than one (dropdown = x:y).}

\item{keepvar}{Select the case-specific variables to be kept. For example: keepvar = c("x", "y").}

\item{create.id}{Create a variable called "id" with a unique identificator per respondent. create.id = TRUE}

\item{blocks}{Specifies the variables designating each choice set block. Each choice set variable must be named
as a letter (for instance Q) followed by a number (starting by 1). Thus, the first block of choice sets can be
named Q1, Q2, Q3, Q4 whereas the second can be called F5, F6, F7, F8. In this case, block = c("Q", "F"). If there
is only one block, block = c("Q").}

\item{sets}{Specify the number of choice sets per block. If there is only one block, specify the total number
of choice sets. For instance, sets = 8.}

\item{alts}{Specify the number of alternatives per choice set. For example, alts = 3.}

\item{options}{Vector indicating the string used to designate the choice. For example, option = c("Option A", "Option B", "Option C"). See examples.}

\item{design}{Data frame with the design matrix in dummy or effects coding. PONER EJEMPLO}
}
\description{
Prepare the results of a DCE to be analysed through choice models.
}
\examples{
data(survey)
data(design)

data.c<-DCEmgmt(data = data,
        droprow = 1:5,
        keepvar = c("age", "height"),
        create.id = TRUE,
        blocks = c("B1_", "B2_"),
        sets = 8,
        alts = 2,
        options = c("Prefiero el servicio A", "Prefiero el servicio B"),
        design = design)

#The data frame data.c contains the coded DCE data
}
