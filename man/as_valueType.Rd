% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08-catalogue_functions.R
\name{as_valueType}
\alias{as_valueType}
\title{Apply all the valueTypes of a data dictionary to a dataset}
\usage{
as_valueType(x, valueType = NA_character_)
}
\arguments{
\item{x}{A xxx xxx}

\item{valueType}{xxx xxx}
}
\value{
The dataset with the data dictionary valueType
applied to each variable
}
\description{
Uses a data dictionary in the Maelstrom Research formats (with "Variables" and "Categories"
in separate tibbles and standard columns in each) to apply their valueType to a dataset in tibble format.
If no data dictionary is provided, the function will automatically evaluate the most restrictive valueType
for each variable in the dataset and apply it.
}
\details{
Must provide an input dataset in tibble format
}
\examples{
\dontrun{
# use case 1: Apply valueType without specifying a data dictionary
as_valueType("1") \%>\% typeof()

tibble(iris \%>\% mutate(Species = as.character(Species))) \%>\%
  mutate(across(c(starts_with("Sepal")), ~ as_valueType(.,"integer")))
}

}
