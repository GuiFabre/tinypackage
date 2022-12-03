#' List of valueType in a tibble format according Maelstrom standards
#'
#' This function gives the list of authorized valueType values, as described in
#' opal.
#'
#' @format ## `tibble`
#' A data frame with 12 rows and 5 columns:
#' \describe{
#'   \item{valueType}{data type as described in Opal}
#'   \item{typeof}{data type provided by base::typeof()}
#'   \item{class}{data class provided by base::class}
#'   \item{call}{function to transpose object according do.call() function}
#'   \item{toValueType}{data type as described in Opal}
#'   ...
#' }
#' @source xxx
"valueType_list"


