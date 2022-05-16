# infection state

#' @title Make Age of Infection (AoI) variable
#' @description
#' @param n size of the population
#' @param variables a list
#' @export
make_AoI <- function(n, variables = NULL) {
  aoi <- IntegerRaggedVariable$new(initial_values = as.list(rep(-1, n)))
  if (!is.null(variables)) {
    return(list("aoi" = aoi))
  } else {
    variables$aoi <- aoi
    return(variables)
  }
}

#' @title Get Multiplicity of Infection (MoI)
#' @description
#' @param variables a list
#' @export
get_MoI <- function(variables) {
  return(
    vapply(X = variables$aoi$get_values(), FUN = function(a) {
      if (a[1] == -1L) {
        return(0)
      } else {
        return(length(a))
      }
    }, FUN.VALUE = integer(1), USE.NAMES = FALSE)
  )
}
