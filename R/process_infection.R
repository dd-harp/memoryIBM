# infection state updates

process_aoi <- function(variables) {
  stopifnot(inherits(x = variables$aoi, what = 'IntegerRaggedVariable'))

  return(
    function(timestep) {
      vals <- variables$aoi$get_values()
      new_vals <- lapply(X = vals, FUN = function(a) {
        if (a[1] == -1L) {
          return(-1L)
        } else {
          return(a + 1L)
        }
      })
      variables$aoi$queue_update(values = new_vals)
    }
  )
}
