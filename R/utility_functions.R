#' Relate one set of strata to another
#'
#' Takes two strata definition tables (e.g. StrataSpecht and StrataCERMB) and
#' returns a list where the number of elements is equal to the number of strata
#' in the first set (\code{strata1}), and each element is a vector with indices
#' for strata in the second set (\code{strata2}). See example below for more
#' detail.
#'
#' @param strata1 A data frame of strata definitions. Usually this should be
#'   the set with the smaller number of strata.
#'
#' @param strata2 A data frame of strata definitions. Usually this should be
#'   the set with the larger number of strata.
#'
#' @return A named list, where element names are taken from \code{strata1}
#'   and each element is a vector of the indices of the related strata in
#'   \code{strata2}.
#'
#' @examples
#' # Relate Specht strata to CERMB database strata (50cm vertical divisions)
#' # The resulting lookup list will have an element for each Specht stratum.
#' # Each element is a vector of integer indices of CERMB strata.
#' #
#' lookup <- relate_strata(CERMBlidar::StrataSpecht, CERMBlidar::StrataCERMB)
#'
#' # Which 'StrataCERMB' strata relate to the second (low shrub) layer
#' # in 'StrataSpecht'?
#' lookup[[2]]
#'
#' @export
#'
relate_strata <- function(strata1, strata2) {
  strata1 <- CERMBlidar::check_strata(strata1)
  strata2 <- CERMBlidar::check_strata(strata2)

  n1 <- nrow(strata1)
  n2 <- nrow(strata2)
  Eps <- 1e-6

  relations <- vector("list", n1)
  for (i1 in 1:n1) {
    lims1 <- c(strata1$lower[i1], strata1$upper[i1])
    k <- ifelse(i1 == 1, 1, 1 + tail(relations[[i1-1]], 1))
    ii <- sapply(k:n2, function(i2) {
      .overlap(lims1, c(strata2$lower[i2] + Eps, strata2$upper[i2]))
    })
    relations[[i1]] <- (k-1) + which(ii)
  }

  names(relations) <- strata1$name

  relations
}

.within <- function(x, lims) {
  lims <- sort(lims)
  x >= lims[1] && x < lims[2]
}

.overlap <- function(lims1, lims2) {
  .within(lims1[1], lims2) ||
    .within(lims1[2], lims2) ||
    .within(lims2[1], lims1) ||
    .within(lims2[2], lims1)
}
