#Classifier: Subfunction 1 of "get_pos_diagnostics" ----------------------------------------

#Split vector into chunks
#' Title Turn vector into branch separated list
#'
#' @param x CA_ref
#' @param n branch_nr
#'
#' @return Return branch separated list
#' @export
#'
#' @examples chunk(CA_ref,branch_nr)
chunk <- function(x,n){split(x, cut(seq_along(x), n, labels = FALSE))}

#-------------------------------------------------------------------------------
