#' Real estate decision matrix on four alternatives.
#'
#' A real estate data matrix pre-loaded in madmR format. The variables are as follows:
#'
#' \itemize{
#'   \item Sq.Foot. The first attribute indicating how much Sq. Foot each real estate alternative has in compeition with one another. A higher square foot is considered better in this data.
#'   \item Preference. The second attribute indicating how well each real estate alternative is esteemed in the community. (0.00-5.00)
#'   \item PriceCost. The third attribute indicating the cost of each real estate alternative.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name maut_dm
#' @usage data(maut_dm)
#' @format A data frame with 5 rows and 3 variables. The first row indicates the weight of each column name. The rows represent rivaling alternatives.
NULL