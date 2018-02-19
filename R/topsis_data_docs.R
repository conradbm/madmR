#' Marketing growth decision matrix on six alternatives.
#'
#' A marketing growth data matrix pre-loaded in madmR format. The variables are as follows:
#'
#' \itemize{
#'   \item AbilitytoMarket. How well the marketing plan is expected to be marketed in a alternative's geographic area. (0.00-5.00)
#'   \item MarketPotential. How well the marketing plan is projected to succeed in a given geographic area. (0.00-5.00)
#'   \item NumberofCompetitorsAttacked. How many competitors will attack the marketing plan in a given geographic area. (0.00-5.00)
#'   \item CompetitiveBarriersCost. How well the marketing plan is able to break through competitor barriers (0.00-5.00)
#'   \item IntensityofCompetitorsCost How extreme compeitors are likely to be toward our marketing plan in a given geographic area. (0.00-5.00)
#'   \item PaybackPeriodCost. How much we will have to pay back per period in % in a given geopgraphic area. (%increase)
#'   \item ROI. Expected return on interest for our marketing plan in a given geopgraphic area. ($)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name topsis_dm
#' @usage data(topsis_dm)
#' @format A data frame with 7 rows and 7 variables. The first row indicates the weight of each column name. The rows represent rivaling marketing geographic areas.
NULL