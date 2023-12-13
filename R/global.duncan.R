#' global.duncan.R
#' A wrapper of duncan.R and dropping geometry of an sf input
#'

#' @param sf a simple feature access (sf) dataset
#' @param PopA character name of column with population A
#' @param PopB character name of column with population B
#'
#' @return Duncan index. A numeric.
#' @export
#'
#' @examples

global.duncan<-function(sf,PopA,PopB){
  df<-sf::st_drop_geometry(sf) #
  duncan(df,PopA,PopB)
}
