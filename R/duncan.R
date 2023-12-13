#' duncan.R
#' Computes Duncan segregation index from 2 populations A and B available within
#' data.frame. The index being symmetric (absolute value of difference) swapping
#' A or B doesn't matter
#'
#' @param df a data.frame
#' @param PopA character name of column with population A
#' @param PopB character name of column with population B
#'
#' @return Duncan index. A numeric.
#' @export
#'
#' @examples

duncan<-function(df,PopA,PopB){
  A<-df[,PopA]
  B<-df[,PopB]
  a<-A/sum(A)
  b<-B/sum(B)
  diff_ab<-a-b
  D<-sum(abs(diff_ab))/2
  D
}
