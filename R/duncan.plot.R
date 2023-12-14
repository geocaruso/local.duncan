#' duncan.plot.R
#' Produces a duncan.scatterplot (a bit similar to a Moran scatterplot) where
#' x-axis is the calculated share of group A in A+B for each spatial unit
#'  and y-axis is the local duncan index for each spatial unit
#' Plus a horizontal line for the global duncan
#'  and a vertical line for the overall share of A in A+B over the entire area

#' @param local.duncan.sf sf output of a local.duncan function (i.e. with keep.sf=TRUE)
#' @param PopA character name of column with population A
#' @param PopB character name of column with population B
#'
#' @return a ggplot
#' @export
#'
#' @examples

duncan.plot<-function(local.duncan.sf,PopA,PopB){
  df<-sf::st_drop_geometry(local.duncan.sf)
  A<-df[,PopA]
  B<-df[,PopB]
  df$shA<-A/(A+B)
  ggplot()+theme_bw()+
    geom_point(data=df, aes(x=shA, y=local.duncan, col=local.duncan))+
    scale_color_viridis_c(option = "turbo")+
    geom_hline(yintercept=global.duncan(local.duncan.sf,PopA,PopB), col="red")+
    geom_vline(xintercept=sum(A)/(sum(A)+sum(B)), col="blue")+
    coord_cartesian(xlim=c(0,1), ylim=c(0,1))
}
