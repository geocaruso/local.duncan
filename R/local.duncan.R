#' local.duncan.R
#'
#' @param sf input polygon support sf
#' @param PopA character name of column with population A
#' @param PopB character name of column with population B
#' @param nb neighbourhood list object. I believe should ideally
#'  comprise itself conversely to most spatial lag cases (local moran etc.).
#'  Typicall result of a st_intersects on sf itself or spdep::poly2nb
#' @param keep.sf to append result in the sf data.frame part.
#'  Otherwise a separated data.frame with one column is returned.
#'  Defaulted to TRUE which includes plotting the sf with local.moran values
#'  and global moran for comparison in the title
#'
#' @return Duncan index for each polygon unit based on its local context defined by nb.
#' @export
#'
#' @examples
local.duncan<-function(sf,PopA,PopB,nb,keep.sf=TRUE){
  df<-sf::st_drop_geometry(sf)
  global.duncan<-duncan(df,PopA,PopB)

  localdf<-lapply(nb,function(x){df[x,c(PopA,PopB)]})
  ld<-lapply(localdf,function(x){duncan(x,PopA,PopB)})
  local.duncan=unlist(ld)
  if(keep.sf==FALSE){
    out<-data.frame(local.duncan=local.duncan)
  } else {
    sf$local.duncan<-local.duncan
    out<-sf
    plot(sf["local.duncan"], main=paste0("local.duncan (global = ",
                                         sprintf("%.3f",global.duncan),")"))
  }
  return(out)
}
