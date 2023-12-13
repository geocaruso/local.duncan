#' nbpoly2sflines.R
#'
#' Assuming a polygon sf and a nb list similarly ordered, make an sf line of the
#' segment linking the centroid of each spatial unit to its neighbour
#' @param sf a sf polygon support input
#' @param nb a neighbourhood list (nb) object (see spdep)
#'
#' @return a sf line of the linking segments
#' @export
#'
#' @examples
#'
nbpoly2sflines<-function(sf,nb){
  sf$nb.lst<-nb
  sf$i.sf<-rownames(sf)
  sf$X<-sf::st_coordinates(sf::st_centroid(sf))[,"X"]
  sf$Y<-sf::st_coordinates(sf::st_centroid(sf))[,"Y"]

  #nb list in long format: each link becoming a record
  #DOES NOT SEEM TO WORK IF EVERYONE IS EVERYONE's NEIGHBOUR. NOT SURE WHY YET.
  #Must be checked I expet it can be used to check global index is then found everywhere
  long.nb<-data.frame(
    i=unlist(apply(sf,1,function(x){rep(x$i.sf,length(x$nb.lst))})),
    nb=unlist(sf$nb.lst))

  #merge xy of destinations (nb)
  DestinM<-merge(long.nb,sf::st_drop_geometry(sf[,c("i.sf","X","Y")]), by.x="nb", by.y="i.sf", all=TRUE, sort=FALSE,)
  names(DestinM)[c(3,4)]<-c("Xnb","Ynb")

  #merge xy of origins (i)
  OriginDestM<-merge(DestinM,sf::st_drop_geometry(sf[,c("i.sf","X","Y")]), by.x="i", by.y="i.sf", all=TRUE, sort=FALSE)
  names(OriginDestM)[c(5,6)]<-c("Xi","Yi")

  #Make a line geometry and sf
  long.nb$geom<-sprintf("LINESTRING(%s %s, %s %s)",
                        OriginDestM$Xi, OriginDestM$Yi, OriginDestM$Xnb, OriginDestM$Ynb)
  sf_nb_lines<-sf::st_as_sf(long.nb, wkt = "geom")

  return(sf_nb_lines)
}
