#Test script to adapt result of as.nb.sgbp (after st_intersects) so it includes or removes self
# and attributes (self.included) made similar to spdep include/remove self
# For mixed cases (some self are present some other are absent), if include or
# remove is not requested then adds an attribute (self.is.in) indicating the elements
#  where self is present.
#
#  nb.self could actually be integrated within a function with as.nb.sgbp since
#  it only makes sense for the sgbp based flow not the spdep::poly2nb


##DATA
#4x4 polygon case
x1<-rep((1:4),each=4)
y1<-rep((1:4),4)
df<-data.frame(id=paste(x1,y1,sep="_"))
df$geom<-sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s, %s %s))",
                 x1, y1, x1, y1+1, x1+1, y1+1, x1+1, y1, x1, y1)
sf<-sf::st_as_sf(df, wkt = "geom")
plot(sf)

#Add a no-neighbour polygon (except for itself)
x1<-6
y1<-6
df2<-data.frame(id=paste(x1,y1,sep="_"))
df2$geom<-sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s, %s %s))",
                 x1, y1, x1, y1+1, x1+1, y1+1, x1+1, y1, x1, y1)
df<-rbind(df,df2)
sf<-sf::st_as_sf(df, wkt = "geom")
plot(sf)

##FUNCTION
nb.self<-function(nb, include=FALSE, remove= FALSE){
  if (include==TRUE & remove== TRUE) stop('cannot both add and remove self nb')

  n<-length(nb)
  self<-rep(FALSE,n)
  for (i in 1:n){
    self[i]<-i %in% nb[[i]] #Change to TRUE if included
  }

  #Describe status, no change
  if (all(self==TRUE)){attributes(nb)$self.included<-TRUE}
  if (all(self==FALSE)){attributes(nb)$self.included<-FALSE}
  attributes(nb)$self.is.in<-which(self==TRUE)

  #Include if
  if (include==TRUE){
    added<-0
        for (i in 1:n){
          if (self[i]==FALSE){
          nb[[i]]<-c(i,nb[[i]]) #Add to nb
          added<-added+1
          }
        }
        message(paste(added, "self nb added;", n-added, "self nb already included"))
        attributes(nb)$self.included<-TRUE
        attributes(nb)$self.is.in<-NULL
  }

  #Remove if
  if (remove==TRUE){
    removed<-0
    for (i in 1:n){
      if (self[i]==TRUE){
        nb[[i]]<-nb[[i]][-which(nb[[i]]==i)] #remove from nb
        removed<-removed+1
      }
    }
    message(paste(removed, "self nb removed;", n - removed, "self nb already absent"))
    attributes(nb)$self.included<-FALSE
    attributes(nb)$self.is.in<-NULL
  }

  return(nb)
  }


#nb from spdep
nbsp<-spdep::poly2nb(sf) #does not include self
nbsp_0<-nb.self(nbsp) #adds attributes is.in which this case is 0L
nbsp_with<-nb.self(nbsp, include=TRUE) #adds #SAME AS spdep::include.self
nbsp_without<-nb.self(nbsp_with, remove=TRUE) #remove #SAME AS spdep::remove.self

#nb from intersects and as.nb.sgbp


#nb from intersects
sgbp<-sf::st_intersects(sf, sf)

as.nb.sgbp <- function(x, ...) { #From Roger Bivand spdep vignette
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

nbsgbp<-as.nb.sgbp(sgbp)

#adds complexity by removing self from 1st. so ther is a mix
#And not to forget 17 has no neighbour
nbsgbp[[1]]<-nbsgbp[[1]][-1]

nbsgbp_0<-nb.self(nbsgbp) #adds attributes is.in which this case is all except the first one
nbsgbp_with<-nb.self(nbsgbp, include=TRUE) #adds
#spdep::include.self(nbsgbp) #SAME RESULT INDEED
nbsgbp_without<-nb.self(nbsgbp_with, remove=TRUE) #remove
spdep::remove.self(nbsgbp_with) #CONVERSELY THIS WON'T WORK: card(nb) : zero length neighbour vector because of case 17

nbsp_without<-nb.self(nbsgbp, remove=TRUE) #remove
spdep::remove.self(nbsgbp) #CONVERSELY THIS WON'T WORK:  Self not included. Cannot remove.
