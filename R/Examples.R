#Examples

#Example 1 - use of duncan()####
df<-data.frame(ID=c("I","II","III","IV"),A=c(4,4,1,0), B=c(0,0,2,2))

source("R/duncan.R")
duncan(df,"A","B")

#Example 2 - a simple geography (4x4) - global duncan ####
#Build geography
x1<-rep((1:4),each=4)
y1<-rep((1:4),4)
df<-data.frame(id=paste(x1,y1,sep="_"))
df$geom<-sprintf("POLYGON((%s %s, %s %s, %s %s, %s %s, %s %s))",
                 x1, y1, x1, y1+1, x1+1, y1+1, x1+1, y1, x1, y1)
sf<-sf::st_as_sf(df, wkt = "geom")
#Populate
sf$A<-1
sf$B<-rep((0:3),4)
#Plot
plot(sf)

#ggplot way
library(ggplot2)
g<-ggplot()+
  geom_sf(data=sf, fill="black", col="grey", linewidth=1)+
  theme_bw()
g

#Adding the two pop as points on top
set.seed(101)
Apt<-sf::st_sample(sf, size = sf$A)
Bpt<-sf::st_sample(sf, size = sf$B)

blues<-geom_sf(data=Apt, size=5, col="blue")
reds<-geom_sf(data=Bpt, size=5, col="red")
g+blues+reds


#global.duncan
source("R/global.duncan.R")
global.duncan(sf,"A","B")

#Example 3 - a simple geography (4x4) - local duncan ####
#
#Create a nb list using above sf
  # not sure what the best route is given
  # spdep is quicker than st_relate but may retire?) use spdep::poly2nb if too slow
  # sgbp<-st_relate(sf, sf, pattern = "F***T****") #List of "queen" neighbour polygons not including itself
sgbp<-st_intersects(sf, sf)
as.nb.sgbp <- function(x, ...) { #From Roger Bivand see spdep vignette
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}
nb<-as.nb.sgbp(sgbp)

#plot the neighbours ggplot geom_sf style on top of geography
#Make sf_lines from nb list
source("R/nbpoly2sflines.R")
nblines<-nbpoly2sflines(sf,nb) #should actually remove self before but no impact

library(ggplot2)
g2<-g+geom_sf(data=nblines, aes(col=factor(i)), linewidth=1)
g2

#Compute local.duncan
source("R/local.duncan.R")
Duncan_nb<-local.duncan(sf,"A","B",nb)

#ggplot way
g3<-g+geom_sf(data=Duncan_nb, aes(fill=local.duncan))
g3+blues+reds

#Example 4 - Manhattan - local duncan ####

#Extract blocks for NY Manhattan and reproject
  #NYMAN<-tigris::blocks(state="NY",county = "061")
  #MAN2263<-sf::st_transform(NYMAN,crs=2263)
  #saveRDS(MAN2263, "data/MAN2263.rds")
MAN2263<-readRDS("data/MAN2263.rds") #re-read from local

#Plot blocks
gM<-ggplot()+geom_sf(data=MAN2263, fill="white")+theme_bw()
gM

#Ethnicity data census 2020 from
#https://s-media.nyc.gov/agencies/dcp/assets/files/excel/data-tools/census/census2020/nyc-decennialcensusdata_2010_2020_census-blocks.xlsx
#100Mb file > externally subsetted to Manhattan and ethnic data only
#Meta from xls file:
  # Total population	Pop2
  # Hispanic	Hsp1
  # White non-Hispanic	WNH
  # Black non-Hispanic	BNH
  # Asian non-Hispanic	ANH
  # Some other race non-Hispanic	ONH
  # Non-Hispanic of two or more races	TwoPlNH
ETHN<-read.csv2("data/Manhattan_ethn.csv")[-1,]

#Make geoid's comparable and merge
MAN2263$GeoID<-substr(MAN2263$GEOID20, 5,16)
all(MAN2263$GeoID %in% ETHN$GeoID) #check match

#Map percentage of different ethnicity
MAN<-merge(MAN2263,ETHN, by="GeoID")
gM_Hsp1P<-ggplot()+
  geom_sf(data=MAN, aes(fill=Hsp1P), col="transparent")+
  theme_bw()+
  scale_fill_viridis_c(option = "rocket", na.value="grey90")
gM_Hsp1P

gM_WNHP<-ggplot()+
  geom_sf(data=MAN, aes(fill=WNHP), col="transparent")+
  theme_bw()+
  scale_fill_viridis_c(option = "rocket", na.value="grey90")
gM_WNHP

gM_BNHP<-ggplot()+
  geom_sf(data=MAN, aes(fill=BNHP), col="transparent")+
  theme_bw()+
  scale_fill_viridis_c(option = "rocket", na.value="grey90")
gM_BNHP

gM_ANHP<-ggplot()+
  geom_sf(data=MAN, aes(fill=ANHP), col="transparent")+
  theme_bw()+
  scale_fill_viridis_c(option = "rocket", na.value="grey90")
gM_ANHP

gridExtra::grid.arrange(gM_Hsp1P, gM_WNHP, gM_BNHP, gM_ANHP,nrow = 2)

#nb
sgbpMAN<-st_intersects(MAN, MAN)
nbMAN<-as.nb.sgbp(sgbpMAN)

#local.duncan
Duncan_MAN_HW<-local.duncan(MAN,"Hsp1","WNH",nbMAN)
Duncan_MAN_BW<-local.duncan(MAN,"BNH","WNH",nbMAN)
Duncan_MAN_AW<-local.duncan(MAN,"ANH","WNH",nbMAN)

gM_HW<-ggplot()+geom_sf(data=Duncan_MAN_HW, aes(fill=local.duncan))+
  theme_bw()+ scale_fill_viridis_c(option = "turbo", na.value="grey90")
gM_HW

gM_BW<-ggplot()+geom_sf(data=Duncan_MAN_BW, aes(fill=local.duncan))+
  theme_bw()+ scale_fill_viridis_c(option = "turbo", na.value="grey90")
gM_BW

gM_AW<-ggplot()+geom_sf(data=Duncan_MAN_AW, aes(fill=local.duncan))+
  theme_bw()+ scale_fill_viridis_c(option = "turbo", na.value="grey90")
gM_AW

#local.duncan scatterplot
source("R/duncan.plot.R")
duncan.plot(Duncan_MAN_HW,"Hsp1","WNH")
duncan.plot(Duncan_MAN_BW,"BNH","WNH")
duncan.plot(Duncan_MAN_AW,"ANH","WNH")


