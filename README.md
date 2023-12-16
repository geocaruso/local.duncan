# local.duncan
Local spatial version of Duncan segregation index.

From a discussion with colleagues involved in the cLuxembourg ensus description project (project including STATEC, LISER and UNI.LU)
a willingness emerged from discussions with Frederic Docquier, Philippe Gerber and Isabelle Pigeron
to have a spatially distributed version of the Duncan index of segregation.

I thought it would exist already in some form, but I didn't find it. I thus made a first quick overnight raster-based version with Yann Ferro
early December 2023 for direct use in the paper 9 of the census series
(see https://statistiques.public.lu/fr/recensement.html), promising myself to develop a proper version.
 
This repo is for reaching a cleaner version, starting from a vector approach using sf and neighbours lists

Very open to any suggestions, including critiques of this local indicator, which surprisingly doesn't seem to exist.
Also of course hoping to see much coding improvement.

The basic idea of Duncan index is to compare the distribution of 2 populations A and B across a series of spatial units.
See https://en.wikipedia.org/wiki/Duncan_Segregation_Index. Duncan is a global index in the sense a single value is returned for a given set of spatial units,
usually a country or a region.

A local version aims to provide a value for each spatial unit i based on the proportion of A and B in i and its neighbouring spatial units.
Thus identifying where segregation is stronger/weaker.
 
 
