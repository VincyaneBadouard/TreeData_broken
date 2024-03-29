% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParacouSubsetFormated.R
\docType{data}
\name{ParacouSubsetFormated}
\alias{ParacouSubsetFormated}
\title{ParacouSubsetFormated}
\format{
A tibble
\describe{
\item{Site}{Site level name}
\item{Plot}{Plot level name}
\item{Subplot}{Subplot level name}
\item{PlotLat}{Plot's latitude}
\item{PlotLon}{Plot's longitude}
\item{XPlotUTM}{Plot's X UTM coordinates}
\item{YPlotUTM}{Plot's Y UTM coordinats}
\item{PlotArea}{Size of the plot}
\item{PlotElevation}{plot's elevation above sea level}
\item{SubplotLat}{Subplot's latitude}
\item{SubplotLon}{Subplot's longitude}
\item{XSubplotUTM}{Subplot's X UTM coordinates}
\item{YSubplotUTM}{Subplot's Y UTM coordinats}
\item{SubplotArea}{Size of the plot}
\item{MinDBH}{The minimum diameter of trees included in the inventory}
\item{IdCensus}{Census identifier}
\item{Year}{Year of measurement}
\item{Month}{Month of measurement}
\item{Day}{Day of measurement}
\item{Date, DateOriginal}{Date of measurement}
\item{TreeFieldNum}{Tree unique identifiers, matching the tag number in the field.}
\item{IdTree}{Tree unique identifiers. This was automatically generated if it was not provided in the input data (using plot and tree tag information).}
\item{StemFieldNum}{Stem identifiers within the tree.}
\item{IdStem}{Stem unique identifiers. This was automatically generated if it was not provided in the input data (using plot tree and stem tag information).}
\item{LifeStatus, LifeStatusOriginal}{Logical: TRUE if tree was alive, FALSE if it was dead}
\item{Diameter}{Tree Diameter}
\item{BD}{Basal diameter}
\item{Circ}{Circumference}
\item{BCirc}{Basal circumference}
\item{HOM}{Height of measurement}
\item{POM}{Code for point of measurement of tree diameter}
\item{BHOM}{Height of measurement of basal diameter}
\item{BPOM}{Code for point of measurement of basal diameter}
\item{TreeHeight}{Tree Height}
\item{TreeLat}{Tree Latitude}
\item{TreeLon}{Tree Longitude}
\item{XTreeUTM}{Tree X UTM coordinates}
\item{YTreeUTM}{Tree Y UTM coordinats}
\item{XTreePlot}{Tree X euclidean position in plot}
\item{YTreePlot}{Tree Y euclidean position in plot}
\item{XTreeSubplot}{Tree X euclidean position in subplot}
\item{YTreeSubplot}{Tree Y euclidean position in subplot}
\item{ScientificName}{Scientific name}
\item{VernName}{Vernacular name}
\item{Family}{Family name}
\item{Genus}{Genus name}
\item{Species}{Species name}
\item{Subspecies}{Subspecies name}
\item{Variety}{Variety name}
\item{Voucher}{Voucher code for the species}
\item{IdLevel}{Deepest taxonomic level for which full identification is known}
\item{Authority}{Taxonomic authority for the ID level}
\item{CommercialSp, CommercialSpOriginal}{Logical: TRUE if species is considered commercial, FALSE if not}
\item{LifeForm}{Life form}
\item{LifeStatusOriginal}{LifeStatus as it was given in the input table, without any correction or transformation}
\item{CommercialSpOriginal}{IsCommercial as it was given in the input table, without any correction or transformation}
\item{DateOriginal}{Date as it was given in the input table, without any correction or transformation}
...
}
}
\usage{
ParacouSubsetFormated
}
\description{
This is the standardized, formated version of ParacouSubset (see help(ParacouSubset))
}
\keyword{datasets}
