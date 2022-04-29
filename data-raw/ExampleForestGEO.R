## code to prepare `ExampleForestGEO` dataset goes here


# devtools::install_github("forestgeo/fgeo.data")
library(fgeo.data)

ExampleForestGEO <- merge(fgeo.data::luquillo_stem5_random, fgeo.data::luquillo_species, by="sp", all.x  =T)

ExampleForestGEO$dbh[sample(which(is.na(ExampleForestGEO$dbh)), size = 5)] <- "NULL" # make dbh a character and with som "NULL" values, because that happens and generates and error with RequireFormat function, so we need to troubleshoot and handle this.


usethis::use_data(ExampleForestGEO, overwrite = TRUE)



## For ExampleForestGEO.Rmd  run next line of code and paste in the item section of R/ExampleForestGEO.R

# see this for desctiptions of columns: \url{http://ctfs.si.edu/Public/CTFSRPackage/files/help/RoutputStem.pdf and fgeo.data::data_dictionary}.
