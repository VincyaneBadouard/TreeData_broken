## code to prepare `ParacouSubsetFormated` dataset goes here

ParacouSubsetFormated <- RequiredFormat(
  ParacouSubset,
  input = ParacouProfile)

usethis::use_data(ParacouSubsetFormated, overwrite = TRUE)
usethis::use_R(ParacouSubsetFormated, overwrite = TRUE)



## For ParacouProfileFormated.Rmd  run next two line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")

write.csv(
  paste0("#'   \\item{", names(ParacouSubsetFormated), "}{", x$Description[match(names(ParacouSubsetFormated), x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
