## code to prepare `SCBISubsetFormated` dataset goes here

SCBISubsetFormated <- readRDS("C:/Users/herrmannV/Dropbox (Smithsonian)/GitHub/VincyaneBadouard/demo/scbi_formated.rds")

SCBISubsetFormated$LifeStatus[c(1, 2, 15, 389)]<- NA # making some NA's
scbi_formated$LifeStatus[151006] <- FALSE #making some dead
scbi_formated  <- scbi_formated[-c(258,3243, 567)] # removing some records


SCBISubsetFormated <- SCBISubsetFormated[IdStem %in% c("67276", "1304", "65905", "58921", "1", "10012", "11012", "66114", "31258", "10032", "3631"), ]

usethis::use_data(SCBISubsetFormated, overwrite = TRUE)



## For SCBISubsetFormated.Rmd  run next two line of code and paste in the item section of R/ParacouProfile.R
x <- read.csv("inst/app/data/interactive_items.csv")
idx = names(SCBISubsetFormated) %in% x$ItemID
write.csv(
  paste0("#'   \\item{", names(SCBISubsetFormated)[idx], ifelse(paste0(names(SCBISubsetFormated)[idx], "Original") %in% names(SCBISubsetFormated), paste0(", ", names(SCBISubsetFormated)[idx], "Original"), ""), "}{", x$Description[match(names(SCBISubsetFormated)[idx], x$ItemID)], "}"), "clipboard",
  quote = F, row.names = F)
