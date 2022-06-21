# get the interactive items
xall <- read.csv("inst/app/data/interactive_items.csv")
x <- xall[xall$Activate, ]

# create the list

AppProfile <- sapply(1:nrow(x), function(i) setNames(x[i, "AppDefaultProfile"],x[i, "ItemID"]))

# save
saveRDS(AppProfile, "inst/app/data/AppProfile.rds")
