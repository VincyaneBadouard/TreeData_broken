# get the interactive items
xall <- read.csv("inst/app/data/interactive_items.csv")
x <- xall[xall$Activate, ]

# create the list

AppProfile <- lapply(1:nrow(x), function(i) x[i, "AppDefaultProfile"])
AppProfile <- setNames(AppProfile, x$ItemID)

# save
saveRDS(AppProfile, "inst/app/data/AppProfile.rds")
