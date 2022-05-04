
# get the Rd from the package
db <- tools::Rd_db("TreeData")


# extract the arguments and their descriptions from the funcions we are interested in
Fct_args <- lapply(db, tools:::.Rd_get_metadata, "arguments")[c("ErrorsDetection.Rd", "StatusCorrection.Rd")]

Fct_args <- lapply(Fct_args, function(x) eval(parse(text = paste("list(", gsub('\\"', "'", gsub(")list", "), list", gsub("(\n)", "", x ))), ")"))))

Fct_args <- do.call(rbind, lapply(Fct_args, function(y) do.call(rbind, lapply(y, function(x) data.frame(Label = x[[1]][[1]],
                                  helpText = paste(x[[2]], collapse = ""))))))


# remove argument Data as we don't need it in the app

Fct_args <- Fct_args[!Fct_args$Label %in% "Data",] # remove Data

# add a column to say what function it is coming from
Fct_args$Function <- gsub("\\.Rd\\.\\d", "", rownames(Fct_args))

# create ItemID
Fct_args$ItemID <- paste0(Fct_args$Function, Fct_args$Label)


# add the default values
Fct_args$Default <- ""

for(f in unique(Fct_args$Function)) {
  form <- formals(f)
  idx = Fct_args$Function %in% f
  m <- match(Fct_args$Label[idx], names(form) )
  Fct_args$Default[idx] <- form[m]
}

## add more info
# ItemTypeFunction <- function(class) switch(class, "numeric" = "numericInput", "logical" = "pickerInput", "call" = "pickerInput", "text" = "textInput")

df_moreInfo <-
data.frame(class = c("numeric", "logical", "call","text"),
           ItemType = c("numericInput", "pickerInput", "pickerInput", "textInput"),
           Multiple = c(F, F, T, F),
           Options = c(FALSE, "list(`live-search` = TRUE)",  "list( `actions-box` = TRUE)", FALSE),
           argument = c("value", "choices", "choices", "value"),
           argument2 = c(FALSE, "selected", "selected", FALSE),
           argValue = c("OtherOptions", "LogicalOptions", "FormatedColumnOptions", "OtherOptions"))




Fct_args <- cbind(Fct_args, df_moreInfo[match(sapply(Fct_args$Default,class), df_moreInfo$class), -1])


# consider default as characher now

Fct_args$Default <- as.character(Fct_args$Default)

# save
write.csv(data.frame(Fct_args), "inst/app/data/interactive_items_CorrerctionFunctions.csv", row.names = F)
