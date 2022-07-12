# get the Rd from the package
db <- tools::Rd_db("TreeData")


# extract the arguments and their descriptions from the funcions we are interested in
Fct_args <- lapply(db, tools:::.Rd_get_metadata, "arguments")[c("ErrorsDetection.Rd",
                                                                "StatusCorrection.Rd",
                                                                "DiameterCorrection.Rd")]

Fct_args <- lapply(Fct_args, function(x) eval(parse(text = paste("list(", gsub('\\"', "'", gsub(")list", "), list", gsub("(\n)", "", x ))), ")"))))

Fct_args <- do.call(rbind, lapply(Fct_args, function(y) do.call(rbind, lapply(y, function(x) data.frame(Label = x[[1]][[1]],
                                  helpText = paste(x[[2]], collapse = ""))))))


# remove argument Data as we don't need it in the app

Fct_args <- Fct_args[!Fct_args$Label %in% "Data",] # remove Data

# add a column to say what function it is coming from
Fct_args$Function <- gsub("\\.Rd\\.\\d*", "", rownames(Fct_args))

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
data.frame(class = c("numeric numeric", "integer integer", "logical logical", "character call","character character", "NULL NULL", "numeric call", "function call"),
           ItemType = c("numericInput", "numericInput","pickerInput", "pickerInput", "textInput", "TBD", "numericInput", "pickerInput"),
           Multiple = c(F, F, F, T, F, "TBD", F, F),
           Options = c(F, F, "list(`live-search` = TRUE)",  "list( `actions-box` = TRUE)", F, "TBD", F, "list( `actions-box` = TRUE)"),
           argument = c("value", "value", "choices", "choices", "value", "TBD", "value", "choices"),
           argument2 = c(F, F, "selected", "selected", F, "TBD", F, "selected"),
           argValue = c("OtherOptions", "OtherOptions", "LogicalOptions", "TBD", "OtherOptions", "TBD", "OtherOptions", "Function")) # TBD because some need to be column names, others need to be list of options given by default.


Fct_args <- cbind(Fct_args, df_moreInfo[match(sapply(Fct_args$Default, function(x) paste(class(eval(x)), class(x))), df_moreInfo$class), -1], ReactiveArgValue
= T) # by default is a reactive in the app, but when list of option defined in the function, it will be changed to FALSE

## get the list of columns
x <- read.csv("inst/app/data/interactive_items.csv")


for(i in which(Fct_args$ItemType %in% "TBD")) {


  if(grepl("ScientificName", Fct_args$helpText[i])) {
    Fct_args$ItemType[i] <- "pickerInput"
    Fct_args$Multiple[i] <- T
    Fct_args$Options[i] <- "list( `actions-box` = TRUE)"
    Fct_args$argument[i] <- "choices"
    Fct_args$argument2[i] <- "selected"
    Fct_args$argValue[i] <- "FormatedScientificNameOptions"
    Fct_args$ReactiveArgValue[i] <- TRUE

  }

}

for(i in which(Fct_args$argValue %in% "TBD")) {


  if(all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])) {
    Fct_args$argValue[i] <- "FormatedColumnOptions"
    Fct_args$ReactiveArgValue[i] <- TRUE
  }

  if(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])){
    Fct_args$argValue[i] <- deparse(Fct_args$Default[[i]])
    Fct_args$ReactiveArgValue[i] <- FALSE
    Fct_args$Default[i] <- paste0('c("', eval(Fct_args$Default[[i]])[1], '")')
    }

}

for(i in which(Fct_args$argValue %in% "TBD")) {


  if(all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T]))  Fct_args$argValue[i] <-"FormatedColumnOptions"

  if(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T])) Fct_args$argValue[i] <- deparse(Fct_args$Default[[i]])


 Fct_args$ReactiveArgValue[i] <- ifelse(!all(eval( Fct_args$Default[[i]]) %in% x$ItemID[x$Activate == T]), FALSE, TRUE)

 if(! Fct_args$ReactiveArgValue[i])  Fct_args$Default[i] <- paste0('c("', eval(Fct_args$Default[[i]])[1], '")')

}

for(i in which(Fct_args$argValue %in% "Function")) {
  Fct_args$argValue[i] <- paste0('list("Default Function" = "', paste(deparse(Fct_args$Default[[i]]), collapse = ""), '")')
  Fct_args$Default[i] <- paste0('list("Default Function" = "', paste(deparse(Fct_args$Default[[i]]), collapse = ""), '")')

  Fct_args$ReactiveArgValue[i] <- FALSE
}


# consider default as character now

Fct_args$Default <- as.character(Fct_args$Default)

# save
write.csv(data.frame(Fct_args), "inst/app/data/interactive_items_CorrerctionFunctions.csv", row.names = F)
