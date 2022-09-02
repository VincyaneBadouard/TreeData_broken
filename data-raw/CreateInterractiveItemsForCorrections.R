# get the Rd from the package
db <- tools::Rd_db("TreeData")

# extract the desctiptions of the arguments

outfile <- tempfile()
Fct_args <- NULL

for(rd in c("FullErrorProcessing.Rd",
            "GeneralErrorsDetection.Rd",
            "BotanicalCorrection.Rd",
            "StatusCorrection.Rd",
            "TaperCorrection.Rd",
            "DiameterCorrection.Rd",
            "RecruitmentCorrection.Rd")) {

  tools::Rd2HTML(db[[rd]], outfile)

  Label <- as.character(rvest::html_nodes(xml2::read_html(outfile ), "tr")[-1])

  Fct_args <- rbind(Fct_args, data.frame( Function = gsub("\\.Rd", "", rd),  Label = as.character(rvest::html_nodes(xml2::read_html(outfile ), "tr")[-1])
  ))

}

Fct_args <- Fct_args[!grepl(">Data<",Fct_args$Label), ] # remove Data



# create ItemID
Fct_args$ItemID <- gsub("<code>|</code>", "", regmatches( Fct_args$Label, regexpr("<code>.*?</code>", Fct_args$Label)))


# add the default values
Fct_args$Default <- ""

for(f in unique(Fct_args$Function)) {
  form <- formals(f)
  idx = Fct_args$Function %in% f
  m <- match(Fct_args$ItemID[idx], names(form) )
  Fct_args$Default[idx] <- form[m]
}


# make ItemID unique
Fct_args$ItemID <-paste0(Fct_args$Function, Fct_args$ItemID)

## add more info

df_moreInfo <-
  data.frame(class = c("numeric numeric", "integer integer", "logical logical", "character call","character character", "NULL NULL", "numeric call", "function call"),
             ItemType = c("numericInput", "numericInput","pickerInput", "pickerInput", "textInput", "TBD", "numericInput", "pickerInput"),
             Multiple = c(F, F, F, T, F, "TBD", F, F),
             Options = c(F, F, "list(`live-search` = TRUE)",  "list( `actions-box` = TRUE)", F, "TBD", F, "list( `actions-box` = TRUE)"),
             Argument = c("value", "value", "choices", "choices", "value", "TBD", "value", "choices"),
             Argument2 = c(F, F, "selected", "selected", F, "TBD", F, "selected"),
             argValue = c("OtherOptions", "OtherOptions", "LogicalOptions", "TBD", "OtherOptions", "TBD", "OtherOptions", "Function")) # TBD because some need to be column names, others need to be list of options given by default.


Fct_args <- cbind(Fct_args, df_moreInfo[match(sapply(Fct_args$Default, function(x) paste(class(eval(x)), class(x))), df_moreInfo$class), -1], ReactiveArgValue = T) # by default is a reactive in the app, but when list of option defined in the function, it will be changed to FALSE

## get the list of columns
x <- read.csv("inst/app/data/interactive_items.csv")


for(i in which(Fct_args$ItemType %in% "TBD")) {


  if(grepl("ScientificName", Fct_args$Label[i])) {
    Fct_args$ItemType[i] <- "pickerInput"
    Fct_args$Multiple[i] <- T
    Fct_args$Options[i] <- "list( `actions-box` = TRUE)"
    Fct_args$Argument[i] <- "choices"
    Fct_args$Argument2[i] <- "selected"
    Fct_args$argValue[i] <- "FormatedScientificNameOptions"
    Fct_args$ReactiveArgValue[i] <- TRUE

  } else { stop("Need to code for this argument")}

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
