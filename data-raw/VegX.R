# ---- get the VegX data structure ----#

# clear environment ####

rm(list = ls())

# load libraries ####
library(XML)


# Load VegX xml files to get data structure and Description
## VegX: an exchange standard for plot-based vegetation data
## Source of information:
## https://iavs-org.github.io/VegX/
## https://github.com/iavs-org/VegX/tree/master/vegxschema



# Links to the raw files .xsd files.
urls <- c(
  comm = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-community.xsd",
  misc = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-misc.xsd",
  org =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-organism.xsd",
  plot = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-plot.xsd",
  obs =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-plotobservation.xsd",
  # user = "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg-userdefined.xsd",
  veg =  "https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/veg.xsd"
)




VegX <- lapply(urls, function(url) {
  txt <- readLines(url)
  doc = xmlParse(txt)

  els <- getNodeSet(doc, "//xsd:element")

  a <-
    data.frame(element = sapply(els, function(el) xmlGetAttr(el, "name", default = NA)),
               minOccurs = sapply(els, function(el) xmlGetAttr(el, "minOccurs", default = NA)),

               maxOccurs = sapply(els, function(el) xmlGetAttr(el, "maxOccurs", default = NA)),
               type = sapply(els, function(el) xmlGetAttr(el, "type", default = NA)) #,
               # source = sapply(els, function(el) xmlGetAttr(el, "source", default = NA)),
               # default = sapply(els, function(el) xmlGetAttr(el, "default", default = NA)),
               # use = sapply(els, function(el) xmlGetAttr(el, "use", default = NA))
               )

  b <- cbind(table = gsub("https://raw.githubusercontent.com/iavs-org/VegX/master/vegxschema/|.xsd", "", url),
             mainElement = a$element,
             a, xmlToDataFrame(doc, nodes = els, homogeneous = F, collectNames = T))

  for(i in 1:nrow(b)) {
    idx <- rev(grep(b$annotation[i], b$complexType, fixed = T))[1]
    if(!is.na(idx)) b$mainElement[i] <- b$element[idx]
  }

  return(b)
})

VegX <- do.call(rbind, VegX)

VegX$complexType <- NULL
names(VegX) <- gsub("annotation" , "description", names(VegX)  )

table(VegX$table)
View(VegX)


# save
write.csv(VegX, paste0("inst/app/data/VegX.csv"), row.names = FALSE)

