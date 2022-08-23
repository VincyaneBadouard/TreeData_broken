test_that("GeneralErrorsDetection", {

  # Import data
  library(data.table)
  data(TestData)

  # Create test data
  MatrixData <- as.matrix(TestData)
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoPlotData <- TestData[, !c("Plot")]

  # Check the function argument
  expect_error(GeneralErrorsDetection(MatrixData),
               regexp = "Data must be a data.frame or data.table")


  # Check the function work
  Rslt <- GeneralErrorsDetection(TestData)

  ## Remove *duplicated rows*
  expect_true(anyDuplicated(TestData)!= 0 & anyDuplicated(Rslt) == 0)

  ## Check *missing value* in
  # (X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/IdTree/Diameter/POM/HOM/Family/Genus/Species/VernName)
  Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree",
            "Diameter", "POM", "TreeHeight", "StemHeight", "HOM",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")
  # v =1
  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Rslt)){ # If the column exists

      MissingVal <- is.na(Rslt[,get(Vars[v])]) # any(MissingVal)

      expect_true(all(Rslt$Comment[MissingVal] != ""))
    }
  }


  ## Check *missing value* (NA/0) in the measurement variables
  Vars <- c("Diameter", "HOM", "TreeHeight")
  # v = 1
  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Rslt)){ # If the column exists

      NullVal <- Rslt[,get(Vars[v])] %in% 0 # any(NullVal) # which(is.na(NullVal))

      expect_true(all(Rslt$Comment[NullVal] != ""))
    }
  }

  ## Check *duplicated TreeFieldNum* in plot-subplot association in a census (at the site scale)

  # duplicated_num <- num <- vector("character")
  # # For each site
  # for (s in unique(na.omit(Rslt$Site))) {
  #   # For each census
  #   for (y in unique(na.omit(Rslt$Year))) {
  #     # For each plot
  #     for (p in unique(na.omit(Rslt$Plot))) {
  #       # For each Subplot in this plot
  #       for (c in unique(na.omit(Rslt[Rslt$Plot==p, Subplot]))) {
  #
  #         num <- Rslt[Rslt$Site == s & Rslt$Year == y
  #                     & Rslt$Plot == p & Rslt$Subplot == c,]$TreeFieldNum # all the TreeFieldNum for each Plot-Subplot combination
  #
  #         # if there are several TreeFieldNum per Plot-Subplot combination
  #         if(anyDuplicated(num) != 0){
  #           duplicated_num <- unique(num[duplicated(num)])
  #
  #           DuplFieldNbr <- (Rslt[,Site] == s & Rslt[,Year] == y
  #                            & Rslt[,Plot] == p & Rslt[,Subplot] == c
  #                            & Rslt[,TreeFieldNum] %in% duplicated_num)
  #
  #           expect_true(all(Rslt$Comment[DuplFieldNbr] != "")) # Rslt[DuplFieldNbr]
  #
  #           num <- vector("character")
  #
  #         } else {num <- vector("character")}
  #       } # end subplot loop
  #     } # end plot loop
  #   } # end year loop
  # } # end site loop


  ## Check of the *unique association of the idTree with plot, TreeFieldNum subplot and coordinates* (at the site scale)

  duplicated_ID <- CorresIDs <- vector("character")
  # For each site
  for (s in unique(na.omit(Rslt$Site))) {

    correspondances <- na.omit(unique(
      Rslt[Rslt$Site == s, .(IdTree, Plot, Subplot, TreeFieldNum, XTreeUTM, YTreeUTM)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      NnUniqdIdTree <- (Rslt[,Site] == s
                     & Rslt[,IdTree] %in% duplicated_ID)

      expect_true(all(Rslt$Comment[NnUniqdIdTree] != "")) # Rslt[NnUniqdIdTree]

    }
  } # end site loop

  ## Check *duplicated idTree* in a census (at the site scale)

  duplicated_ids <- ids <- vector("character")
  # For each site
  for (s in unique(na.omit(Rslt$Site))) {
    # For each census
    for (y in unique(na.omit(Rslt$Year))) {

      ids <- Rslt[Rslt$Site == s & Rslt$Year == y,]$IdTree # all the IdTree for each Site and Year combination

      # if there are several IdTree per Site and Year combination
      if(anyDuplicated(ids) != 0){
        duplicated_ids <- unique(ids[duplicated(ids)])

        DuplIdTree <- (Rslt[,Site] == s & Rslt[,Year] == y
                 & Rslt[,IdTree] %in% duplicated_ids)

        expect_true(all(Rslt$Comment[DuplIdTree] != "")) # Rslt[DuplIdTree]

      }
    } # end year loop
  } # end site loop


  ## Check for trees *outside the subplot* A FAIRE

  ## Check *fix Plot and Subplot number* A FAIRE

})

# Remove *duplicated rows*
# Check *missing value* in X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/IdTree/Diameter/POM/HOM/Family/Genus/Species/VernName
# Check *missing value* (NA/0) in the measurement variables
# Check *duplicated TreeFieldNum* in plot-subplot association in a census (at the site scale)
# Check of the *unique association of the idTree with plot, subplot, TreeFieldNum and coordinates* (at the site scale)
# Check *duplicated idTree* in a census (at the site scale)
# Check for trees *outside the subplot*
# Check *invariant coordinates per IdTree*
# Check *fix Plot and Subplot number*
