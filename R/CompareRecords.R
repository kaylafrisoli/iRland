#' @export
MakeComparisons <- function(RLdata,
                         comparisons,
                         unique.ids1=NULL,
                         unique.ids2=NULL,
                         variables.to.match=NULL,
                         string.comparators=NULL,
                         verbose=FALSE){
  # set the default variables.to.match to every column of the RLdata
  if(is.null(variables.to.match)){
    variables.to.match <- names(RLdata)
  } else{
    variables.to.match <- variables.to.match
  }
  # set the default string.comparator to jarowinkler for each column
  if(is.null(string.comparators)){
    string.comparators <- rep("jarowinkler", cols)
  } else{
    string.comparators <- string.comparators
  }

  GetHouseholdNames_Compare <- function(household_matrix, comparison_fct){
    names1 = paste(RLdata$Forename[RLdata$household_year %in% household_matrix[1]], collapse = "_")
    names2 = paste(RLdata$Forename[RLdata$household_year %in% household_matrix[2]], collapse = "_")

    return(get(comparison_fct)(names1, names2))
  }
  comparison.names <- paste(variables.to.match,
                            substr(string.comparators, 1, 3),
                            sep=".")

  # if(n.splits > 1){
  #   m <- 1:n.splits
  #   starts <- split.size*(m - 1) + 1
  #   if(nrow(comparisons)%%split.size == 0){
  #     ends <- split.size*(m)
  #   } else{
  #     ends <- c((split.size*(m))[1:(n.splits-1)], nrow(comparisons))
  #   }
  #   split.data.names <- paste0("d", 1:n.splits)
  #   split.blocks <- lapply(seq_along(starts), function(i) comparisons[starts[i]:ends[i], ])
  #   names(split.blocks) <- split.data.names
  #
  # } else{
    comparison.data <- comparisons
  # }

    for (i in 1:length(variables.to.match)) {
      if(verbose) print(i)

      if(variables.to.match[i] == "EXTRACT_HOUSEHOLD_INFO"){

        household.combs <- apply(comparison.data[, c("min.id", "max.id")],
                                 2, function(x) as.character(RLdata[, "household_year"][x]))
        if(verbose) print(head(household.combs))
        household.name.combs2 <- apply(household.combs, 1, GetHouseholdNames_Compare, comparison_fct =  string.comparators[i])
        comparison.data[comparison.names[i]] <- household.name.combs2

      } else{
        my.combs <- apply(comparison.data[, c("min.id", "max.id")],
                          2, function(x) as.character(RLdata[, variables.to.match[i]][x]))
        if(verbose) print(head(my.combs))
        if(nrow(comparison.data) == 1){
          comparison.data[, comparison.names[i]] <- get(string.comparators[i])(my.combs[ 1],
                                                                              my.combs[ 2])
        } else {
        pasted.combo.values <- paste0(my.combs[, 1], ".", my.combs[, 2])
        if(verbose) print(head(pasted.combo.values))
        unique.ones <- which(!duplicated(pasted.combo.values))
        unique.comparisons <- get(string.comparators[i])(my.combs[unique.ones, 1], my.combs[unique.ones, 2])
        mapping      <- match(pasted.combo.values, pasted.combo.values[unique.ones])
        comparison.data[comparison.names[i]] <- unique.comparisons[mapping]
        }
      }
    }

    if(is.null(unique.ids1)){
      comparison.data["true_match1"] <- NA
    } else{
      comparison.data["true_match1"] <-
        GetPairwiseMatchesFromIDs(comparison.data[, c("min.id", "max.id")], unique.ids1)
    }
    if(is.null(unique.ids2)){
      comparison.data["true_match2"] <- NA
    } else{
      comparison.data["true_match2"] <-
        GetPairwiseMatchesFromIDs(comparison.data[, c("min.id", "max.id")], unique.ids2)
    }
    return(comparison.data)
  }

#' @export
RemoveDedups <- function(records,
                         list.variable,
                         verbose = FALSE,
                         write.blocks = FALSE) {
  # changed from records$Year to pull
  # x <- expand.grid(min.id = which(records$Year == 1901),
  #                  max.id = which(records$Year == 1911))


  # if records was read in using read_csv (i.e. is a tibble)
  # then we extract variables different than data frames
  if(is.tibble(records)){
    x <- expand.grid(min.id = which(dplyr::pull(records, list.variable) == 1901),
                     max.id = which(dplyr::pull(records, list.variable) == 1911))
    x$min.id <- as.numeric(as.character(x$min.id))
    x$max.id <- as.numeric(as.character(x$max.id))
    x$blockid <- paste0(dplyr::pull(records, list.variable)[apply(x[1:2], 1, min)],
                        "_",
                        dplyr::pull(records, list.variable)[apply(x[1:2], 1, max)])
  } else{
    # kayla added comma before list.variables on 7/24 at 5 pm
    x <- expand.grid(min.id = which(records[, list.variable] == 1901),
                     max.id = which(records[, list.variable] == 1911))
    x$min.id <- as.numeric(as.character(x$min.id))
    x$max.id <- as.numeric(as.character(x$max.id))
    x$blockid <- paste0(records[, list.variable][apply(x[1:2], 1, min)],
                        "_",
                        records[, list.variable][apply(x[1:2], 1, max)])
  }


  pairs.to.compare <- data.frame(
    min.id = apply(x[1:2], 1, min),
    max.id = apply(x[1:2], 1, max),
    blockid = x$blockid
    # passid = "NA"
  )



  if (verbose)
    print("done with new.combs")

  if (write.blocks) {
    write.csv(pairs.to.compare, "pastepassall.csv")
  }
  return(pairs.to.compare)
}



#' @export
RemoveDedupsBlock <- function(records,
                         block.variables, # don't include year
                         verbose = FALSE,
                         write.blocks = FALSE) {

  block_groups <- apply(records[, block.variables], 1,
                        paste0, collapse = TRUE)

  x <- expand.grid(min.id = which(records$Year == 1901),
                   max.id = which(records$Year == 1911))

  x$min.id <- as.numeric(as.character(x$min.id))
  x$max.id <- as.numeric(as.character(x$max.id))

  # if records was read in using read_csv (i.e. is a tibble)
  # then we extract variables different than data frames
  if(is.tibble(records)){
    x$blockid <- paste0(dplyr::pull(records, list.variable)[apply(x[1:2], 1, min)],
                        "_",
                        dplyr::pull(records, list.variable)[apply(x[1:2], 1, max)])
  } else{
    x$blockid <- paste0(records[list.variable][apply(x[1:2], 1, min)],
                        "_",
                        records[list.variable][apply(x[1:2], 1, max)])
  }


  pairs.to.compare <- data.frame(
    min.id = apply(x[1:2], 1, min),
    max.id = apply(x[1:2], 1, max),
    blockid = x$blockid,
    passid = "NA"
  )



  if (verbose)
    print("done with new.combs")

  if (write.blocks) {
    write.csv(pairs.to.compare, "pastepassall.csv")
  }
  return(pairs.to.compare)
}
