ExactMatch <- function(vec1, vec2){
  exact.match <- 1* (vec1 == vec2)
  return(exact.match)
}

ExactNonMatch <- function(vec1, vec2){
  exact.match <- 1* (vec1 == vec2)
  return(!exact.match)
}


ExactSoundex <- function(vec1, vec2){

  return(ExactMatch(RecordLinkage::soundex(vec1), RecordLinkage::soundex(vec2)))
}


f1 <-
  SortPaste <- function(vec1, vec2){

    return(apply(cbind(vec1, vec2), 1, function(x) return(paste0(sort(x), collapse = ""))))
    #return(paste0(sort(c(vec1, vec2)), collapse = ""))
  }

AbsoluteDifference <- function(vec1, vec2){
  ab.dif <- as.numeric(abs(as.numeric(vec1)-as.numeric(vec2)))
  return(ab.dif)
}


GetPairwiseMatchesFromIDs <- function(combinations.of.original.data,
                                      ids.from.original.data){
  ids.from.original.data <- as.vector(ids.from.original.data)
  1 * (ids.from.original.data[combinations.of.original.data[, 1]] ==
         ids.from.original.data[combinations.of.original.data[, 2]])
}

