RemoveDedups <- function(records,
                         list.variable,
                         verbose = FALSE,
                         write.blocks = FALSE) {
  
  x <- expand.grid(min.id = which(records$Year == 1901),
                   max.id = which(records$Year == 1911))
  
  x$min.id <- as.numeric(as.character(x$min.id))
  x$max.id <- as.numeric(as.character(x$max.id))
  
  # if records was read in using read_csv (i.e. is a tibble)
  # then we extract variables different than data frames
  if(is.tibble(records)){
    x$blockid <- paste0(pull(records, list.variable)[apply(x[1:2], 1, min)],
                        "_",
                        pull(records, list.variable)[apply(x[1:2], 1, max)])
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

