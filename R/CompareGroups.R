#' @export
GetHouseMatch <- function(house.data, comparison.data, rf.cutoff, rf.prob.name){
  comparison.data.subset <- comparison.data[paste0(ticknock$household_year[comparison.data$min.id],
                                                   ".",
                                                   ticknock$household_year[comparison.data$max.id]) ==
                                              as.character(house.data["min.max.house"]), ]
  min.max <- c(comparison.data.subset$min.id,
               comparison.data.subset$max.id)
  rf.prob.name.id <- which(names(comparison.data.subset) %in% rf.prob.name)
  ids.to.replace <- comparison.data.subset$max.id[comparison.data.subset[, rf.prob.name.id] > rf.cutoff]
  replacement.ids <- comparison.data.subset$min.id[comparison.data.subset[, rf.prob.name.id] > rf.cutoff]
  replacement.ids.full <- replacement.ids[match(min.max, ids.to.replace)]
  min.max[!is.na(replacement.ids.full)] <- replacement.ids.full[!is.na(replacement.ids.full)]

  return(sum(comparison.data.subset[, rf.prob.name.id][comparison.data.subset[, rf.prob.name.id] > rf.cutoff])/
           length(unique(min.max)))
}


#' @export
JacKay <- function(vec1, vec2){
  return(length(intersect(vec1, vec2)) / length(union(vec1, vec2)))
}

# NOTE RETURNED NOT IN ORDER
#' @export
AdjJacPreProc <- function(x){
  x_new <- c()
  x_tab <- table(x)
  x_tab_dup <- table(x) > 1
  if(sum(x_tab_dup) > 0){
    for(i in which(x_tab_dup)){
      x_new <- c(x_new, paste0(names(x_tab[i]), 1:x_tab[i]))
    }
    if(any(!x_tab_dup)){
      return(c(x_new, names(x_tab[!x_tab_dup])))
    } else{
      return(x_new)
    }
  } else{
    return(x)
  }
}
