# for a given row of house.data
#' @export
GetHouseMatch <- function(house.data, original.data, comparison_data, rf.cutoff, rf.prob.name){
  # for a given row of house.data, get the corresponding comparison_data rows
  comparison_data.subset <- comparison_data[paste0(original.data[, "household_year"][comparison_data$min.id],
                                                   ".",
                                                   original.data[, "household_year"][comparison_data$max.id]) ==
                                              as.character(house.data["min.max.house"]), ]
  #  predicted_vals <- comparison_data.subset[, rf.prob.name.id]
  # get all ids associated with these comparisons
  min.max <- c(comparison_data.subset$min.id,
               comparison_data.subset$max.id)
  rf.prob.name.id <- which(names(comparison_data.subset) %in% rf.prob.name)
  # which max ids are associated with a high probability?
  ids.to.replace <- comparison_data.subset$max.id[comparison_data.subset[, rf.prob.name.id] > rf.cutoff]
  # which min ids are associated with a high probability?
  replacement.ids <- comparison_data.subset$min.id[comparison_data.subset[, rf.prob.name.id] > rf.cutoff]
  replacement.ids.full <- replacement.ids[match(min.max, ids.to.replace)]
  min.max[!is.na(replacement.ids.full)] <- replacement.ids.full[!is.na(replacement.ids.full)]

  return(sum(comparison_data.subset[, rf.prob.name.id][comparison_data.subset[, rf.prob.name.id] > rf.cutoff], na.rm = TRUE)/
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
