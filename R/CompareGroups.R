# for a given row of house_data, where the pairwise data NEEDS to already be made
# in comparison_data
#' @importFrom rlang .data
#'
#' @export
GetGroupLinkage <- function(house_data, comparison_data, cutoff, similarity_field){
  comparison_data_subset <- comparison_data %>%
    dplyr::filter(min_max_house == pull(house_data, min.max.house))

  A_size = length(unique(comparison_data_subset$reference))
  B_size = length(unique(comparison_data_subset$candidate))

  comparison_data_subset_cutoff <- comparison_data_subset %>%
    dplyr::group_by(reference) %>%
    dplyr::arrange(dplyr::desc(.data[[similarity_field]])) %>%
    # dplyr::top_n(n = 1) %>%
    dplyr::slice(1) %>% # slice is faster and doesn't produce an error
    dplyr::ungroup() %>%
    pull(.data[[similarity_field]]) %>%
    .[. > cutoff]

  M_size <- length(comparison_data_subset_cutoff)
  Sim_sum <- sum(comparison_data_subset_cutoff, na.rm = TRUE)

  return(Sim_sum / (A_size + B_size - M_size))
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



# for a given row of house_data
GetGroupLinkageOld <- function(house_data, original_data, comparison_data, cutoff, similarity_field){
  # for a given row of house_data, get the corresponding comparison_data rows
  # comparison_data_subset <- comparison_data[paste0(original_data[, "household_year"][comparison_data$min.id],
  #                                                  ".",
  #                                                  original_data[, "household_year"][comparison_data$max.id]) ==
  #                                             as.character(house_data["min.max.house"]), ]
  # house_data <- as.data.frame(house_data)
  comparison_data_subset <- comparison_data %>%
    dplyr::filter(min_max_house == pull(house_data, min.max.house))

  #  predicted_vals <- dplyr::pull(ccomparison_data_subset, similarity_field.id)
  # get all ids associated with these comparisons
  min.max <- c(dplyr::pull(comparison_data_subset, min.id),
               dplyr::pull(comparison_data_subset, max.id))
  similarity_field_id <- which(names(comparison_data_subset) %in% similarity_field)
  # which max ids are associated with a high probability?
  ids.to.replace <- dplyr::pull(comparison_data_subset, max.id)[dplyr::pull(comparison_data_subset, similarity_field_id) > cutoff]
  # which min ids are associated with a high probability?
  replacement_ids <- dplyr::pull(comparison_data_subset, min.id)[dplyr::pull(comparison_data_subset, similarity_field_id) > cutoff]
  replacement_ids_full <- replacement_ids[match(min.max, ids.to.replace)]
  min.max[!is.na(replacement_ids_full)] <- replacement_ids_full[!is.na(replacement_ids_full)]

  return(sum(dplyr::pull(comparison_data_subset,
                         similarity_field_id)[dplyr::pull(comparison_data_subset, similarity_field_id) > cutoff], na.rm = TRUE)/
           length(unique(min.max)))
}
