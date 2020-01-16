

#' Get an unlabeled entity
#'
#' Find someone or something that has not been previously labeled
#'
#'
#' @param path_data path to data we want to label
#'
#' @param path_labels path to file where we track previously labeled data
#'
#' @return ourID the ID of an unlabeled entity
#'
#' @examples
#'
#' @export
GetUnlabeledID <- function(path_data=NULL, labelee_data=NULL, path_labels){
  # should we do data instead of path_data?????
  # let's do both
  #
  # if you want to read in multiple files simultaneously see ireland_full_carlow.R
  # could also just sample from the paths to minimize computation
  # but we'd have to also return the path

  if(!is.null(labelee_data) & !is.null(path_data)){
    stop("either path_data or labelee_data should be specified, not both")
  }
  if(is.null(labelee_data) & is.null(path_data)){
    stop("either path_data or labelee_data must be specified")
  }

  if(is.null(labelee_data)){
    dat_to_label <- suppressMessages(LoadCleanRaw(path_data, assignID=c( "County", "DED", "Year")))
  } else{
    dat_to_label <- labelee_data
  }

  dat_labels <- suppressMessages(read_csv(path_labels))

  unlabeledRecs <- which(!(pull(dat_to_label, "ourID") %in% dat_labels["ourID"]))
  if(length(unlabeledRecs) == 0) stop("all data in path_data or labelee_data have already been labeled")
  unlabeledID <- sample(pull(dat_to_label, "ourID")[unlabeledRecs], 1)
  return(unlabeledID)
}



# get DEDs within a radius of current_location from geo_data
# dat$County_DED <- paste0(dat$County, ".", dat$DED %>%
#                          gsub("  ", " ", .) %>% gsub(" ", "_", .)) %>%
# gsub(" ", "", .) %>% gsub("_$", "", .)

# could do some type of radius or max?
#' @export
LocateDEDs <- function(distance_matrix, County_DED, radius_meters, max_num=NULL, min_num=NULL){
  x <- distance_matrix[order(distance_matrix[, County_DED]), County_DED, drop=FALSE]
  County_DED_within <- rownames(x)[x <= radius_meters]

  if(!is.null(min_num)){
    if(length(County_DED_within) < min_num){
      County_DED_within <- rownames(x)[1:min_num]
    }
  }

  if(!is.null(max_num)){
    if(length(County_DED_within) > max_num){
      County_DED_within <- rownames(x)[1:max_num]
    }
  }
  return(County_DED_within)
}


# other options: "~/GoogleDrive/irelandData/census_ireland_1901"
#' @export
ExtractDataByLocation <- function(County_DEDs,
                                  path_1901 = "data/census_ireland_1901",
                                  path_1911 = "data/census_ireland_1911",
                                  Pre_Loaded_Data = NULL){

  if(is.null(Pre_Loaded_Data)){

    full_file_paths <- c(list.files(path_1901,
                                    pattern = "*.txt",
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = TRUE),
                         list.files(path_1911,
                                    pattern = "*.txt",
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = TRUE))

    # Extract the DEDs -- district electoral division

    DEDs_to_match_data <- c(list.files(path_1901,
                                       pattern = "*.txt",
                                       recursive = TRUE,
                                       full.names = FALSE,
                                       include.dirs = FALSE),
                            list.files(path_1911,
                                       pattern = "*.txt",
                                       recursive = TRUE,
                                       full.names = FALSE,
                                       include.dirs = FALSE)) %>%
      stringr::str_extract( "([^/]+$)") %>%
      gsub(".txt", "", .) %>% gsub("_", " ", .)

    counties <- c(list.files(path_1901,
                             pattern = "*.txt",
                             recursive = TRUE,
                             full.names = TRUE,
                             include.dirs = FALSE),
                  list.files(path_1911,
                             pattern = "*.txt",
                             recursive = TRUE,
                             full.names = TRUE,
                             include.dirs = FALSE)) %>%
      gsub("([^/]+$)", "", .) %>%
      gsub("/$", "", .) %>%
      stringr::str_extract( "([^/]+$)")

    all_county_deds <- paste0(counties, ".", DEDs_to_match_data %>%
                                gsub("  ", " ", .) %>%
                                gsub(" ", "_", .))    %>%
      gsub(" ", "", .) %>% gsub("_$", "", .)

    # 10/8/18 fix some of the grepl issues with county names etc.

    if("Kildare.Athy_East" %in% County_DEDs){
      County_DEDs <- c(County_DEDs, "Kildare.Athy_East_Urban")
    }
    # which_files <- which(all_county_deds %in% County_DEDs)
    which_files <- which(stringr::str_replace_all(all_county_deds, "[^[:alnum:]]", "") %in%
                           stringr::str_replace_all(County_DEDs, "[^[:alnum:]]", ""))

    tbl1 = lapply(full_file_paths[which_files], LoadCleanRaw, assignID = c("County", "DED", "Year"))
    # for(i in 1:length(tbl1)){
    #   tbl1[[i]] <- mutate_all(tbl1[[i]], as.character)
    # }
    data_within_radius = tbl1 %>% bind_rows()
    # rbind.fill was slightly faster so we'll stick to that
    #data_within_radius = tbl1 %>% rbind.fill()


  }else{
    data_within_radius = filter(Pre_Loaded_Data, County_DED %in% County_DEDs)
  }

  return(data_within_radius)
}


# other options: "~/GoogleDrive/irelandData/census_ireland_1901"
#' @export
ExtractDataByLocationYear <- function(County_DEDs,
                                      path = "data/census_ireland_1901",
                                      Pre_Loaded_Data = NULL){

  if(is.null(Pre_Loaded_Data)){

    full_file_paths <- c(list.files(path,
                                    pattern = "*.txt",
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = TRUE))

    # Extract the DEDs -- district electoral division

    DEDs_to_match_data <- c(list.files(path,
                                       pattern = "*.txt",
                                       recursive = TRUE,
                                       full.names = FALSE,
                                       include.dirs = FALSE)) %>%
      stringr::str_extract( "([^/]+$)") %>%
      gsub(".txt", "", .) %>% gsub("_", " ", .)

    counties <- c(list.files(path,
                             pattern = "*.txt",
                             recursive = TRUE,
                             full.names = TRUE,
                             include.dirs = FALSE)) %>%
      gsub("([^/]+$)", "", .) %>%
      gsub("/$", "", .) %>%
      stringr::str_extract( "([^/]+$)")

    all_county_deds <- paste0(counties, ".", DEDs_to_match_data %>%
                                gsub("  ", " ", .) %>%
                                gsub(" ", "_", .))    %>%
      gsub(" ", "", .) %>% gsub("_$", "", .)

    # 10/8/18 fix some of the grepl issues with county names etc.

    if("Kildare.Athy_East" %in% County_DEDs){
      County_DEDs <- c(County_DEDs, "Kildare.Athy_East_Urban")
    }
    # which_files <- which(all_county_deds %in% County_DEDs)
    which_files <- which(stringr::str_replace_all(all_county_deds, "[^[:alnum:]]", "") %in%
                           stringr::str_replace_all(County_DEDs, "[^[:alnum:]]", ""))

    tbl1 = lapply(full_file_paths[which_files], LoadCleanRaw, assignID = c("County", "DED", "Year"))
    # for(i in 1:length(tbl1)){
    #   tbl1[[i]] <- mutate_all(tbl1[[i]], as.character)
    # }
    data_within_radius = tbl1 %>% bind_rows()
    # rbind.fill was slightly faster so we'll stick to that
    #data_within_radius = tbl1 %>% rbind.fill()


  }else{
    data_within_radius = filter(Pre_Loaded_Data, County_DED %in% County_DEDs)
  }

  return(data_within_radius)
}

# want greater than or equal to cutoff
# if function returns logical true/false then leave cutoff null
#' @export
SubsetByFunction <- function(data_to_subset, data_labelee, var, funct,
                             cutoff=NULL, cutoff_operator=c( "greater.equal", "greater","less.equal", "less", "between")){

  function_output <- sapply(pull(data_to_subset, var), function(x) {
                                  get(funct)(x, pull(data_labelee, var))
                                  })
  if(length(function_output) == 0){
    return(data_to_subset)
  }

  if(is.null(cutoff)){
    return(data_to_subset[function_output & !is.na(function_output), ])
  }
  suppressWarnings(if(cutoff_operator == "greater.equal"){
    return(data_to_subset[(function_output >= cutoff) & !is.na(function_output >= cutoff), ])
  })
  if(cutoff_operator == "greater"){
    return(data_to_subset[(function_output > cutoff) & !is.na(function_output > cutoff), ])
  }
  if(cutoff_operator == "less.equal"){
    return(data_to_subset[(function_output <= cutoff) & !is.na(function_output <= cutoff), ])
  }
  if(cutoff_operator == "less"){
    return(data_to_subset[(function_output < cutoff) & !is.na(function_output < cutoff), ])
  }
  if(cutoff_operator == "between"){
    cutoff <- sort(cutoff, decreasing = FALSE)
    return(data_to_subset[(function_output > cutoff[1]) &
                            (function_output < cutoff[2]) &
                            !is.na(function_output > cutoff[1]) &
                            !is.na(function_output < cutoff[2]), ])
  }
}


# data_household is the data where we know we can find
# the household of the candidate
#' @export
GetHousehold <- function(data_candidate, data_household=NULL){

  if(is.null(data_household)){
    data_household <- pull(data_candidate, "County_DED") %>% ExtractDataByLocation()
  }

  household <- data_household %>%
    filter(household_year == pull(data_candidate, "household_year"))

  return(household)

}



