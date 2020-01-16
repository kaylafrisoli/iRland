#' Consistently load raw Irish census data
#'
#' Consistently load and standardize raw Irish census data
#'
#' @param path path to data
#'
#' @param file_delim single character used to separate fields within a record; default is " " for .txt files
#'
#' @param assignID string of variable names for creating a unique identifier
#'
#' @param preProcess has the data been preprocessed?
#'
#' @return data in tibble format
#'
#' @examples
#' LoadCleanRaw("~/GoogleDrive/irelandData/ticknock_kayla.csv", ",", preProcess = TRUE)
#' LoadCleanRaw("~/GoogleDrive/irelandData/census_ireland_1901/Carlow/Ticknock.txt")
#' LoadCleanRaw("~/GoogleDrive/irelandData/census_ireland_1901/Carlow/Ticknock.txt",
#'  assignID = c("County", "DED", "Year"))
#' @export
LoadCleanRaw <- function(path, file_delim = " ", assignID=NULL, preProcess = FALSE){

  dat <- suppressWarnings(suppressMessages(readr::read_delim(path,
                                                             delim = file_delim,
                                                      trim_ws = TRUE,
                                                      escape_double = FALSE))) %>%
    dplyr::mutate_all(as.character)

  if(!preProcess){
    # ticknock_kayla has been pre-processed, but the .txt raw files haven't
    dat$locations = gsub(" ", "", paste0(dat$TownStreet, dat$DED, dat$County))
    dat$locations = gsub("\\\\", "", dat$locations)
    dat$locations = gsub("&", "", gsub(",", "", dat$locations))
    dat$locations = gsub("\'", "", dat$locations)

    dat$household = gsub(" ", "", paste0(dat$TownStreet, dat$DED, dat$County, dat$Number))
    dat$household = gsub("\\\\", "", dat$household)
    dat$household = gsub("&", "", gsub(",", "", dat$household))
    dat$household = gsub("\'", "", dat$household)

    dat$housename = gsub(" ", "", paste0(dat$TownStreet, dat$DED, dat$County, dat$Surname))
    dat$housename = gsub("\\\\", "", dat$housename)
    dat$housename = gsub("&", "", gsub(",", "", dat$housename))
    dat$housename = gsub("\'", "", dat$housename)

    # dat <- dat[!(dat$Forename %in% c("?", "-")), ]
    dat <- dat[order(dat$Surname, dat$household), ]
  }

  dat[dat == "-"] <- NA
  dat$Age <- as.numeric(dat$Age)
  dat$Year <- as.numeric(dat$Year)

  dat$household_year <- paste0(dat$household,"_", dat$Year)


  dat$Literacy[dat$Literacy %in% c("Read write", "Can read and write",
                                   "Read and rite", "Can read write",
                                   "R w", "R and write",
                                   "Read with" )] <- "Read and write"

  dat$Literacy[dat$Literacy %in% c("Cannot r and w",
                                   "Not read write")] <- "Cannot read or write"

  dat$Literacy[dat$Literacy %in% c("Can read only" , "Read only")] <- "Read"

  dat$MaritalStat[dat$MaritalStat == "Not Married"] <- "Single"

  dat$MaritalStat[is.na(dat$MaritalStat)] <- ""

  dat$County_DED <- paste0(dat$County, ".",
                           dat$DED %>%
                             gsub(pattern="  ", replacement=" ") %>%
                             gsub(pattern=" ", replacement="_")) %>%
    gsub(pattern=" ", replacement="") %>%
    gsub(pattern="_$", replacement="")

  if(!is.null(assignID)){
    dat$ourID <- apply(dat[, assignID], 1, paste0, collapse="") %>%
      paste0(".") %>%
      paste0(1:nrow(dat))
  }

  names_all <- c("Year", "County",     "DED",  "TownStreet", "Number",     "ID",
                 "Surname",
                "Forename",   "Age",   "Sex",   "RelationHead",   "Religion",
                "Birthplace", "Occupation",
                "Literacy",   "IrishLang",  "MaritalStat", "SpecIlliness",
                "YearsMarr",  "ChildBorn",  "ChildLiv",
                "locations",  "household",  "housename",  "household_year", "
                County_DED", "ourID")

  missing_names <- which(!(names_all %in% names(dat)))

  if(length(missing_names) == 0){
    return(dat)

  } else{
    dat[names_all[missing_names]] <- NA
    return(dat)
  }

}


#' @export
AssignID <- function(dat, variables.for.id){

  dat$ourID <- apply(dat[, variables.for.id], 1, paste0, collapse="") %>%
    paste0(".") %>%
    paste0(1:nrow(dat))

  return(dat)
}

