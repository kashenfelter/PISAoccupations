#' Add columns with occupations and change format to long.
#'
#' @param dataSource data frame returned by importFromTxt function.
#' @param NAcodes occupation that need to be converted to NA.
#' 
#' @return data frame in long format ...
#'

addISCO <- function(dataSource, NAcodes) {
    dataSource %>%
      mutate(motherOccupation = ifelse(motherOccupation %in% NAcodes, NA, motherOccupation),
	     fatherOccupation = ifelse(fatherOccupation %in% NAcodes, NA, fatherOccupation)) %>%
      mutate(mother= substr(motherOccupation, 1, 1),
	     father = substr(fatherOccupation, 1, 1)) %>%
      select(-motherOccupation, -fatherOccupation) %>%
      gather(parent, occupation, mother, father) 
}


#' Helper function for exportSpreadsheet to convert data in wide format.
#'
#' @param sourceTibble tibble returned by importFromTxt function.
#' @param column name of a column to spread.
#'
#' @return tibble
#'

spreadBySubject <- function(sourceTibble, column) {
  sourceTibble %>%
    select(cnt, isco, eval(as.name(column))) %>%
    spread_("isco", column) %>%
    setNames(c(" ", as.character(1:9), "country"))
}


#' Export data for one year to spreadsheet file.
#'
#' @param sourceTibble tibble returned by importFromTxt function.
#' @param outputFilePath Path to an output file.
#'
#' @return nothing
#'
#' @export
#'

exportSpreadsheet <- function(dataSource, outputFilePath) {
  bySubject <- split(dataSource, dataSource$subject)
  means <- lapply(bySubject, function(x) spreadBySubject(x, "ave.perf")[, c(1, 11, 2:10)])
  ses <- lapply(bySubject, function(x) spreadBySubject(x, "se")[, c(1, 11, 2:10)])
  popShare <- lapply(bySubject[1], function(x) spreadBySubject(x, "pop.share")[, c(1, 11, 2:10)])
  nStud <- lapply(bySubject[1], function(x) spreadBySubject(x, "nstud")[, c(1, 11, 2:10)])
  nSchool <- lapply(bySubject[1], function(x) spreadBySubject(x, "nschool")[, c(1, 11, 2:10)])
  tmp <- c(means, ses, popShare, nStud, nSchool)

  WriteXLS(tmp, outputFilePath, c("MATH means", "READ means", "SCIE means",
				  "MATH se", "READ se", "SCIE se",
				  "population share", "number of schools", "number of students"))
}

#' Helper function that adds subjects to data frames in importFromTxt function.
#'
#' @param slist List containing tibbles.
#' @param subs Character vector of subject names.
#' 
#' @return list
#'

addSubject <- function(slist, subs) {
  lapply(as.list(1:3), function(x) slist[[x]] %>% 
           mutate(subject = subs[x]))
}


#' Import data from .txt files from OECD website.
#'
#' @param inputFilePath path to the file to import.
#' @param variablesStartPositions starting position of column containing
#'        country code, school id, mother and father occupation
#'        and plausible values and replicate weights.
#' @param variablesEndPositions ending positions of column given in previous argument.
#' @param groupingVariablesStartPositions starting positions of additional columns 
#'        with factor variables.
#' @param groupingVariablesEndPositions ending positions of variables from previous argument.
#' @param groupingVariablesNames names for variables given in previous two arguments.
#' @param outputFilePath path to .rda file to which results will be saved.
#' @param studyYear year of study.
#' @param NAcodes codes for NA values in PV and weight variables.
#'
#' @return tibble
#'
#' @export
#'

importFromTxt <- function(inputFilePath, variablesStartPositions, variablesEndPositions,
			  groupingVariablesStartPositions, groupingVariablesEndPositions, groupingVariablesNames, 
			  studyYear, NAcodes = character(0), occupationNAcodes = character(0), outputFilePath = NULL) {

  nFactors <- length(groupingVariablesNames)
  if(missing(studyYear))
    stop("Year of study must be given.")
  if(missing(inputFilePath) | missing(outputFilePath))
    stop("Input and output files paths must be given.")
  if(nFactors == 0)
    stop("Please provide grouping variables.")
  if(sum(c("motherOccupation", "fatherOccupation", "cnt", "schoolID") %in% groupingVariablesNames) < 4)
    stop("'motherOccupation', 'fatherOccupation', 'cnt' and 'schoolID' must be among grouping variables")

  subjects <- c("MATH", "READ", "SCIE")
  pvNames <- paste0("PV", paste0(rep(1:5, times = 3), rep(subjects, each = 5)))
  weightsNames <- c("W_FSTUWT", paste0("W_FSTR", 1:80))
  columnTypes <- paste0(paste(rep("c", nFactors), collapse = ""), paste(rep("n", 96), collapse = ""))
  read_fwf(file = inputFilePath,
	   col_positions = fwf_positions(start = c(groupingVariablesStartPositions, variablesStartPositions),
					 end = c(groupingVariablesEndPositions, variablesEndPositions),
					 col_names = c(groupingVariablesNames, pvNames, weightsNames)),
	   col_types = columnTypes) %>% 
    mutate(studentID = 1:(dim(.)[1])) %>%
    mutate_if(is.numeric, function(x) ifelse(x %in% NAcodes, NA, x)) %>%
    addISCO(occupationNAcodes) -> tmp

    newGroups <- groupingVariablesNames[groupingVariablesNames != "motherOccupation" & groupingVariablesNames != "fatherOccupation" & groupingVariablesNames != "schoolID"]
    newGroups <- c(newGroups, "occupation")
    
    countryMeans <- lapply(subjects, function(x) mean_pv(tmp, x, "cnt", weightsNames[1], weightsNames[-1], "studentID", "schoolID"))
    iscoMeans <- lapply(subjects, function(x) mean_pv(tmp, x, newGroups,  weightsNames[1], weightsNames[-1], "studentID", "schoolID"))
    
    countryMeans <- addSubject(countryMeans, subjects)
    iscoMeans <- addSubject(iscoMeans, subjects)
    
    countryMeans <- lapply(as.list(1:3), function(x) countryMeans[[x]] %>% 
                             mutate(isco = "cnt"))
    iscoMeans <- lapply(as.list(1:3), function(x) iscoMeans[[x]] %>%
                          mutate(isco = occupation))
    
    results <- bind_rows(c(countryMeans, iscoMeans))
    if(!is.null(outputFilePath))
      save(results, file = outputFilePath)
    
    results %>%
      filter(occupation != "0") %>%
      select(-occupation) %>%
      filter(complete.cases(.)) %>%
      mutate(year = studyYear)
}
