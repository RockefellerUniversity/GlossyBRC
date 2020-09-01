#' Get all of the terms and corresponding information in the current glossary
#'
#' Get all of the terms and corresponding information in the current glossary
#'
#' @rdname get_glossary
#' @param select_terms A character vector with terms corresponding to those in the glossary. If this is left NULL, all terms in the glossary will be retrieved.
#' @importFrom yaml read_yaml
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr "%>%"
#' @export
#' @docType methods
#' @return  A list where each element is a list for each term, which is comrised of nested lists with relevant information.

#' @export
get_glossary <- function(select_terms = NULL, data_frame = FALSE){

  yamls <- list.files("inst/yaml/", full.names = TRUE, recursive = TRUE)

  all_yaml_list <- list()

  for (i in seq_along(yamls)){
    yaml_temp <- read_yaml(yamls[i])
    name_temp <- file_path_sans_ext(yamls[i]) %>%
      basename()
    all_yaml_list[name_temp] <- yaml_temp

  }
  if (is.null(select_terms)){
    if (data_frame == TRUE){
      return(do.call(rbind, lapply(all_yaml_list, data.frame)))
    } else {
      return(all_yaml_list)
    }
  } else {
    for (i in seq_along(select_terms)){
      if (!select_terms[i] %in% names(all_yaml_list)){
        stop(paste0(select_terms[i], " is not in the glossary! Check the elements in the gloassary with 'list_terms()'. "))
      }
    }
    yaml_subset <- all_yaml_list[select_terms]
    if (data_frame == TRUE){
      return(do.call(rbind, lapply(yaml_subset, data.frame)))
    } else {
      return(yaml_subset)
    }
  }

}

#' List the terms in the current glossary
#'
#' List the terms in the current glossary
#'
#' @rdname list_terms
#' @param categories To only retrieve the terms from particular categories, a character vector of those categories can be given. Otherwise, all terms from all categories will be returned as separate lists for each category. To see the possible categories, use the list_categories() function.
#' @export
#' @docType methods
#' @return  A list where each element is a list for each category of terms

#' @export
list_terms <- function(select_categories = NULL){

  category_dirs <- list.files("inst/yaml/")
  categories <- file_path_sans_ext(category_dirs)

  category_list <- list()
  for (i in seq_along(categories)){
    terms <- list.files(paste0("inst/yaml/", categories[i]))
    category_list[[i]] = file_path_sans_ext(terms)
  }
  names(category_list) <- categories

  if (is.null(select_categories)){
    return(category_list)
  } else {
    for (i in seq_along(select_categories)){
      if (!select_categories[i] %in% names(category_list)){
        stop(paste0(select_categories[i], " is not in the glossary! Check the categories in the glossary with 'list_categories()'. "))
      }
    }
    category_subset <- category_list[select_categories]
    return(category_subset)
  }
}

#' List the categories in the current glossary
#'
#' List the categories in the current glossary
#'
#' @rdname list_categories
#' @export
#' @docType methods
#' @return  A list where each element is a list for each term, which is comrised of nested lists with relevant information.

#' @export
list_categories <- function(){

  category_dirs <- list.files("inst/yaml/")
  categories <- file_path_sans_ext(category_dirs)

  return(categories)
}
