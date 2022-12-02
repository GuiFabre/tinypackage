#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param x xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
valueType_of <- function(x){

  # check if the col is empty
  if(is.list(x) & nrow(x) %>% sum <= 1){ return(as_valueType(x = x[[1]], valueType)) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to valueType")

  type  = x %>% typeof()
  class = class(x)[[max(length(class(x)))]]

  vT_list <- tinyPackage::valueType_list

  valueType <-
    unique(vT_list[which(vT_list[['typeof']] == type & vT_list[['class']] == class),]$`toValueType`)

  if(type == "double" & class == "Date") valueType <- "date"

  fabR::silently_run(
    if(class == "factor"){
      lvls <- attributes(x)$`levels` %>% as.character()
      valueType <- try({as_valueType(lvls,"integer")   ;valueType = "integer"},silent = TRUE)
      if(class(valueType)[1] == "try-error") valueType <- try({as_valueType(lvls,"decimal");valueType = "decimal"},silent = TRUE)
      if(class(valueType)[1] == "try-error") valueType <- try({as_valueType(lvls,"date")   ;valueType = "date"   },silent = TRUE)
      if(class(valueType)[1] == "try-error") valueType <- try({as_valueType(lvls,"boolean");valueType = "boolean"},silent = TRUE)
      if(class(valueType)[1] == "try-error") valueType <- try({                             valueType = "text"   },silent = TRUE)
    }
  )
  if(length(valueType) == 0) valueType <- "text"

  return(valueType)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param ... xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
valueType_self_adjust <- function(...){

  # test data

  if(is_dataset(...) & !is_data_dict(...)){
    data <- as_dataset(...)

    is_factor <-
      data %>%
      summarise(across(everything(), ~ toString(class(.)))) %>%
      pivot_longer(everything()) %>%
      filter(.data$`value` %in% c("factor"))

    data_dict <- data_dict_extract(data)
    data_dict[['Categories']] <-
      bind_rows(
        Categories = tibble(name = as.character(),variable = as.character()),
        data_dict[['Categories']])

    for( i in names(data)) data[[i]] <- as_valueType(x = data[[i]], valueType = valueType_guess(data[[i]]))
    data_dict_final <- data_dict_extract(data)
    data_dict[['Variables']]['valueType'] <- NULL
    data_dict_final[['Variables']] <-
      data_dict_final[['Variables']][c('name','valueType')] %>%
      left_join(data_dict[['Variables']], by = c("name"))
    data_dict_final <- c(data_dict_final['Variables'], data_dict['Categories'])

    data <-
      data_dict_apply(data, data_dict_final) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.)))

    return(data)
  }

  if(!is_dataset(...) & is_data_dict(...)){
    data_dict <- as_data_dict_shape(...)

    if(sum(nrow(data_dict[['Categories']])) == 0){
      warning(
        "Your data dictionary contains no categorical variables.
valueType will remain as it is.")
      return(data_dict)

    }else{

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`name`) %>% distinct %>%
        rowwise() %>%
        mutate(valueType = valueType_guess(.data$`name`))

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`variable`,.data$`name`) %>%
        left_join(category_outcomes, by = "name") %>%
        select(.data$`variable`,.data$`valueType`) %>%
        distinct %>%
        group_by(.data$`variable`) %>%
        summarise(valueType = paste0(.data$`valueType`,collapse = "|"))

      category_outcomes <-
        data_dict[['Categories']] %>%
        select(.data$`variable`,.data$`name`) %>%
        left_join(category_outcomes, by = "variable") %>%
        group_by(.data$`variable`) %>% group_split() %>%
        lapply(function(x){
          test_vT <- str_detect(x$valueType[1], "\\|")
          if(test_vT) x <- x %>% mutate(valueType = valueType_guess(unique(x$name)))
          return(x)
        }) %>%
        bind_rows() %>%
        left_join(tinyPackage::valueType_list, by = "valueType") %>%
        select(name = .data$`variable`,proposed_tO = .data$`typeof`,proposed_vT = .data$`valueType`) %>%
        distinct

      if(length(data_dict[['Variables']][['typeof']]) > 0){

        data_dict_tO <-
          data_dict[['Variables']] %>% select(.data$`name`,.data$`typeof`) %>%
          left_join(category_outcomes, by = "name") %>%
          mutate(
            proposed_tO = ifelse(
              is.na(.data$`proposed_tO`),.data$`typeof`,.data$`proposed_tO`)) %>%
          mutate(`proposed_tO` = replace_na(.data$`proposed_tO`,'character')) %>%
          select(typeof = .data$`proposed_tO`)

        data_dict[['Variables']]['typeof'] <- data_dict_tO

      }

      if(length(data_dict[['Variables']][['valueType']]) > 0){

        data_dict_vT <-
          data_dict[['Variables']] %>% select(.data$`name`,.data$`valueType`) %>%
          left_join(category_outcomes, by = "name") %>%
          mutate(
            proposed_vT = ifelse(
              is.na(.data$`proposed_vT`),.data$`valueType`,.data$`proposed_vT`)) %>%
          mutate(`proposed_vT` = replace_na(.data$`proposed_vT`,'text')) %>%
          select(valueType = .data$`proposed_vT`)

        data_dict[['Variables']]['valueType'] <- data_dict_vT

      }

      if(length(data_dict[['Variables']][['valueType']]) == 0 &
         length(data_dict[['Variables']][['typeof']])    == 0   ) {

        data_dict_vT <-
          data_dict[['Variables']] %>%
          left_join(category_outcomes, by = "name") %>%
          rename(typeof = .data$`proposed_tO`, valueType = .data$`proposed_vT`)}

    }

    return(data_dict)
  }

  stop("The argument is neither a dataset or a data dictionary.")
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param from xxx xxx xxx
#' @param to   xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
valueType_adjust <- function(from, to = NULL){

  # test data
  if(is.null(to)) return(valueType_self_adjust(from))

  # apply the data dictionary of the dataset to the data dictionary
  if(is_dataset(from) & is_data_dict(to)){
    as_dataset(from)
    as_data_dict_shape(to)

    data <- from
    data_dict <- to

    # data must match
    if(suppressWarnings(check_dataset_variables(data, data_dict)) %>% nrow > 0){
      stop("Names across your data dictionary differ from names across the dataset.",
           crayon::bold("\n\nUseful tip:")," Use check_dataset_variables(data, data_dict) to get non existing variables")}

    vT_list<- tinyPackage::valueType_list
    vT_tables <-
      data %>%
      summarise(across(everything(), valueType_of)) %>%
      pivot_longer(cols = everything()) %>%
      rename(valueType = .data$`value`) %>%
      left_join(vT_list, by = "valueType") %>%
      select(.data$`name`, .data$`valueType`,.data$`typeof`)

    # if(length(data_dict[['Variables']][['valueType']]) == 0 &
    #    length(data_dict[['Variables']][['typeof']]) == 0 ){
    #
    #   data_dict[['Variables']]['typeof'] <-
    #     data_dict[['Variables']]['name'] %>%
    #     left_join(vT_tables %>% select(.data$`name`, .data$`typeof`), by = "name") %>%
    #     select( .data$`typeof`)
    #
    #   data_dict[['Variables']]['valueType'] <-
    #     data_dict[['Variables']]['name'] %>%
    #     left_join(vT_tables %>% select(.data$`name`, .data$`valueType`), by = "name") %>%
    #     select( .data$`valueType`)
    #
    #   return(data_dict)
    # }else{
    #
    # if(length(data_dict[['Variables']][['typeof']]) > 0){
    data_dict[['Variables']]['typeof'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>% select(.data$`name`, .data$`typeof`), by = "name") %>%
      select( .data$`typeof`)
    # }

    # if(length(data_dict[['Variables']][['valueType']]) > 0){
    data_dict[['Variables']]['valueType'] <-
      data_dict[['Variables']]['name'] %>%
      left_join(vT_tables %>% select(.data$`name`, .data$`valueType`), by = "name") %>%
      select( .data$`valueType`)
    # }

    return(data_dict)
    # }
  }

  if(is_data_dict(from) & is_dataset(to)){

    # test data dict
    tryCatch({data_dict <- as_mlstr_data_dict(from)},warning = function(cond){stop(cond)})

    # test dataset
    data <- as_dataset(to)

    # data must match
    if(suppressWarnings(check_dataset_variables(data, data_dict)) %>% nrow > 0){
      stop("Names across your data dictionary differ from names across the dataset.",
           crayon::bold("\n\nUseful tip:")," Use check_dataset_variables(data, data_dict) to get non existing variables")}

    data_dict_data <-
      data_dict_extract(data) %>%
      as_mlstr_data_dict()

    is_factor <-
      data %>%
      summarise(across(everything(), ~ class(.))) %>%
      pivot_longer(everything()) %>%
      filter(.data$`value` == "factor")

    data_dict_data[['Variables']] <-
      data_dict_data[['Variables']] %>%
      select(-.data$`valueType`) %>%
      left_join(data_dict[['Variables']] %>%
                  select(.data$`name`, .data$`valueType`),by = "name")

    for(i in names(data)){
      data[[i]] <-
        as_valueType(
          x = data[[i]],
          valueType = data_dict[['Variables']][[
            which(data_dict[['Variables']]$`name` == i),
            'valueType']])}

    data <-
      data_dict_apply(data, data_dict_data) %>%
      mutate(across(c(is_factor$`name`), ~ as.factor(.)))

    return(data)
  }

  stop("The arguments are neither or (both) a dataset or a data dictionary.")
}

#' Apply all the valueTypes of a data dictionary to a dataset
#'
#' xxx xxx xxx
#'
#' Must provide an input dataset in tibble format
#'
#' @param x A xxx xxx
#'
#' @return The dataset with the data dictionary valueType
#' applied to each variable
#'
#' @examples
#' \dontrun{
#' # use case 1: Apply valueType without specifying a data dictionary
#' as_valueType("1") %>% typeof()
#'
#' tibble(iris %>% mutate(Species = as.character(Species))) %>%
#'   mutate(across(c(starts_with("Sepal")), ~ as_valueType(.,"integer")))
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
valueType_guess <- function(x){

  # test
  x <- unique(x)

  # check if the col is empty
  if(is.list(x) & nrow(x) %>% sum <= 1){ return(as_valueType(x = x[[1]])) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to valueType")

  vT_list <- tinyPackage::valueType_list

  test_vT_boolean <- suppressWarnings(try({as_valueType(as.character(x), "boolean")}, silent = TRUE))
  test_vT_integer <- suppressWarnings(try({as_valueType(as.character(x), "integer")}, silent = TRUE))
  test_vT_decimal <- suppressWarnings(try({as_valueType(as.character(x), "decimal")}, silent = TRUE))
  test_vT_date    <- suppressWarnings(try({as_valueType(x, "date"   )}, silent = TRUE))
  test_vT_text    <-                       as_valueType(x, "text"   )

  test_vT <-
    tribble(
      ~`valueType` ,~`class`                  ,
      "boolean"    ,  class(test_vT_boolean )[1],
      "integer"    ,  class(test_vT_integer )[1],
      "decimal"    ,  class(test_vT_decimal )[1],
      "date"       ,  class(test_vT_date    )[1]) %>%
    filter(.data$`class` != "try-error") %>%
    summarise(
      valueType = paste0(.data$`valueType`,collapse = "|"),
      class = paste0(.data$`class`,collapse = "|")) %>%
    mutate(
      valueType =
        case_when(
          .data$valueType == "boolean|integer|decimal"      ~ "integer"       ,
          .data$valueType == "integer|decimal"              ~ "integer"       ,
          .data$valueType == "integer|decimal|date"         ~ "date"          ,
          .data$valueType == "decimal|date"                 ~ "date"          ,
          .data$valueType == "boolean|integer|decimal|date" ~ valueType_of(x) ,
          TRUE                                              ~  .data$valueType
        )) %>% pull(.data$`valueType`)

  if(test_vT == "") test_vT <- 'text'

  return(test_vT)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param data_dict_apply xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_extract <- function(data_dict, data_dict_apply = FALSE){

  # tests
  if(toString(attributes(data_dict)$`Mlstr::class`) == "Mlstr_data_dict"){
    data_dict <- as_mlstr_data_dict(data_dict, as_data_dict = TRUE)}else{
      data_dict <- as_data_dict(data_dict)}

  if(nrow(data_dict[['Variables']]) == 0){
    data = tibble(.rows = 0)
    return(data)}


  # valueType/typeof() is needed to generate dataset, which will be all text if not present
  vT_list <- tinyPackage::valueType_list

  data_dict_temp <- data_dict

  if(is.null(data_dict_temp[["Variables"]][['valueType']])){
    data_dict_temp[["Variables"]] <-
      data_dict_temp[["Variables"]] %>%
      left_join(
        vT_list %>%
          select(
            typeof = .data$`toTypeof`,
            valueType = .data$`toValueType`) %>%
          distinct, by = "typeof") %>%
      mutate(valueType = replace_na(.data$`valueType`, "character"))}

  data <-
    data_dict_temp[["Variables"]]  %>%
    select(.data$`name`) %>%
    pivot_wider(names_from = .data$`name`, values_from = .data$`name`) %>%
    slice(0)

  for(i in 1:ncol(data)){
    # stop()}
    data[i] <- as_valueType(x = data[i], valueType = data_dict_temp[["Variables"]]$`valueType`[i])
  }

  if(data_dict_apply == TRUE){
    data <- data_dict_apply(data, data_dict)
    return(data)
  }

  data <- as_dataset(data)
  return(data)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param dataset_list xxx xxx xxx
#' @param data_dict_apply xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
study_create <- function(dataset_list, data_dict_apply = FALSE){

  # check if listed datasets
  if(is.data.frame(dataset_list)) dataset_list <- list(dataset_list)

  study <- dataset_list %>% lapply(FUN = function(x) {
    if(data_dict_apply == TRUE){ x = tibble(data_dict_apply(x)) ; return(x)}else{return(x)}
  })

  fargs <- as.list(match.call(expand.dots = TRUE))
  if(is.null(names(dataset_list))){
    study <-
      study %>%
      stats::setNames(
        fabR::make_name_list(as.character(fargs['dataset_list']), study))}

  study <- as_study(study)

  return(study)
}



#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#' @param col_id xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_dataset <- function(object, col_id = NULL){

  # if only the tibble is given in parameter
  if(is.data.frame(object)) {

    # first column must be completly filled
    if(ncol(object) == 0){
      attributes(object)$`Mlstr::class` <- "dataset"
      attributes(object)$`Mlstr::col_id` <- NULL
      return(object)}

    # if !is.null(col_id) column must be present and completely filled
    if(!is.null(col_id)){
      col_id <- object %>% select(all_of(col_id)) %>% names
      if(sum(is.na(object[col_id])) > 0) stop("Your id column(s) must not contain any NA values.")}

    attributes(object)$`Mlstr::class` <- "dataset"
    attributes(object)$`Mlstr::col_id` <- col_id
    return(object)
  }

  # else
  stop(
    "\n\nThis object is not a dataset as defined by Maelstrom standards which must be
a data frame. Please refer to documentation.")

}




#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_study <- function(object){

  # check if names in object exist
  if(is.null(names(object)) | all(sapply(as.list(names(object)), FUN = function(x) nchar(x)) == 0 )){
    stop("One or more datasets are not named. Please provide named list of datasets.")}

  # check if names in object are unique
  if(!setequal(length(names(object)),length(unique(names(object))))){
    stop("The name of your datasets are not unique. Please provide different names.")}

  # check if listed datasets
  tryCatch(
    object <- object %>% lapply(FUN = function(x) as_dataset(x)),
    error = function(x) stop(
      "\n\nThis object is not a study as defined by Maelstrom standards.
It must be exclusively a list of (at least one) dataset(s).
Please refer to documentation."))

  attributes(object)$`Mlstr::class` <- "study"
  return(object)

}


#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_taxonomy <- function(object){

  # check if names in object exist
  if(names(object) %in% c("taxonomy","vocabulary" ,"term") %>% sum != 3){
    stop(
      "\n\nThis object is not a taxonomy as defined by Maelstrom standards.
It must be a dataframe containing at least 'taxonomy', 'vocabulary' and 'term'
columns. Please refer to documentation.",

      crayon::bold("\n\nUseful tip:")," Use opal_taxonomy_get(opal) or mlstr_taxonomy_get(opal)
to get the taxonomy present in your Opal environment.")}

  # check if names in taxonomy exist
  if(names(object) %in%
     c("vocabulary_short","taxonomy_scale",
       "vocabulary_scale","term_scale") %>% sum == 4){

    attributes(object)$`Mlstr::class` <- "mlstr_taxonomy"
  }else{
    attributes(object)$`Mlstr::class` <- "opal_taxonomy"}

  return(object)

}

#' Apply all the valueTypes of a data dictionary to a dataset
#'
#' Uses a data dictionary in the Maelstrom Research formats (with "Variables" and "Categories"
#' in separate tibbles and standard columns in each) to apply their valueType to a dataset in tibble format.
#' If no data dictionary is provided, the function will automatically evaluate the most restrictive valueType
#' for each variable in the dataset and apply it.
#'
#' Must provide an input dataset in tibble format
#'
#' @param x A xxx xxx
#' @param valueType xxx xxx
#'
#' @return The dataset with the data dictionary valueType
#' applied to each variable
#'
#' @examples
#' \dontrun{
#' # use case 1: Apply valueType without specifying a data dictionary
#' as_valueType("1") %>% typeof()
#'
#' tibble(iris %>% mutate(Species = as.character(Species))) %>%
#'   mutate(across(c(starts_with("Sepal")), ~ as_valueType(.,"integer")))
#'}
#'
#' @import dplyr tidyr fabR
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
as_valueType <- function(x, valueType = NA_character_){

  # check if the col is empty
  if(is.list(x) & sum(nrow(x)) <= 1){ return(as_valueType(x = x[[1]], valueType)) }

  # check if the col is a vector
  if(is.list(x)) stop("'list' object cannot be coerced to valueType")

  # if x is already the output format, no need to go further
  if(class(x)[1] == "Date" & valueType == "date")    return(x)
  if(is.integer(x)         & valueType == "integer") return(x)
  if(is.logical(x)         & valueType == "boolean") return(x)
  if(is.na(valueType)      | valueType == "text")    return(as.character(x))

  vT_list <- tinyPackage::valueType_list
  # check if valueType exists
  if(!valueType %in% vT_list$`valueType`) {
    stop(
      "\nThe valueType provided does not exists. Please refer to documentation.",
      crayon::bold("\n\nUseful tip:")," Use check_data_dict_valueType(data_dict) to get conflicted elements")}

  dataType <- vT_list[[which(vT_list['valueType'] == valueType),'call']]

  if(dataType     == "as_any_date") x <- as.character(x)
  if(dataType     == "as.logical")  x <- as.integer(x)
  if( class(x)[1] == "factor")      x <- as.character(x)

  x_temp <- do.call(dataType, list(x)) %>% unlist

  condition <- tibble(to_test = x_temp, original = x)

  if(length(x_temp) == 0){
    return(x_temp)}

  if(valueType %in% c("text","locale","point","linestring","polygon","binary")){
    return(x_temp)}

  if(!all(is.na(condition$`to_test`) == is.na(condition$`original`))){
    test_condition <- FALSE
  }else{

    test_condition <- condition[which(!is.na(condition['original'])),] %>% distinct()

    if(valueType %in% c("integer","decimal")){
      test_condition <- test_condition %>%
        mutate(across(everything(), ~ as.numeric(.))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("boolean")){
      test_condition <- test_condition %>%
        mutate(across(everything(), ~ as_any_boolean(.))) %>%
        mutate(test = .data$`to_test` == .data$`original`) %>%
        pull(.data$`test`) %>% all}

    if(valueType %in% c("date","datetime")){
      test_condition <-
        test_condition <- test_condition %>%
        mutate(across(everything(), ~ fabR::silently_run(as_any_date(.)))) %>%
        mutate(test = toString(.data$`to_test`) == toString(.data$`original`)) %>%
        pull(.data$`test`) %>% all}
  }

  # test if data and data_dict content match

  if(test_condition == FALSE){
    stop(
      "\n\nThe valueType conflicts with the data type. Object cannot be coerced to valueType",
      crayon::bold("\n\nUseful tip:")," Use valueType_guess(x) to evaluate the first potential valueType.
For further investigation, you can use check_dataset_valueType(data, data_dict).")
  }

  return(x_temp)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
is_dataset <- function(object){

  object <- object
  # if only the tibble is given in parameter
  if(class(try(fabR::silently_run(as_dataset(object))))[1] == 'try-error') return(FALSE)
  return(TRUE)

}



#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
is_study <- function(object){

  object <- object
  # if only the study is given in parameter
  if(class(try(fabR::silently_run(as_study(object))))[1] == 'try-error') return(FALSE)
  return(TRUE)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
is_valueType <- function(object){

  object <- object
  vT_list <- tinyPackage::valueType_list
  # check if valueType exists
  if(!all(object %in% vT_list$`valueType`)) return(FALSE)

  # else
  return(TRUE)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' xxx xxx xxx
#'
#' @param object xxx xxx xxx
#'
#' @return xxx xxx xxx
#'
#' @examples
#' \dontrun{
#'
#' # example
#'
#'}
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
is_taxonomy <- function(object){

  object <- object
  # if only the tibble is given in parameter
  if(class(try(fabR::silently_run(as_taxonomy(object))))[1] == 'try-error') return(FALSE)
  return(TRUE)

}

