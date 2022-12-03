#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
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
check_data_dict_categories <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  test <- test_cat_presence <- test_cat_unique <-
    tibble(name_var = as.character(), value = as.character(), condition = as.character())
  if(sum(nrow(data_dict[['Categories']])) == 0){
    warning("You data dictionary contains no categorical variables")
    return(test)}

  var_names <-
    data_dict[['Variables']] %>%
    mutate(name_var = as.character(.data$`name`)) %>%
    select(.data$`name_var`) %>% distinct

  cat_names <-
    data_dict[['Categories']] %>%
    mutate(name_var = as.character(.data$`variable`)) %>%
    fabR::add_index(.force = TRUE) %>%
    mutate(value = paste0("lines: ",.data$`index`)) %>%
    arrange(.data$`name_var`) %>%
    distinct

  test_cat_presence <-
    anti_join(cat_names,var_names,by = "name_var") %>%
    mutate(
      condition = "[ERR] - Categories not in the data dictionary") %>%
    select(.data$`name_var`, .data$`value`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  cat_names_count <-
    data_dict[['Categories']] %>%
    select(name_var = .data$`variable`,.data$`name`) %>%
    mutate(across(everything(), as.character)) %>%
    group_by(.data$`name_var`,.data$`name`) %>% add_count() %>% ungroup %>%
    filter(.data$`n` > 1)

  test_cat_unique <-
    cat_names_count %>%
    mutate(
      condition = "[ERR] - Category names not unique in the data dictionary") %>%
    select(.data$`name_var`, value = .data$`name`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct

  test <- bind_rows(test, test_cat_presence, test_cat_unique)

  return(test)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data xxx xxx xxx
#' @param data_dict xxx xxx xxx
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
check_dataset_categories <- function(data, data_dict = NULL){

  if(is.null(data_dict)) data_dict <- data_dict_extract(data)

  # test if enough data_dict or dataset
  as_data_dict_shape(data_dict)
  as_dataset(data)

  test <-
    test_cat_data_dict            <-
    test_cat_in_data_dict_only    <- test_cat_in_dataset_only <-
    test_values_in_data_dict_only <- test_values_in_dataset_only <-
    tibble(name_var = as.character(),value = as.character(), condition = as.character())

  if(sum(nrow(data_dict[['Categories']])) == 0){
    warning("You data dictionary contains no categorical variables")
    return(test)}

  test_cat_data_dict <- check_data_dict_categories(data_dict)
  # categorical content extracted from dataset
  data_dict_cat_from_data <- data_dict_extract(data)
  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    filter(.data$`name` %in% all_of(data_dict_cat_from_data[['Categories']]$`variable`))

  # categorical content extracted from data_dict
  data_dict_cat_from_data_dict <- data_dict
  data_dict_cat_from_data_dict[['Variables']] <-
    data_dict_cat_from_data_dict[['Variables']] %>%
    filter(.data$`name` %in% all_of(data_dict_cat_from_data_dict[['Categories']]$`variable`))

  # categorical variable in the data_dict, but not in the dataset
  test_cat_in_data_dict_only <-
    data_dict_cat_from_data_dict[['Variables']]['name'] %>%
    anti_join(data_dict_cat_from_data[['Variables']]['name'], by = "name") %>%
    rename(name_var = .data$`name`) %>%
    mutate(condition =
             "[INFO] - Variable declared as categorical in the data dictionary but not in the dataset")

  # categorical variable in the dataset, but not in the data_dict
  test_cat_in_dataset_only <-
    data_dict_cat_from_data[['Variables']]['name'] %>%
    anti_join(data_dict_cat_from_data_dict[['Variables']]['name'], by = "name") %>%
    rename(name_var = .data$`name`) %>%
    mutate(condition =
             "[INFO] - Categorical variable only in the dataset but not declared in the data dictionary")

  # remove already assessed
  data_dict_cat_from_data_dict[['Variables']] <-
    data_dict_cat_from_data_dict[['Variables']] %>%
    filter(!.data$`name` %in% all_of(test_cat_in_data_dict_only$`name_var`))

  data_dict_cat_from_data_dict[['Categories']] <-
    data_dict_cat_from_data_dict[['Categories']] %>%
    filter(!.data$`variable` %in% all_of(test_cat_in_data_dict_only$`name_var`))


  data_dict_cat_from_data[['Variables']] <-
    data_dict_cat_from_data[['Variables']] %>%
    filter(!.data$`name` %in% all_of(test_cat_in_dataset_only$`name_var`))

  if(sum(nrow(data_dict_cat_from_data[['Categories']])) > 0){
    data_dict_cat_from_data[['Categories']] <-
      data_dict_cat_from_data[['Categories']] %>%
      filter(!.data$`variable` %in% all_of(test_cat_in_dataset_only$`name_var`))}

  if(sum(nrow(data_dict_cat_from_data[['Categories']])) > 0){
    # categorical values in the data_dict, but not in the dataset
    test_values_in_data_dict_only <-
      anti_join(
        data_dict_cat_from_data_dict[['Categories']] %>% select(.data$`variable`, .data$`name`),
        data_dict_cat_from_data[['Categories']] %>% select(.data$`variable`, .data$`name`),
        by = c("variable", "name")) %>%
      rename(name_var = .data$`variable`) %>%
      rename(value = .data$`name`) %>%
      mutate(condition = "[INFO] - More categories declared in the data dictionary than unique values in the dataset")

    # categorical values in the dataset, but not in the data_dict
    test_values_in_dataset_only      <-
      anti_join(
        data_dict_cat_from_data[['Categories']] %>% select(.data$`variable`, .data$`name`),
        data_dict_cat_from_data_dict[['Categories']] %>% select(.data$`variable`, .data$`name`),
        by = c("variable", "name")) %>%
      rename(name_var = .data$`variable`) %>%
      rename(value = .data$`name`) %>%
      mutate(condition = "[INFO] - More unique values in the dataset than categories declared in the data dictionary")

  }

  test =
    bind_rows(test, test_cat_data_dict, test_cat_in_data_dict_only, test_cat_in_dataset_only,
              test_values_in_data_dict_only, test_values_in_dataset_only) %>%
    distinct()

  return(test)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
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
check_data_dict_variables <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  var_names <-
    data_dict[['Variables']] %>%
    add_count(.data$`name`) %>%
    filter(.data$`n` > 1) %>%
    select(.data$`name`)

  test <-
    var_names %>%
    mutate(
      condition = "[ERR] - duplicated variable name") %>%
    select(name_var = .data$`name`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.)))

  var_NA <-
    data_dict[['Variables']] %>%
    fabR::add_index(.force = TRUE) %>%
    filter(is.na(.data$`name`) | .data$`name` == "") %>%
    mutate(name = paste0("row number: ",.data$`index`)) %>%
    select(.data$`name`)

  test <-
    test %>% bind_rows(
      var_NA %>%
        mutate(
          condition = "[ERR] - missing variable name") %>%
        select(name_var = .data$`name`, .data$`condition`) %>%
        mutate(across(everything(), ~as.character(.)))) %>%
    filter(!is.na(.data$`name_var`)) %>%
    distinct

  return(test)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data xxx xxx xxx
#' @param data_dict xxx xxx xxx
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
check_dataset_variables <- function(data, data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)
  as_dataset(data)

  test_cat <- tibble(name_var = as.character(), condition = as.character())
  if(!is.null(data_dict[['Categories']])) test_cat <- check_data_dict_categories(data_dict)

  var_names_in_data_dict <-
    data_dict[['Variables']] %>%
    select(name_var = .data$`name`) %>%
    mutate(data_dict = "data_dict")

  var_names_in_dataset <-
    data %>% names %>% as_tibble() %>%
    rename(name_var = .data$`value`) %>%
    mutate(data = "data")

  test <-
    bind_rows(

      full_join(var_names_in_dataset,var_names_in_data_dict,by = "name_var") %>%
        mutate(condition = case_when(
          is.na(data_dict) ~ "[ERR] - Non existing variable in data dictionary",
          is.na(data)      ~ "[ERR] - Non existing variable in the dataset",
          TRUE ~ NA_character_)) %>%
        select(.data$`name_var`, .data$`condition`) %>%
        mutate(across(everything(), ~as.character(.))) %>%
        filter(!is.na(.data$`condition`)),

      test_cat) %>%
    distinct()

  return(test)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param var_names xxx xxx xxx
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
check_name_standards <- function(var_names){

  # var_names_valid <- make.names(
  #   paste0("X",var_names) %>% stringr::str_replace_all("-","_"))
  # 
  # test <-
  #   var_names[paste0("X",var_names)%>% stringr::str_replace_all("-","_") != var_names_valid] %>%
  #   as_tibble() %>% rename(name_var = .data$`value`) %>%
  #   mutate(
  #     condition = "[ERR] - Incompatible variable names with Maelstrom standards") %>%
  #   mutate(across(everything(), ~as.character(.))) %>%
  #   distinct()
  
  test <- tibble(name_var = as.character(), condition = as.character())

  return(test)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
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
check_data_dict_missing_categories <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  test <- tibble(name_var = as.character(), value = as.character())

  if(is.null(data_dict[['Categories']][['missing']])){
    warning("You data dictionary contains no missing column in you categorical variables")
    return(test)}

  missing_val <-
    data_dict[['Categories']] %>%
    select(name_var = .data$`variable`,.data$`missing`)

  missing_val_test <-
    missing_val %>%
    select(-.data$`name_var`) %>%
    distinct() %>%
    rowwise() %>%
    mutate(
      value = class(try(fabR::silently_run(fabR::as_any_boolean(.data$`missing`))))[1]) %>%
    filter(.data$`value` == "try-error") %>%
    inner_join(missing_val,by = "missing")

  test <-
    missing_val_test %>%
    select(.data$`name_var`,value = .data$`missing`) %>%
    mutate(
      condition = "[ERR] - incompatible missing value in the missing columns with Maelstrom standards") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  return(test)
}



#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
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
check_data_dict_valueType <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  test <- test_valueType_names <- test_valueType_cat <-
    tibble(name_var = as.character(), value = as.character(), condition = as.character())

  if(is.null(data_dict[['Variables']][['valueType']])){
    warning("Your data dictionary contains no valueType column")
    return(test)}

  vT_list <- tinyPackage::valueType_list
  test_valueType_names <-
    data_dict[['Variables']] %>%
    filter(! .data$`valueType` %in% vT_list$`valueType`) %>%
    select(name_var = .data$`name`,value = .data$`valueType`) %>%
    mutate(
      condition = "[ERR] - Incompatible valueType names with Opal standards") %>%
    mutate(across(everything(), ~as.character(.))) %>%
    distinct()

  if(length(data_dict[['Categories']]) > 0){

    vT_text <-
      vT_list %>%
      filter(.data$`toValueType` == 'text') %>% pull(.data$`valueType`)

    data_dict_vt <-
      data_dict[['Variables']] %>%
      select(name_var = .data$`name`,.data$`valueType`) %>%
      filter(! .data$`valueType` %in% vT_text)

    vT_names <-
      data_dict[['Categories']] %>%
      select(name_var = .data$`variable`,.data$`name`) %>%
      inner_join(data_dict_vt,by = "name_var")

    test_valueType_cat <-
      vT_names %>%
      select(-.data$`name_var`) %>%
      distinct() %>%
      rowwise() %>%
      mutate(test = class(try(fabR::silently_run(
        as_valueType(.data$`name`,.data$`valueType`))))[1]) %>%
      filter(.data$`test` == "try-error") %>%
      inner_join(vT_names,by = c("name", "valueType"))

    test_valueType_cat <-
      test_valueType_cat %>%
      mutate(
        condition = "[ERR] - valueType conflict in categories") %>%
      select(.data$`name_var`, value = .data$`name`, .data$`condition`) %>%
      mutate(across(everything(), ~as.character(.))) %>%
      distinct
  }

  test <- bind_rows(test_valueType_names, test_valueType_cat)

  return(test)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data xxx xxx xxx
#' @param data_dict xxx xxx xxx
#' @param valueType_guess xxx xxx xxx
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
check_dataset_valueType <- function(data, data_dict = NULL, valueType_guess = FALSE){

  if(is.null(data_dict)) data_dict <- data_dict_extract(data) %>% as_mlstr_data_dict()

  # test if enough data_dict or data
  as_data_dict_shape(data_dict)
  as_dataset(data)

  test <- test_vT_name <- test_vT_dataset <- test_vT_compatible <-
    tibble(name_var = as.character(), value = as.character(),condition = as.character())

  # check if `valueType` column exists
  if(is.null(data_dict[['Variables']][['valueType']])){
    warning("Unknown or uninitialised column: `valueType`")
    return(test)}

  data      <- data_dict_match_dataset(data, data_dict, output = "data")
  data_dict <- data_dict_match_dataset(data, data_dict, output = "data_dict")

  test_vT_name <- check_data_dict_valueType(data_dict)

  names_data <- names(data)[!names(data) %in% test_vT_name$`name_var`]

  for(i in names_data){
    # stop()}

    data_dict_vT <-
      data_dict[['Variables']][which(data_dict[['Variables']]$`name` == i),]$`valueType`

    # test valueType
    condition <- class(try(fabR::silently_run({as_valueType(data[[i]],data_dict_vT)})))[1]
    guess <- valueType_guess(data[[i]])

    test_vT_dataset <-
      rbind(
        test_vT_dataset,
        tibble(
          name_var  = i,
          value     = data_dict_vT,
          condition = ifelse(condition == 'try-error',"[ERR] - valueType conflict in dataset",NA_character_),
          suggestion = guess))
  }

  test <-
    bind_rows(test, test_vT_name, test_vT_dataset) %>%
    mutate(
      condition = ifelse(is.na(.data$`condition`) & .data$`value` %in% c("text","decimal", "integer"),
                         "[INFO] - refined valueType proposed",.data$`condition`)) %>%
    filter(.data$`value` != .data$`suggestion`) %>%
    filter(!is.na(.data$`condition`)) %>%
    distinct()

  if(valueType_guess == FALSE){
    test <- test %>% filter(!str_detect(.data$`condition`,"^\\[INFO\\]"))}

  return(test)
}
