#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param study xxx xxx xxx
#' @param dataset xxx xxx xxx
#' @param data_dict xxx xxx xxx
#' @param taxonomy xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#'
#' dataset   <- study_TOKYO %>% mutate(dob = as_any_date(dob, format = "mdy"))
#' data_dict <- dd_TOKYO_format_maelstrom_tagged
#'
#' study_assessement_report(dataset, data_dict)
#'
#' }
#'
#' @import dplyr haven
#' @export
study_assessement_report <- function(study = NULL, dataset = NULL, data_dict = NULL, taxonomy = NULL){

  fargs <- as.list(match.call(expand.dots = TRUE))

  # check on arguments
  if((!is.null(fargs[['dataset']]) | !is.null(fargs[['data_dict']])) & !is.null(fargs[['study']]))     stop("Too many argments entered")
  if(is.null(fargs[['dataset']]) & is.null(fargs[['data_dict']]) & is.null(fargs[['study']]))          stop("At least one argument is missing")

  # cas de une etude
  if(!is.null(fargs[['study']])){

    study <- as_study(study)
    study_temp <-
      vector(mode = "list", length = length(names(study))) %>%
      stats::setNames(names(study))

    for(i in names(study)){
      study_temp[[i]][['dataset']]   <- study[[i]]
      study_temp[[i]][['data_dict']] <- data_dict_extract(study[[i]])
    }

    study <- study_temp
  }

  # cas d'un data_dict seul
  if(!is.null(fargs[['data_dict']]) & is.null(fargs[['dataset']])) {

    # test if enough data_dict
    as_data_dict(data_dict['Variables'])
    data_dict_temp <- data_dict
    dataset <- data_extract(data_dict_temp)

    study <-
      vector(mode = "list", length = 1) %>%
      stats::setNames(fabR::make_name_list(as.character(fargs['data_dict']), list(NULL)))

    study[[1]][['dataset']]   <- dataset
    study[[1]][['data_dict']] <- data_dict
  }

  # cas d'un dataset seul
  if(is.null(fargs[['data_dict']]) & !is.null(fargs[['dataset']])) {

    # test if enough dataset
    as_dataset(dataset)

    message("Creation of data dictionary")
    data_dict <- data_dict_extract(dataset)
    study <-
      vector(mode = "list", length = 1) %>%
      stats::setNames(fabR::make_name_list(as.character(fargs['dataset']), list(NULL)))

    study[[1]][['dataset']]   <- dataset
    study[[1]][['data_dict']] <- data_dict
  }

  # cas d'un data_dict et d'un dataset
  if(!is.null(fargs[['dataset']]) & !is.null(fargs[['data_dict']])) {

    # test if enough dataset and data_dict
    as_dataset(dataset)
    as_data_dict_shape(data_dict)

    study <-
      vector(mode = "list", length = 1) %>%
      stats::setNames(fabR::make_name_list(as.character(fargs['dataset']), list(NULL)))

    study[[1]][['dataset']]   <- dataset
    study[[1]][['data_dict']] <- data_dict
  }

  report_list <-
    vector(mode = "list", length = length(names(study))) %>%
    stats::setNames(names(study))

  message(crayon::bold(
    "- DATA ASSESSMENT: -------------------------------------------------------------"))

  for(i in 1:length(study)){
    # stop()}

    dataset_name   <- study[i] %>% names
    dataset        <- study[[dataset_name]][['dataset']] %>% zap_labels() %>% ungroup()
    data_dict      <- study[[dataset_name]][['data_dict']] %>% data_dict_ungroup()
    data_dict_name <- study[i] %>% names
    data_dict[['Variables']] <- data_dict[['Variables']] %>% fabR::add_index(.force = TRUE)

    message(
      "---- dataset : ",crayon::bold(dataset_name),if(dataset %>% nrow == 0) " (empty dataset)")

    # create report structure
    report <- list(
      `Overview` = c(),
      `Participants` = tibble(),
      `Data dictionary summary` = tibble(),
      `Text variable summary` = tibble(),
      `Date variable summary` = tibble(),
      `Numerical variable summary` = tibble(),
      `Categorical variable summary` = tibble())

    ### RESHAPE ELEMENTS ###

    # simplify elements

    if(is.null(data_dict[['Categories']])){
        data_dict[['Categories']] <- tibble(
          variable = as.character(),
          name = as.character(),
          missing = as.character())}

    # rename column for later full_join

    ## variables
    data_dict_var <-
      data_dict[['Variables']] %>%
      select(-matches("^name_var$")) %>%
      rename(name_var = .data$`name`) %>%
      rename(`index in data dict.` = .data$`index`) %>%
      select(.data$`index in data dict.`, everything())

    ## categories

    if(data_dict[['Categories']] %>% nrow > 0){
      data_dict_cat <-
        data_dict[['Categories']] %>%
        select(-matches("^name_var$")) %>%
        rename(name_var = .data$`variable`) %>%
        mutate(missing = ifelse(.data$`missing` == "TRUE", "Missing categorical values :", "Valid categorical values :")) %>%
        unite("Categories in data dictionary", .data$`name`,starts_with("label:"), sep = " = ",remove = TRUE) %>%
        group_by_at(vars(c(-.data$`Categories in data dictionary`))) %>%
        summarise(across(c(.data$`Categories in data dictionary`),~ paste0(.,collapse = "\n")),.groups = "drop") %>%
        arrange(.data$`name_var`,desc(.data$`missing`)) %>%
        unite("Categories in data dictionary", .data$`missing`,.data$`Categories in data dictionary`, sep = "\n",remove = TRUE) %>%
        group_by_at(vars(c(-.data$`Categories in data dictionary`))) %>%
        summarise(across(c(.data$`Categories in data dictionary`),~ paste0(.,collapse = "\n\n")),.groups = "drop") %>%
        mutate(`Categories in data dictionary` = ifelse(is.na(.data$`name_var`), NA, .data$`Categories in data dictionary`)) %>%
        ungroup
    }else{
      data_dict_cat <- tibble(name_var = as.character(),`Categories in data dictionary` = NA_character_)}

    ### ASSESSMENT ###

    message("Assess names of variables in dataset") # +table
    odd_names <-
      check_name_standards(
        c(data_dict[['Variables']] %>% pull(.data$`name`),
          data_dict[['Categories']] %>% pull(.data$`variable`) %>% unique)) %>%
      bind_rows(check_name_standards(dataset %>% names)) %>%
      rename(`Quality assessment comment` = .data$`condition`) %>%
      distinct %>%
      group_by_at(vars(c(.data$`name_var`, .data$`Quality assessment comment`))) %>%
      mutate(table = ifelse(.data$`name_var` %in% data_dict[['Variables']]$`name`, "data_dict|dataset", "dataset")) %>%
      select(.data$`table`, .data$`name_var`, .data$`Quality assessment comment`) %>%
      mutate(across(everything(), ~ as.character(.))) %>%
      ungroup()

    message("Assess the data type of each variable found in the dataset") # +table
    dataset_summary <-
      dataset %>%
      summarise(across(everything(), ~ class(.)[1])) %>%
      pivot_longer(cols = everything(),names_to = "name_var",values_to = "Dataset data type") %>%
      mutate(table =  "dataset") %>%
      select(.data$`table`,.data$`name_var`, .data$`Dataset data type`)

    if(data_dict_cat %>% nrow > 0 & dataset %>% nrow > 0){
      message("Assess the presence of categories not in the data dictionary") # +table
      unknown_cat <-
        check_existing_category(data_dict) %>%
        rename(`Quality assessment comment` = .data$`condition`) %>%
        unite("name_var", .data$`name_var`,.data$`value`, sep = "::") %>%
        mutate(table = "data_dict") %>%
        select(.data$`table`, .data$`name_var`, .data$`Quality assessment comment`)
    }else{unknown_cat <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    message("Assess the presence of variables in the dataset that are not in the data dictionary") # +table
    unknown_var <-
      check_dataset_variables(data = dataset, data_dict = data_dict) %>%
      mutate(table = "data_dict|dataset") %>%
      rename(`Quality assessment comment` = .data$`condition`) %>%
      select(.data$`table`, .data$`name_var`, .data$`Quality assessment comment`)


    # message("Assess the presence of variables in the data dictionary that are not in the dataset")
    # unknown_var <-
    #   check_existing_variable(data_dict,dataset) %>%
    #   rename(`Quality assessment comment` = .data$`condition`)

    if(dataset %>% nrow > 0){
      message("Assess the presence of duplicated variables in the dataset") # + table
      duplicated_columns <-
        fabR::get_duplicated_cols(dataset) %>%
        rename(`Quality assessment comment` = .data$`condition`) %>%
        mutate(table = ifelse(.data$`name_var` %in% data_dict[['Variables']]$`name`, "data_dict|dataset", "dataset")) %>%
        mutate(table = "dataset") %>%
        select(.data$`table`,.data$`name_var`, .data$`Quality assessment comment`, .data$`table`) %>%
        mutate(across(everything(), ~ as.character(.)))
    }else{duplicated_columns <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(dataset %>% nrow > 0){
      message("Assess the presence of possible duplicated participants in the dataset") # + table
      duplicated_rows <-
        fabR::get_duplicated_rows(dataset) %>%
        rename(`Quality assessment comment` = .data$`condition`) %>%
        mutate(table = "dataset") %>%
        select(.data$`table`, .data$`Quality assessment comment`) %>%
        separate(.data$`Quality assessment comment`, into = c("Quality assessment comment", "Participant identifier"),sep = " : ")
    }else{duplicated_rows <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(dataset %>% nrow > 0){
      message("Assess the presence of all empty responses participants in dataset") # + table
      participant_empty <-
        fabR::get_all_na_rows(dataset) %>%
        rename(
          `Quality assessment comment` = .data$`condition`) %>%
        mutate(table = "dataset") %>%
        select(.data$`table`, .data$`Quality assessment comment`, `Participant identifier` = .data$`participant`)
    }else{participant_empty <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(data_dict_cat %>% nrow > 0 & dataset %>% nrow > 0){
      message("Compare values present in dataset and declared in data dictionary") # + table
      check_category_values <-
        check_cat_values(dataset, data_dict) %>%
        rename(
          `Quality assessment comment` = .data$`condition`,
          `Mix cat. and other values` = .data$`categorical`) %>%
        mutate(
          table = "data_dict|dataset",
          `Mix cat. and other values` = ifelse(.data$`Mix cat. and other values` == "dual","YES",NA_character_))
    }else{check_category_values <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(dataset %>% nrow > 0 & FALSE){
      message("Compare valueType in data dictionary and dataset data type in dataset") # + table
      check_valueType_adequacy <-
        check_dataset_valueType(dataset, data_dict) %>%
        rename(
          `Quality assessment comment` = .data$`condition`) %>%
        filter(!is.na(.data$`Quality assessment comment`)) %>%
        mutate(table = "data_dict|dataset") %>%
        select(.data$`table`,.data$`name_var`, .data$`Quality assessment comment`)
    }else{check_valueType_adequacy <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(dataset %>% nrow > 0){
      message("Assess the presence of empty columns in dataset") # + table
      dataset_empty <-
        fabR::get_all_na_cols(dataset) %>%
        rename(
          `Quality assessment comment` = .data$`condition`) %>%
        mutate(table = "dataset") %>%
        select(.data$`table`,.data$`name_var`, .data$`Quality assessment comment`) %>%
        mutate(across(everything(), ~ as.character(.)))
    }else{dataset_empty <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    if(dataset %>% nrow > 0){
      message("Assess the presence of unique value columns in dataset")  # + table
      dataset_unique_value <-
        fabR::get_unique_value_cols(dataset) %>%
        rename(
          `Quality assessment comment` = .data$`condition`) %>%
        mutate(table = "dataset") %>%
        select(.data$`table`,.data$`name_var`, .data$`Quality assessment comment`) %>%
        mutate(across(everything(), ~ as.character(.)))
    }else{dataset_unique_value <- dataset_summary %>% slice(0) %>% add_column(`Quality assessment comment` = NA_character_)}

    ### JOIN INFORMATION ###

    # join comments
    join_comments <-
      unknown_cat %>%
      full_join(unknown_var,by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(odd_names,  by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(check_category_values %>% filter(!is.na(.data$`Quality assessment comment`)),  by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(duplicated_columns,  by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(check_valueType_adequacy,  by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(dataset_empty,  by = c("table", "name_var", "Quality assessment comment")) %>%
      full_join(dataset_unique_value,  by = c("table", "name_var", "Quality assessment comment")) %>%
      group_by(.data$`table`,.data$`name_var`) %>%
      summarise(across(c(.data$`Quality assessment comment`),~ paste0(.,collapse = "\n")), .groups = "drop")

    # full_join variables and categories
    report$`Data dictionary summary` <-
      suppressMessages({                                         # mute the left join
        data_dict_var %>%
          left_join(data_dict_cat, by = "name_var") %>%
          full_join(check_category_values %>% select(.data$`name_var`, matches("Mix cat. and other values")), by = "name_var") %>%
          full_join(join_comments, by = "name_var") %>%
          full_join(dataset_summary %>% select(-.data$`table`), by = "name_var") %>%
          select(.data$`index in data dict.`, .data$`table`, `name` = .data$`name_var`,
                 starts_with("label:"),
                 .data$`valueType`,.data$`Dataset data type`,
                 matches("Mix cat. and other values"),matches("Categories in data dictionary"),
                 .data$`Quality assessment comment`, everything()) %>%
          mutate(table = ifelse(is.na(.data$`table`), ifelse(.data$`name` %in% data_dict$Variables$`name`, "data_dict|dataset", "dataset"),.data$`table`))
    })

    ### SUMMARIZE VARIABLES VALUES ###

    if(dataset %>% nrow > 0){
      message("Summarise information about participants") # + table
      if(nrow(duplicated_rows) == 0){
        duplicated_rows <- tibble(table = "dataset", `Quality assessment comment` = "No duplicated participant found" )}

      if(nrow(participant_empty) == 0){
        participant_empty <- tibble(table = "dataset", `Quality assessment comment` = "All participant have at least one variable filled" )}
    }

    report$`Participants` = bind_rows(duplicated_rows, participant_empty)

    message("Summarise information for all variables")
    var_all <-
      report$`Data dictionary summary` %>%
      pull(.data$`name`) %>%
      intersect(dataset %>% names)

    report$`Text variable summary`        = report$`Data dictionary summary` %>% filter(.data$`Dataset data type` %in% c("character")) %>% select(2:3) %>% mutate(empty = TRUE)
    report$`Date variable summary`        = report$`Data dictionary summary` %>% filter(.data$`Dataset data type` %in% c("Date")) %>% select(2:3) %>% mutate(empty = TRUE)
    report$`Numerical variable summary`   = report$`Data dictionary summary` %>% filter(.data$`Dataset data type` %in% c("numeric", "logical")) %>% select(2:3) %>% mutate(empty = TRUE)
    report$`Categorical variable summary` = report$`Data dictionary summary` %>% filter(!is.na(.data$`Categories in data dictionary`)) %>% select(2:3) %>% mutate(empty = TRUE)

    if(nrow(dataset) > 0){

      summary <- tibble()
      for(var_temp in var_all){
        summary <-
          summary %>%
          bind_rows(
            summary_variables(
              name_var = dataset %>% select(all_of(var_temp)),
              dict_var = data_dict$Categories %>% filter(.data$`variable` == var_temp)) %>% rename(name = .data$`name_var`))}

      var_text <-
        report$`Data dictionary summary` %>%
        filter(.data$`Dataset data type` %in% c("character")) %>%
        pull(.data$`name`) %>%
        intersect(dataset %>% names)

      if(length(var_text)){

        message("Summarise information for text variables")
        summary_text <- tibble(name = as.character())
        for(var_t in var_text){
          summary_text <-
            summary_text %>%
            bind_rows(
              summary_variables(
                name_var = dataset %>% select(all_of(var_t)),
                dict_var = data_dict$Categories %>% filter(.data$`variable` == var_t)) %>%
                rename(name = .data$`name_var`))}

        report$`Text variable summary` <-
          report$`Data dictionary summary` %>%
          filter(.data$`name` %in% var_text) %>%
          select(.data$`index in data dict.`, .data$`table`, .data$`name`,
                 starts_with("label:"), .data$`valueType`,.data$`Dataset data type`,
                 matches("Mix cat. and other values"), .data$`Quality assessment comment`) %>%
          left_join(summary, by = "name") %>%
          inner_join(summary_text, by = "name") %>%
          mutate(across(matches("Mix cat. and other values"),
                        `% Missing categorical values (if applicable)` =
                          ifelse(.data$`Mix cat. and other values` == "YES",
                                 .data$`% Missing categorical values (if applicable)`,NA)))}

      var_date <-
        report$`Data dictionary summary` %>%
        filter(.data$`Dataset data type` %in% c("Date")) %>%
        pull(.data$`name`) %>%
        intersect(dataset %>% names)

      if(length(var_date)){

        message("Summarise information for date variables")
        summary_date <- tibble(name = as.character())
        for(var_d in var_date){
          summary_date <-
            summary_date %>%
            bind_rows(
              summary_variables_date(
                name_var = dataset %>% select(all_of(var_d)),
                dict_var = data_dict$Categories %>% filter(.data$`variable` == var_d)) %>%
                rename(name = .data$`name_var`))}

        report$`Date variable summary` <-
          report$`Data dictionary summary` %>%
          filter(.data$`name` %in% var_date) %>%
          select(.data$`index in data dict.`, .data$`table`, .data$`name`, starts_with("label:"),
                 .data$`valueType`,.data$`Dataset data type`,matches("Mix cat. and other values"),
                 .data$`Quality assessment comment`) %>%
          left_join(summary, by = "name") %>%
          inner_join(summary_date, by = "name") %>%
          mutate(across(matches("Mix cat. and other values"),
                        `% Missing categorical values (if applicable)` =
                          ifelse(.data$`Mix cat. and other values` == "YES",
                                 .data$`% Missing categorical values (if applicable)`,NA)))}

      var_numerical <-
        report$`Data dictionary summary` %>%
        filter(.data$`Dataset data type` %in% c("numeric", "logical")) %>%
        pull(.data$`name`) %>%
        intersect(dataset %>% names)

      if(length(var_numerical)){

        message("Summarise information for numerical variables")
        summary_numerical <- tibble(name = as.character())
        for(var_n in var_numerical){
          summary_numerical <-
            summary_numerical %>%
            bind_rows(
              summary_variables_numerical(
                name_var = dataset %>% select(all_of(var_n)),
                dict_var = data_dict$Categories %>% filter(.data$`variable` == var_n)) %>%
                rename(name = .data$`name_var`))}

        report$`Numerical variable summary` <-
          report$`Data dictionary summary` %>%
          filter(.data$`name` %in% var_numerical) %>%
          select(.data$`index in data dict.`, .data$`table`,
                 .data$`name`, starts_with("label:"), .data$`valueType`,
                 .data$`Dataset data type`,matches("Mix cat. and other values"),
                 .data$`Quality assessment comment`) %>%
          left_join(summary, by = "name") %>%
          inner_join(summary_numerical, by = "name") %>%
          mutate(across(matches("Mix cat. and other values"),
                        `% Missing categorical values (if applicable)` =
                          ifelse(.data$`Mix cat. and other values` == "YES",
                                 .data$`% Missing categorical values (if applicable)`,NA)))}

      var_categorical <-
        report$`Data dictionary summary` %>%
        filter(!is.na(.data$`Categories in data dictionary`)) %>%
        pull(.data$`name`) %>%
        intersect(dataset %>% names)

      if(length(var_categorical)){

        message("Summarise information for categorical variables")
        summary_categorical <- tibble(name = as.character())
        for(var_c in var_categorical){
          summary_categorical <-
            summary_categorical %>%
            bind_rows(
              summary_variables_categorical(
                name_var = dataset %>% select(all_of(var_c)),
                dict_var = data_dict$Categories %>% filter(.data$`variable` == var_c)) %>%
                rename(name = .data$`name_var`))}

        report$`Categorical variable summary` <-
          report$`Data dictionary summary` %>%
          filter(!is.na(.data$`Categories in data dictionary`)) %>%
          select(.data$`index in data dict.`,.data$`table`, .data$`name`,
                 starts_with("label:"), .data$`valueType`, .data$`Dataset data type`,
                 matches("Mix cat. and other values"), .data$`Quality assessment comment`,
                 .data$`Categories in data dictionary`) %>%
          left_join(summary, by = "name") %>%
          inner_join(summary_categorical, by = "name") %>%
          select(everything(),
                 -matches("Categories in data dictionary"),-matches("% Valid categorical values"),-matches("Values present in dataset"),
                 -matches("Data dictionnary categories not present in dataset"), -matches("Dataset value not present in data dictionnary"),

                 matches("% Valid categorical values"),matches("Categories in data dictionary"),matches("Values present in dataset"),
                 matches("Data dictionnary categories not present in dataset"),matches("Dataset value not present in data dictionnary"))}
    }

    message("Summarise global information (Overview)")
    report$Overview <-
      tibble(`---` = c(
        'Quality control of study specific dataset'                                ,
        'Date'                                                                     ,
        '1_Name of the dataset'                                            ,
        '    1_Participant identifier'                                             ,
        '    1_Variables'                                                          ,
        '        1_Total number of variables'                                      ,
        '        1_Nb. categories not in the data dictionary'                      ,
        '        1_Nb. incompatible variable names with Maelstrom standards'       ,
        '    1_Data type in dictionnary (valueType)'                                                         ,
        '        1_Nb. text variables'                                             ,
        '        1_Nb. date variables'                                             ,
        '        1_Nb. numerical variables'                                        ,
        '        1_Nb. categorical variables'                                      ,
        '2_Name of the dataset'                                                    ,
        '    2_Participant identifier'                                             ,
        '    2_Rows'                                                               ,
        '        2_Total number of observations'                                   ,
        '        2_Nb. unique observations'                                        ,
        '        2_Nb. unique participants'                                        ,
        '        2_Nb. potential duplicated participants'                          ,
        '        2_Nb. empty observations'                                         ,
        '    2_Columns'                                                            ,
        '        2_Total number of variables'                                      ,
        '        2_Nb. empty columns'                                              ,
        '        2_Nb. columns containing only one value'                          ,
        '        2_Nb. incompatible variable names with Maelstrom standards'       ,
        '        2_Nb. variables in the dataset not present in the data dictionary',
        '        2_Nb. variables in the data dictionary not present in the dataset',
        '        2_Nb. potential duplicated variables'                             ,
        '    2_Data type in dataset'                                               ,
        '        2_Nb. text variables'                                             ,
        '        2_Nb. date variables'                                             ,
        '        2_Nb. numerical variables'                                        ,
        '        2_Nb. categorical variables'                                      ,
        '        2_Nb. variables where valueType conflicts with dataset data type'       ))

      report$Overview <-
        report$Overview %>%
        mutate(`-----` = case_when(

          .data$`---` == 'Quality control of study specific dataset'                      ~ " ",
          .data$`---` == 'Date'                                                           ~ Sys.Date() %>% as.character,
          .data$`---` == '1_Name of the dataset'                                          ~ dataset_name %>% str_remove_all("`"),
          .data$`---` == '    1_Participant identifier'                                   ~ data_dict$Variables$name[1],
          .data$`---` == '    1_Variables'                                                ~ " ",
          .data$`---` == '        1_Total number of variables'                            ~ report$`Data dictionary summary`   %>% filter(str_detect(.data$`table`, "data_dict")) %>% nrow() %>% as.character,
          .data$`---` == '        1_Nb. categories not in the data dictionary'                ~ report$`Data dictionary summary`   %>% filter(str_detect(.data$`table`, "data_dict") & str_detect(.data$`Quality assessment comment`, "Categories not in the data dictionary")) %>% nrow() %>% as.character,
          .data$`---` == '        1_Nb. incompatible variable names with Maelstrom standards' ~ join_comments    %>% filter(str_detect(table, "data_dict") & str_detect(.data$`Quality assessment comment`, "Incompatible variable names with Maelstrom standards")) %>% nrow() %>% as.character,
          .data$`---` == '    1_Data type in dictionnary (valueType)'                     ~ " ",
          .data$`---` == '        1_Nb. text variables'                                   ~ report$`Text variable summary`           %>% filter(str_detect(.data$`table`, "data_dict")) %>% nrow() %>% as.character,
          .data$`---` == '        1_Nb. date variables'                                   ~ report$`Date variable summary`           %>% filter(str_detect(.data$`table`, "data_dict")) %>% nrow() %>% as.character,
          .data$`---` == '        1_Nb. numerical variables'                              ~ report$`Numerical variable summary` %>% filter(str_detect(.data$`table`, "data_dict")) %>% nrow() %>% as.character,
          .data$`---` == '        1_Nb. categorical variables'                            ~ report$`Categorical variable summary`    %>% filter(str_detect(.data$`table`, "data_dict")) %>% nrow() %>% as.character,
          .data$`---` == '2_Name of the dataset'                                          ~ dataset_name %>% str_remove_all("`"),
          .data$`---` == '    2_Participant identifier'                                   ~ dataset %>% select(1) %>% names,
          .data$`---` == '    2_Rows'                                                     ~ " ",
          .data$`---` == '        2_Total number of observations'                         ~ dataset %>% select(1) %>% nrow %>% as.character,
          .data$`---` == '        2_Nb. unique observations'                              ~ dataset %>% distinct  %>% nrow %>% as.character,
          .data$`---` == '        2_Nb. unique participants'                              ~ dataset %>% select(1) %>% distinct() %>% nrow %>% as.character,
          .data$`---` == '        2_Nb. potential duplicated participants'                ~ duplicated_rows %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Possible duplicated participants")) %>% nrow %>% as.character,
          .data$`---` == '        2_Nb. empty observations'                               ~ participant_empty %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Empty participant")) %>% nrow %>% as.character,
          .data$`---` == '    2_Columns'                                                  ~ " ",
          .data$`---` == '        2_Total number of variables'                            ~ dataset %>% ncol %>% as.character,
          .data$`---` == '        2_Nb. empty columns'                                    ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Empty column")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. columns containing only one value'                           ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Unique value in the colomn")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. incompatible variable names with Maelstrom standards'        ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Incompatible variable names with Maelstrom standards")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. variables in the dataset not present in the data dictionary' ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Non existing variable in data dictionary")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. variables in the data dictionary not present in the dataset' ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Non existing variable in the dataset")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. potential duplicated variables'                              ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "Possible duplicated variables")) %>% nrow() %>% as.character,
          .data$`---` == '    2_Data type in dataset'                                     ~ " ",
          .data$`---` == '        2_Nb. text variables'                                   ~ report$`Text variable summary`           %>% filter(str_detect(.data$`table`, "dataset")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. date variables'                                   ~ report$`Date variable summary`           %>% filter(str_detect(.data$`table`, "dataset")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. numerical variables'                              ~ report$`Numerical variable summary`      %>% filter(str_detect(.data$`table`, "dataset")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. categorical variables'                            ~ report$`Categorical variable summary`    %>% filter(str_detect(.data$`table`, "dataset")) %>% nrow() %>% as.character,
          .data$`---` == '        2_Nb. variables where valueType conflicts with dataset data type' ~ join_comments %>% filter(str_detect(.data$`table`, "dataset") & str_detect(.data$`Quality assessment comment`, "valueType conflict")) %>% nrow() %>% as.character,
          TRUE                                                                                ~ "EMPTY",
        )) %>%
        mutate(
          `---` = str_remove_all(.data$`---`, "1_"),
          `---` = str_remove_all(.data$`---`, "2_")) %>%
        rename(
          `Quality control of study specific dataset` = .data$`---`,
          ` ` = .data$`-----`) %>% slice(-1)

      message("Generate report\n")

      # create report structure
      if(report$`Participants` %>% nrow == 0)                                                report$`Participants`                <- NULL
      if(report$`Text variable summary`        %>% select(matches("^empty$")) %>% ncol == 1) report$`Text variable summary`       <- NULL
      if(report$`Date variable summary`        %>% select(matches("^empty$")) %>% ncol == 1) report$`Date variable summary`       <- NULL
      if(report$`Numerical variable summary`   %>% select(matches("^empty$")) %>% ncol == 1) report$`Numerical variable summary`  <- NULL
      if(report$`Categorical variable summary` %>% select(matches("^empty$")) %>% ncol == 1) report$`Categorical variable summary`<- NULL

      report_list[[i]]    <- report
    }

  return(report_list)
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
#' @import dplyr
check_existing_category <- function(data_dict){

  var_names <-
    data_dict$Variables %>%
    mutate(unique_name = .data$`name`)

  cat_names <-
    data_dict$Categories %>%
    mutate(unique_name = .data$`variable`)

  test <-
    anti_join(cat_names,var_names,by = "unique_name") %>%
    mutate(
      condition = "[ERR] - Categories not in the data dictionary") %>%
    select(name_var = .data$`variable`, value = .data$`name`, .data$`condition`, everything(), -.data$`unique_name`) %>%
    unique %>%
    mutate(across(everything(), ~as.character(.)))

  return(test)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param dataset xxx xxx xxx
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
#' @import dplyr
check_existing_variable <- function(data_dict, dataset){

  var_names_in_data_dict <-
    data_dict$Variables %>%
    select(.data$`table`, name_var = .data$`name`)

  var_names_in_dataset <- tibble(name_var = names(dataset), dataset = "dataset")

  test <-
    full_join(var_names_in_dataset,var_names_in_data_dict,by = "name_var") %>%
    mutate(table = case_when(
      is.na(.data$`dataset`) ~ "data_dict",
      is.na(.data$`table`) ~ "dataset",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(.data$`table`)) %>%
    mutate(condition = case_when(
      .data$`table` == "dataset" ~ "[ERR] - Non existing variable in data dictionary",
      .data$`table` == "data_dict" ~ "[INFO] - Non existing variable in the dataset")) %>%
    select(.data$`table`,.data$`name_var`, .data$`condition`) %>%
    mutate(across(everything(), ~as.character(.)))

  return(test)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param dataset xxx xxx xxx
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
#' @import dplyr
check_cat_values <- function(dataset, data_dict){

  dataset   <- data_dict_match_dataset(data = dataset, data_dict = data_dict,output = "data")
  data_dict <- data_dict_match_dataset(data = dataset, data_dict = data_dict,output = "data_dict")

  test <-
    data_dict$Variables %>%
    select(name_var = .data$`name`) %>%
    mutate(
      categorical = NA_character_,
      condition = "")

  data_dict_cat <-
    data_dict$Categories %>% select(name_var = .data$`variable`, name_cat = .data$`name`)

  for(i in test$name_var){
    if(data_dict_cat %>% filter(.data$`name_var` == i) %>% nrow > 0) test[test$name_var == i, "categorical"] <- "categorical"
  }

  for(i in test %>% filter(.data$`categorical` == "categorical") %>% pull(.data$`name_var`)){

    values_in_dataset   <- dataset %>% select(all_of(i)) %>% filter(if_any(i, ~ !is.na(.))) %>% unique %>% pull(i)
    values_in_data_dict <- data_dict_cat %>% filter(.data$`name_var` == i) %>% pull(.data$`name_cat`)

    val_dataset_in_data_dict <- values_in_dataset %in% values_in_data_dict %>% all
    val_data_dict_in_dataset <- values_in_data_dict %in% values_in_dataset %>% all

    if(val_dataset_in_data_dict != TRUE) test[test$name_var == i, "categorical"] <- "[INFO] - More unique values in the dataset than categories declared in the data dictionary"
    if(val_data_dict_in_dataset != TRUE) test[test$name_var == i, "condition"]   <- "[INFO] - More categories declared in the data dictionary than unique values in the dataset"

  }

  test <-
    test %>% filter(!is.na(.data$`categorical`)) %>%
    mutate(
      condition   = ifelse(.data$`categorical` != "categorical", paste(.data$`categorical`,.data$`condition`,sep="\n"), .data$`condition`),
      categorical = ifelse(.data$`categorical` != "categorical", "dual", .data$`categorical`),
      condition   = str_remove(.data$`condition`, "\n$"),
      condition   = na_if(.data$`condition`,"")
    )

  return(test)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param name_var xxx xxx xxx
#' @param dict_var xxx xxx xxx
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
#' @import dplyr
summary_variables <- function(name_var, dict_var){

  # i = "Species"
  # name_var = dataset %>% select(all_of(i))
  # dict_var = data_dict$Categories %>% filter(variable == i)

  name_variable <- name_var %>% names
  name_var <-
    name_var %>%
    select(value_var = 1) %>%
    count(.data$`value_var`) %>%
    mutate(n = .data$`n`/sum(.data$`n`)) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  dict_var <-
    dict_var %>%
    select(value_var = .data$`name`, category = .data$`missing`) %>%
    group_by(.data$`category`) %>%
    mutate(index = row_number()) %>%
    mutate(category = ifelse(.data$`category` == 1, "2_Missing values", "1_Valid values")) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  summary   <-
    name_var %>%
    full_join(dict_var, by = "value_var") %>%
    mutate(n = replace_na(.data$`n`, 0)) %>%
    mutate(
      category =
        case_when(
          is.na(.data$`value_var`)  ~ "4_NA values",
          TRUE ~ ifelse(is.na(.data$`category`),"3_Valid other values",.data$`category`) %>% as.character))

  if(summary %>% nrow > 0){

    summary <-
      tibble(
        `name_var`                                     = name_variable,
        `Nb. distinct values`                          = summary %>% filter(!.data$`category` %in% c("4_NA values") & n > 0) %>% nrow(),
        `% Valid values`                               = round(summary %>% filter(.data$`category` %in% c("1_Valid values","3_Valid other values")) %>% pull(.data$`n`) %>% sum,4),
        `% NA`                                         = round(summary %>% filter(.data$`category` == "4_NA values") %>% pull(.data$`n`) %>% sum,4),
        `% Missing categorical values (if applicable)` = round(summary %>% filter(.data$`category` == "2_Missing values") %>% pull(.data$`n`) %>% sum,4))

    if(summary$`% Missing categorical values (if applicable)` %>% sum == 0) summary <- summary %>% select(-.data$`% Missing categorical values (if applicable)`)

  }else{summary <- tibble(name_var = as.character())
  }

  return(summary)

}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param name_var xxx xxx xxx
#' @param dict_var xxx xxx xxx
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
#' @import dplyr
summary_variables_text <- function(name_var, dict_var){

  # i = "test 1"
  # name_var = dataset %>% select(all_of(i))
  # dict_var = data_dict$Categories %>% filter(variable == i)

  name_variable <- name_var %>% names
  name_var <-
    name_var %>%
    select(value_var = 1) %>%
    count(.data$`value_var`) %>%
    mutate(n = .data$`n`/sum(.data$`n`)) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  dict_var <-
    dict_var %>%
    select(value_var = .data$`name`, category = .data$`missing`) %>%
    group_by(.data$`category`) %>%
    mutate(index = row_number()) %>%
    mutate(category = ifelse(.data$`category` == 1, "2_Missing values", "1_Valid values")) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  summary   <-
    name_var %>%
    full_join(dict_var, by = "value_var") %>%
    mutate(n = replace_na(.data$`n`, 0)) %>%
    mutate(
      category =
        case_when(
          is.na(.data$`value_var`)  ~ "4_NA values",
          TRUE      ~ ifelse(is.na(.data$`category`),"3_Valid other values",.data$`category`) %>% as.character))

  if(summary %>% filter(.data$`category` %in% c("3_Valid other values")) %>% nrow > 0){

    summary <-
      tibble(
        `name_var` = name_variable)

  }else{summary <- tibble(name_var = as.character())}

  return(summary)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param name_var xxx xxx xxx
#' @param dict_var xxx xxx xxx
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
#' @import dplyr lubridate
summary_variables_date <- function(name_var, dict_var){

  # i = "dob"
  # name_var = dataset %>% select(all_of(i))
  # dict_var = data_dict$Categories %>% filter(variable == i)

  name_variable <- name_var %>% names
  name_var <-
    name_var %>%
    select(value_var = 1) %>%
    count(.data$`value_var`) %>%
    mutate(n = .data$`n`/sum(.data$`n`)) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  dict_var <-
    dict_var %>%
    select(value_var = .data$`name`, category = .data$`missing`) %>%
    group_by(.data$`category`) %>%
    mutate(index = row_number()) %>%
    mutate(category = ifelse(.data$`category` == 1, "2_Missing values", "1_Valid values")) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  summary   <-
    name_var %>%
    full_join(dict_var, by = "value_var") %>%
    mutate(n = replace_na(.data$`n`, 0)) %>%
    mutate(
      category =
        case_when(
          is.na(.data$`value_var`)  ~ "4_NA values",
          TRUE      ~ ifelse(is.na(.data$`category`),"3_Valid other values",.data$`category`) %>% as.character))

  if(summary %>% filter(.data$`category` %in% c("3_Valid other values")) %>% nrow > 0){
    summary <-
      tibble(
        `name_var`        = name_variable,
        `Lowest date`     = as_date(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% min(na.rm = TRUE)),
        `Highest date`    = as_date(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% max(na.rm = TRUE)),
        `MIN\n(year)`      = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[1]],
        `Q1\n(year)`       = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[2]] %>% round,
        `MEDIAN\n(year)`   = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[3]] %>% round,
        `Q3\n(year)`       = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[5]] %>% round,
        `MAX\n(year)`      = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[6]],
        `MEAN\n(year)`     = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer)[[4]] %>% round,
        `ST.DEV\n(years)`   = stats::sd(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`) %>% year %>% as.integer) %>% round,
        `Span\n(year)` = year(.data$`Highest date`) - year(.data$`Lowest date`)
      )
  }else{summary <- tibble(name_var = as.character())}

  return(summary)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param name_var xxx xxx xxx
#' @param dict_var xxx xxx xxx
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
#' @import dplyr
summary_variables_numerical <- function(name_var, dict_var){

  # i = "SMO QTY"
  # name_var = dataset %>% select(all_of(i))
  # dict_var = data_dict$Categories %>% filter(variable == i)

  name_variable <- name_var %>% names
  name_var <-
    name_var %>%
    select(value_var = 1) %>%
    mutate(n = 1) %>%
    mutate(across(.data$`value_var`, ~ as.numeric(.)))

  dict_var <-
    dict_var %>%
    select(value_var = .data$`name`, category = .data$`missing`) %>%
    group_by(.data$`category`) %>%
    mutate(index = row_number()) %>%
    mutate(category = ifelse(.data$`category` == 1, "2_Missing values", "1_Valid values")) %>%
    mutate(across(.data$`value_var`, ~ as.numeric(.)))

  summary   <-
    name_var %>%
    full_join(dict_var, by = "value_var") %>%
    mutate(n = replace_na(.data$`n`, 0)) %>%
    mutate(
      category =
        case_when(
          is.na(.data$`value_var`)  ~ "4_NA values",
          TRUE      ~ ifelse(is.na(.data$`category`),"3_Valid other values",.data$`category`) %>% as.character))

  if(summary %>% filter(.data$`category` %in% c("3_Valid other values")) %>% nrow > 0){
    summary <-
      tibble(
        `name_var`= name_variable,
        `MIN`     = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[1]],
        `Q1`      = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[2]],
        `MEDIAN`  = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[3]],
        `Q3`      = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[5]],
        `MAX`     = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[6]],
        `MEAN`    = summary(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))[[4]],
        `ST.DEV`  = stats::sd(summary %>% filter(.data$`category`  == "3_Valid other values") %>% pull(.data$`value_var`))
      )
  }else{summary <- tibble(name_var = as.character())}

  return(summary)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param name_var xxx xxx xxx
#' @param dict_var xxx xxx xxx
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
#' @import dplyr
summary_variables_categorical <- function(name_var, dict_var){

  # i = "SMO QTY"
  # name_var = dataset %>% select(all_of(i))
  # dict_var = data_dict$Categories %>% filter(variable == i)

  name_variable <- name_var %>% names
  name_var <-
    name_var %>%
    select(value_var = 1) %>%
    count(.data$`value_var`) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  dict_var <-
    dict_var %>%
    select(value_var = .data$`name`, category = .data$`missing`) %>%
    group_by(.data$`category`) %>%
    mutate(index = row_number()) %>%
    mutate(category = ifelse(.data$`category` == 1, "2_Missing values", "1_Valid values")) %>%
    mutate(across(.data$`value_var`, ~ as.character(.)))

  summary   <-
    name_var %>%
    full_join(dict_var, by = "value_var") %>%
    mutate(n = replace_na(.data$`n`, 0)) %>%
    mutate(
      category =
        case_when(
          is.na(.data$`value_var`)  ~ "4_NA values",
          TRUE      ~ ifelse(is.na(.data$`category`),"3_Valid other values",.data$`category`) %>% as.character))

  summary_category  <-
    summary %>%
    group_by(.data$`category`,.data$`index`) %>%
    summarise(n = sum(.data$`n`), name_var = paste0(.data$`value_var`, collapse = " ; "), .groups = "drop") %>%
    mutate(
      index = replace_na(.data$`index`,1),
      name_var            = str_replace(.data$`name_var`, "^NA$","")) %>%
    ungroup %>%
    mutate(n_perc = paste0(round(100*(.data$`n` / sum(.data$`n`)),2),"%")) %>%      # handle the round
    rowwise() %>%
    mutate(
      name_var = ifelse(
        .data$`category` == "3_Valid other values",
        unlist(.data$`name_var` %>% str_split(" ; "))[1:5] %>%
        paste0(collapse = " ; "),.data$`name_var`),
      name_var = ifelse(.data$`category` == "3_Valid other values" & n > 5, paste0(.data$`name_var`," [...]"), .data$`name_var`)) %>%
    ungroup %>%
    mutate(
      cat_var_absence    = ifelse(.data$`n` == 0, .data$`name_var`, ""),
      other_val_presence = ifelse(.data$`category` == "3_Valid other values", .data$`name_var`, ""),
      list_values        = ifelse(.data$`category` == "3_Valid other values", "", .data$`name_var`),
      n_perc             = paste0(" : ", .data$`n_perc`)) %>%
    unite("list_values",.data$`list_values`,.data$`n_perc`, sep = "",remove = TRUE, na.rm = TRUE) %>%
    mutate(cat_index = str_sub(.data$`category`,1,1)) %>%
    group_by(.data$`category`,.data$`cat_index`) %>%
    mutate(
      category = case_when(
        .data$`category` == "1_Valid values"       ~ "Valid categorical values : \n",
        .data$`category` == "2_Missing values"     ~ "\nMissing categorical values : \n",
        .data$`category` == "3_Valid other values" ~ "\nOther values (non categorical)",
        .data$`category` == "4_NA values"          ~ "\nNA values",
        TRUE                             ~ .data$`category`)) %>%
    select(-.data$`name_var`) %>%
    mutate(across(c(.data$`list_values`,.data$`cat_var_absence`,.data$`other_val_presence`), ~ ifelse(.data$`cat_index` == 4 ,.,paste0(.,"\n")))) %>%
    mutate(category = ifelse(.data$`index` == 1 ,.data$`category`,"")) %>%
    mutate(category_space_prefix = ifelse(.data$`index` == 1 & .data$`cat_index` %in% c(2,3,4),"\n","")) %>%
    mutate(category_space_suffix = ifelse(.data$`index` == 1 & .data$`cat_index` %in% c(1,2),  "\n","")) %>%
    unite("list_values",.data$`category`,.data$`list_values`, sep = "",remove = TRUE, na.rm = TRUE) %>%
    unite("cat_var_absence",.data$`category_space_prefix`,.data$`cat_var_absence`,.data$`category_space_suffix`, sep = "",remove = FALSE, na.rm = TRUE) %>%
    unite("other_val_presence",.data$`category_space_prefix`,.data$`other_val_presence`,.data$`category_space_suffix`, sep = "",remove = TRUE, na.rm = TRUE) %>%
    mutate(
      cat_var_absence    = ifelse(.data$`cat_var_absence` %>% str_squish() == "","",.data$`cat_var_absence`),
      other_val_presence    = ifelse(.data$`other_val_presence` %>% str_squish() == "","",.data$`other_val_presence`)) %>%
    select(-.data$`index`, -.data$`n`) %>%
    ungroup() %>%
    summarise(across(everything(), ~ paste0(.,collapse = "")))

  if(summary %>% filter(.data$`category` %in% c("1_Valid values","2_Missing values")) %>% nrow > 0){

    summary <-
      tibble(
        `name_var`                   = name_variable,
        `% Valid categorical values` = round(summary %>% filter(.data$`category` == "1_Valid values") %>% pull(.data$`n`) %>% sum / (summary %>% pull(.data$`n`) %>% sum),4),
        `Values present in dataset`                           = summary_category$list_values,
        `Data dictionnary categories not present in dataset`  = summary_category$cat_var_absence,
        `Dataset value not present in data dictionnary`       = summary_category$other_val_presence

      )

  }else{summary <- tibble(name_var = as.character())}

  return(summary)
}


