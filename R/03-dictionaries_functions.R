#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param from xxx xxx xxx
#' @param to xxx xxx xxx
#' @param name_prefix xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#'
#'
data_dict_expand <- function(data_dict,from = 'Variables',name_prefix = 'Categories::',to = 'Categories'){

  # test
  data_dict <- as_data_dict_shape(data_dict)

  from <- substitute(from)
  if(typeof(from) == "character") from <- as.symbol(from)
  if(typeof(from) == "symbol")    from <- substitute(from)
  if(typeof(from) == "language")  from <- as.symbol(from)

  to <- substitute(to)
  if(typeof(to) == "character") to <- as.symbol(to)
  if(typeof(to) == "symbol")    to <- substitute(to)
  if(typeof(to) == "language")  to <- as.symbol(to)

  if(is.null(data_dict[[from]])){
    stop("Your data dictionary contains no '",from,"' component.")}


  names_col <- data_dict[[from]] %>% select(starts_with(name_prefix)) %>% names

  if(length(names_col) == 0){
    warning("Your data dictionary contains no column starting with '",name_prefix,"' in ",from)
    return(data_dict)}

  rename_col <- make.unique(make.names(names_col %>% str_remove(name_prefix)))

  data_dict[[to]] <-
    tibble(name = as.character(),variable = as.character()) %>%
    bind_rows(data_dict[[to]])

  for(i in names_col){
    # stop()}

    tryCatch(
      {to_temp <-
        data_dict[[from]] %>%
        select(variable = .data$`name`, col_to = !! i ) %>%
        filter(!is.na(.data$`col_to`)) %>%
        mutate(
          col_to = ifelse(str_detect(.data$`col_to`, "_="),
                          str_replace_all(.data$`col_to`, "_=", "__SEP_IN__"), .data$`col_to`),
          col_to = ifelse(str_detect(.data$`col_to`, "_;"),
                          str_replace_all(.data$`col_to`, "_;", "__SEP_OUT__"),.data$`col_to`),
          col_to = ifelse(str_detect(.data$`col_to`, "__SEP_IN__"),       .data$`col_to`,
                          str_replace_all(.data$`col_to`, "=", "__SEP_IN__")),
          col_to = ifelse(str_detect(.data$`col_to`, "__SEP_OUT__"),      .data$`col_to`,
                          str_replace_all(.data$`col_to`, ";", "__SEP_OUT__"))) %>%
        separate_rows(.data$`col_to`, sep="__SEP_OUT__") %>%
        separate(.data$`col_to`, into = c("name",i), sep = "__SEP_IN__") %>%
        mutate_all(~ stringr::str_squish(.)) %>%
        rename_with(.cols = !! i ,.fn =  ~ rename_col[which(names_col == i)])

      data_dict[[to]] <-
        data_dict[[to]] %>%
        full_join(to_temp,by = c("name","variable"))}
      ,
      warning=function(w) {
        # Choose a return value in case of warning
        error_vars <-
          fabR::silently_run(
            data_dict[[from]] %>%
              select(variable = .data$`name`, col_to = !! i ) %>%
              filter(!is.na(.data$`col_to`)) %>%
              mutate(
                col_to = ifelse(str_detect(.data$`col_to`, "_="),
                                str_replace_all(.data$`col_to`, "_=", "__SEP_IN__"), .data$`col_to`),
                col_to = ifelse(str_detect(.data$`col_to`, "_;"),
                                str_replace_all(.data$`col_to`, "_;", "__SEP_OUT__"), .data$`col_to`),
                col_to = ifelse(str_detect(.data$`col_to`, "__SEP_IN__"), .data$`col_to`,
                                str_replace_all(.data$`col_to`, "=", "__SEP_IN__")),
                col_to = ifelse(str_detect(.data$`col_to`, "__SEP_OUT__"), .data$`col_to`,
                                str_replace_all(.data$`col_to`, ";", "__SEP_OUT__"))) %>%
              separate_rows(.data$`col_to`, sep="__SEP_OUT__") %>%
              separate(.data$`col_to`, into = c("name", i), sep = "__SEP_IN__") %>%
              filter(across(!! i, ~is.na(.))) %>% pull(.data$`variable`) %>% toString)

        stop(
          "\n\nParsing elements failures in your data dictionnary.",
          "\nVariables affected:\n",
          error_vars,"\n",
          "Column affected:  ",i,"\n",
          crayon::bold("\n\nUseful tip:")," If your colums contains ',' or '=' in
its labels, replace the separators by '_;' and '_=' and reprocess.
Example:
  > wrong: '0 = No alcohol  ; 1 = Alcohol(red ; white)'
  > good : '0 = No alcohol ", crayon::bold("_;") ," 1 = Alcohol(red ; white)'\n")
      })

  }

  data_dict[[from]] <- data_dict[[from]] %>% select(- !! names_col)

  data_dict <- as_data_dict_shape(data_dict)
  # data_dict <- as_data_dict(data_dict)
  # if(apply_mlstr_standards == TRUE) data_dict <- as_mlstr_data_dict(data_dict)

  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param from xxx xxx xxx
#' @param to xxx xxx xxx
#' @param name_prefix xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_flatten <- function(data_dict, from = 'Categories', to = 'Variables', name_prefix = 'Categories::'){

  # test
  data_dict <- as_data_dict_shape(data_dict)

  from <- substitute(from)
  if(typeof(from) == "character") from <- as.symbol(from)
  if(typeof(from) == "symbol")    from <- substitute(from)
  if(typeof(from) == "language")  from <- as.symbol(from)

  to <- substitute(to)
  if(typeof(to) == "character") to <- as.symbol(to)
  if(typeof(to) == "symbol")    to <- substitute(to)
  if(typeof(to) == "language")  to <- as.symbol(to)

  if(is.null(data_dict[[from]])){
    stop("Your data dictionary contains no '",from,"' component.")}

  if(is.null(data_dict[[to]])){
    stop("Your data dictionary contains no '",to,"' component.")}

  # add categories content
  if(sum(nrow(data_dict[[from]])) > 0){

    col_from <- tibble(name = as.character())

    for(i in names(data_dict[[from]] %>% select(-.data$`name`,-.data$`variable`))){
      # stop()}

      cat_temp <-
        data_dict[[from]] %>%
        select(.data$`variable`,.data$`name`,!! i) %>%
        unite("from", .data$`name`, !! i, sep = " __SEP_IN__ ") %>%
        group_by(.data$`variable`) %>%
        summarise(from = paste0(.data$`from`,collapse = " __SEP_OUT__ \n"), .groups = "drop") %>%
        mutate(
          from = ifelse(str_detect(.data$`from`, ";"),
                        str_replace_all(.data$`from`, "__SEP_OUT__", "_;"),.data$`from`),
          from = ifelse(str_detect(.data$`from`, "="),
                        str_replace_all(.data$`from`, "__SEP_IN__", "_="), .data$`from`),
          from =   str_replace_all(.data$`from`, "__SEP_OUT__", ";"),
          from =   str_replace_all(.data$`from`, "__SEP_IN__", "="))

      names(cat_temp) <- c("name",paste0(name_prefix,i))

      col_from <- full_join(col_from,cat_temp, by = "name")

    }

    data_dict[[to]] <-
      data_dict[[to]] %>%
      full_join(col_from, by = c("name"))

    data_dict[[from]] <- NULL
  }

  data_dict <- as_data_dict_shape(data_dict)

  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param taxonomy xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_pivot_wider <- function(data_dict, taxonomy = NULL){

  # test
  data_dict <- as_data_dict(data_dict)

  if(is.null(taxonomy)) return(data_dict)

  taxonomy <- as_taxonomy(taxonomy)
  taxonomy_opal <-
    taxonomy  %>%
    unite("taxonomy_opal", .data$`taxonomy`, .data$`vocabulary`, na.rm = TRUE, sep = "::", remove = FALSE) %>%
    arrange(.data$`index_taxonomy`, .data$`index_vocabulary`, .data$`index_term`) %>%
    select(.data$`index_vocabulary`, .data$`taxonomy_opal`,.data$`taxonomy`,.data$`vocabulary`) %>%
    distinct() %>%
    mutate(
      name_col = str_replace(
        .data$`taxonomy_opal`,
        .data$`vocabulary`,
        as.character(.data$`index_vocabulary`)),
      name_term = paste0(.data$`name_col`,".term")) %>%
    select(.data$`name_col`,.data$`name_term`,.data$`taxonomy`)

  taxonomy_opal <-
    taxonomy_opal %>%
    filter(.data$`name_col` %in% names(data_dict[["Variables"]]) &
             .data$`name_term` %in% names(data_dict[["Variables"]]))

  if(taxonomy_opal %>% nrow > 0){

    for(i in 1:nrow(taxonomy_opal)){
      # stop()}

      # i = 3
      name_col   <- taxonomy_opal$`name_col`[i]
      name_term  <- taxonomy_opal$`name_term`[i]
      col_final  <- taxonomy_opal$`taxonomy`[i]

      data_dict_temp <-
        data_dict[['Variables']] %>%
        select(.data$`name`, any_of(name_col), any_of(name_term)) %>%
        pivot_wider(
          names_from = name_col,
          values_from = name_term,
          names_prefix = paste0("__temp__.",col_final,"::")) %>%
        janitor::remove_empty("cols")

      col_temp <- names(data_dict_temp)[-1]
      col_final <- str_remove(col_temp,"^__temp__\\.")

      for(j in 1:length(col_temp)){
        # stop()}

        col_temp_j  <- col_temp[j]
        col_final_j <- col_final[j]

        if(col_temp_j %in% names(data_dict[['Variables']])){
          stop("Column name ",col_temp_j, " already exists in your data dictionary")}

        data_dict[['Variables']] <-
          data_dict[['Variables']] %>%
          full_join(
            data_dict_temp %>% select(.data$`name`, all_of(col_temp_j)) ,
            by = c("name"))

        if((data_dict[['Variables']] %>% names) %in% col_final_j %>% sum == 1){

          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            unite(!! col_final_j,
                  !! col_final_j,
                  !! col_temp_j, sep = "|", na.rm = TRUE) %>%
            mutate(across(!! col_final_j, ~na_if(.,"")))

        }else{
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            rename_with(.cols = col_temp_j, .fn = ~ col_final_j)
        }
      }

      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        select(-all_of(name_col),-all_of(name_term))

    }

    if(paste0(attributes(taxonomy)$`Mlstr::class`,"") == "mlstr_taxonomy"){

      keys <-
        taxonomy %>%
        select(.data$`vocabulary`, .data$`vocabulary_short`) %>%
        filter(!is.na(.data$`vocabulary_short`)) %>% distinct %>%
        mutate(vocabulary_short = paste0("Mlstr_area::",.data$`vocabulary_short`)) %>%
        mutate(vocabulary = paste0("Mlstr_area::",.data$`vocabulary`))

      col_area <-
        data_dict[['Variables']] %>%
        select(any_of(keys$`vocabulary_short`)) %>% names

      for(i in col_area){

        data_dict[['Variables']] <-
          data_dict[['Variables']] %>%
          rename_with(
            .cols = any_of(i),
            .fn =  ~ keys %>% filter(.data$`vocabulary_short` == i) %>% pull(.data$`vocabulary`))
      }

      taxo_scales <-
        taxonomy %>%
        unite("area_scale_opal", .data$`taxonomy_scale`, .data$`vocabulary_scale`, na.rm = TRUE, sep = "::", remove = FALSE) %>%
        select(.data$`area_scale_opal`,.data$`term_scale`) %>%
        filter(!is.na(.data$`area_scale_opal`)) %>% distinct %>%
        rename("___area_scale_opal___" = .data$`area_scale_opal`) %>%
        rename("Mlstr_area::1.scale" = .data$`term_scale`)

      if(!is.null(data_dict[['Variables']][['Mlstr_area::1.scale']])){

        if(!is.null(data_dict[['Variables']][['___area_scale_opal___']])){
          stop("Column name '___area_scale_opal___' already exists in your data dictionary")}

        fabR::silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            left_join(

              data_dict[['Variables']] %>%
                select(.data$`name`,.data$`Mlstr_area::1.scale`) %>%
                filter(!is.na(.data$`Mlstr_area::1.scale`)) %>%
                left_join(taxo_scales, by = "Mlstr_area::1.scale") %>%
                pivot_wider(
                  names_from = .data$`___area_scale_opal___`,
                  values_from = .data$`Mlstr_area::1.scale`) ,

              by = c("name") ) %>%

            select(-.data$`Mlstr_area::1.scale`)
        })

        if(!is.null(data_dict[['Variables']][['NA']])){
          warning(data_dict[['Variables']][['NA']] %>% stats::na.omit() %>% unique %>% toString,
                  " scale(s) not in your taxonomy but present in your data dictionary")}
      }
    }
  }

  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param taxonomy xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_pivot_longer <- function(data_dict, taxonomy = NULL){

  # test
  data_dict <- as_data_dict(data_dict)

  if(is.null(taxonomy)) { return(data_dict) }

  taxonomy <- as_taxonomy(taxonomy)

  order_taxonomy <-
    taxonomy %>%
    select(.data$`taxonomy`,.data$`index_taxonomy`) %>%
    distinct() %>% pull(.data$`taxonomy`)

  taxonomy_opal <-
    taxonomy  %>%
    unite("taxonomy_opal", .data$`taxonomy`, .data$`vocabulary`, na.rm = TRUE, sep = "::", remove = FALSE) %>%
    arrange(.data$`index_taxonomy`, .data$`index_vocabulary`, .data$`index_term`) %>%
    group_by(.data$`taxonomy`) %>%
    group_split() %>%
    stats::setNames(taxonomy %>% select(.data$`taxonomy`) %>%
                      distinct() %>% arrange(.data$`taxonomy`) %>% pull(.data$`taxonomy`)) %>%
    as.list()

  taxonomy_opal <- taxonomy_opal[order_taxonomy]

  for(i in names(taxonomy_opal)){

    taxonomy_i <-
      taxonomy_opal[[i]] %>%
      filter(
        .data$`taxonomy_opal` %in%
          (data_dict[['Variables']] %>% names)) %>%
      select(.data$`index_vocabulary`, .data$`vocabulary`, .data$`index_term`, .data$`term`) %>%
      distinct

    if(taxonomy_i %>% nrow > 0){

      try({
        data_dict_temp <-
          data_dict[['Variables']] %>%
          select(.data$`name`, matches(paste0("^",i,"::",taxonomy_i$`vocabulary`,"$"))) %>%
          # pivoting area of information
          pivot_longer(
            cols = starts_with(i),
            names_to = i,
            names_prefix = paste0(i,"::"),
            values_to = "term",
            values_drop_na = TRUE) %>%
          left_join(taxonomy_i, by = "term") %>%
          arrange(.data$`index_vocabulary`, .data$`index_term`) %>%
          select(-.data$`index_vocabulary`, -.data$`index_term`,-.data$`vocabulary`) %>%
          group_by(.data$`name`) %>%
          distinct()
      }, silent = TRUE)

      group_max_size <- data_dict_temp %>% group_size() %>% max()
      arrange_taxonomy <-
        paste0(i,"::",rep(1:group_max_size,2) %>% sort(),c("",".term"))

      try({

        fabR::silently_run({
          data_dict_temp <-
            data_dict_temp %>%
            summarise(
              across(c(any_of(i), .data$`term`),
                     ~ paste0(.,collapse = "|")),
              .groups = "drop") %>%
            separate(col = i,
                     into = arrange_taxonomy[1:length(arrange_taxonomy) %% 2 == 1],
                     sep = "\\|") %>%
            separate(col = .data$`term`,
                     into = arrange_taxonomy[1:length(arrange_taxonomy) %% 2 == 0],
                     sep = "\\|") %>%
            ungroup() %>%
            select(everything(), -any_of(arrange_taxonomy), any_of(arrange_taxonomy))
        })

        fabR::silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>%
            select(-matches(paste0("^",i,"::",taxonomy_i$`vocabulary`,"$"))) %>%
            full_join(data_dict_temp, by = c("name"))
        })

      }, silent = TRUE)}
  }


  if(paste0(attributes(taxonomy)$`Mlstr::class`,"") == "mlstr_taxonomy"){

    keys <-
      taxonomy %>%
      select(.data$`vocabulary`, .data$`vocabulary_short`) %>%
      filter(!is.na(.data$`vocabulary_short`)) %>% distinct

    col_area <-
      data_dict[['Variables']] %>%
      select(matches("Mlstr_area::[0-9]+$")) %>% names

    for(i in col_area){

      key <-
        keys %>%
        rename_with(.cols = .data$`vocabulary`,       .fn =  ~ paste0(i)) %>%
        rename_with(.cols = .data$`vocabulary_short`, .fn =  ~ paste0(i,".vocabulary_short"))

      # re-arrange things (can do better)
      if(is.null(data_dict[['Variables']][['___Mlstr_temp___']]) |
         is.null(data_dict[['Variables']][['___Mlstr_temp___vocabulary']])){

        fabR::silently_run({
          data_dict[['Variables']] <-
            data_dict[['Variables']] %>% left_join(key) %>%
            rename_with(.cols = all_of(i),       .fn =  ~ "___Mlstr_temp___") %>%
            rename_with(.cols = paste0(i,".vocabulary_short"),       .fn =  ~ "___Mlstr_temp___vocabulary") %>%
            mutate(`___Mlstr_temp___` = .data$`___Mlstr_temp___vocabulary`) %>%
            rename_with(.cols = .data$`___Mlstr_temp___`, .fn =  ~ i) %>%
            select(-.data$`___Mlstr_temp___vocabulary`)
        })

      }else{
        stop("Your data dictionary cannot be processed into Maelstrom format (presence of `___Mlstr_temp___` column")
      }
    }

    cols_scales <-
      taxonomy %>%
      unite("area_scale_opal", .data$`taxonomy_scale`, .data$`vocabulary_scale`, na.rm = TRUE, sep = "::", remove = FALSE) %>%      select(.data$`area_scale_opal`,.data$`term_scale`) %>%
      select(.data$`area_scale_opal`) %>%
      filter(!is.na(.data$`area_scale_opal`)) %>% distinct %>%
      pull(.data$`area_scale_opal`) %>%
      intersect(names(data_dict[['Variables']]))

    if(length(cols_scales) > 0){
      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        unite("Mlstr_area::1.scale" , all_of(cols_scales),sep = " | ",na.rm = TRUE) %>%
        mutate(`Mlstr_area::1.scale` = na_if(.data$`Mlstr_area::1.scale`,""))}

    arrange_taxonomy =
      paste0("Mlstr_area","::",rep(1:length(col_area),2) %>% sort(),c("",".term"))

    # re-arrange things (can do better)
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      select(
        everything(),
        -starts_with("Mlstr_additional"),
        -starts_with("Mlstr_area"),
        starts_with("Mlstr_additional"),
        matches("^Mlstr_area::1$"),
        matches("^Mlstr_area::1.term$"),
        matches("^Mlstr_area::1.scale$"),
        matches("^Mlstr_area::2$"),
        matches("^Mlstr_area::2.term$"),
        matches("^Mlstr_area::3$"),
        matches("^Mlstr_area::3.term$"),
        everything()) %>%
      rename_with(.cols = any_of("Mlstr_additional::1.term"), .fn = ~ "Mlstr_additional::Source") %>%
      rename_with(.cols = any_of("Mlstr_additional::2.term"), .fn = ~ "Mlstr_additional::Target") %>%
      select(-matches("^Mlstr_additional::1$"),-matches("^Mlstr_additional::2$"))

  }

  return(data_dict)
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
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_opalr_fix <- function(data_dict){

  # test if actually an opalr data dictionary
  if(data_dict %>% names %in% c("variables", "table", "project") %>% sum != 3){
    stop("Your file is not in the opalr format. Please provide another file")}

  data_dict[['Variables']]  <-
    data_dict[['variables']] %>% as_tibble() %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    mutate(across(everything(), ~ na_if(.,"")))

  if(sum(nrow(data_dict[['categories']])) > 0){
    data_dict[['Categories']] <-
      data_dict[['categories']] %>% as_tibble() %>%
      mutate(across(everything(), ~ as.character(.))) %>%
      mutate(across(everything(), ~ na_if(.,"")))
  }

  data_dict[['variables']]  <- NULL
  data_dict[['categories']] <- NULL

  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param filter_var xxx xxx xxx
#' @param filter_cat xxx xxx xxx
#' @param filter_all xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_filter <- function(data_dict, filter_var = NULL, filter_cat = NULL, filter_all = NULL){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  if(!is.null(filter_all) & (!is.null(filter_var) | !is.null(filter_cat))) stop("Too many argments entered")
  if( is.null(filter_all) &   is.null(filter_var) &  is.null(filter_cat))  return(data_dict)

  if(!is.null(filter_all)) filter_var <- filter_cat <- filter_all

  data_dict[['Variables']] <-
    eval(parse(text = paste("data_dict[['Variables']] %>% filter(",filter_var,")")))

  if(!is.null(data_dict[['Categories']])){
    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      filter(.data$`variable` %in% data_dict[['Variables']]$`name`)

    if(!is.null(filter_cat)){
      data_dict[['Categories']] <-
        eval(parse(text = paste("data_dict[['Categories']] %>% filter(",filter_cat,")")))}
  }

  if(sum(nrow(data_dict[['Categories']])) == 0) data_dict[['Categories']] <- NULL

  return(data_dict)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param ... xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_group_split <- function(data_dict, ...){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  if(!is_grouped_df(data_dict[['Variables']])) data_dict <- data_dict_group_by(data_dict, ...)

  if(!is_grouped_df(data_dict[['Variables']])) stop("
\n\nThe data dictionary list must be grouped to be splited. Please group them using
data_dict_group_by(data_dict, col)")

  col <- as.symbol(names(group_keys(data_dict[['Variables']])))

  group_names_var <- group_keys(data_dict[['Variables']]) %>% pull

  if(sum(nrow(data_dict[['Categories']])) == 0){
    data_dict[['Categories']] =
      tibble(col = as.character()) %>%
      rename_with(.cols = "col", ~ deparse(col)) %>%
      group_by(!! col)
  }

  if(names(group_keys(data_dict[['Variables']]))[1] != names(group_keys(data_dict[['Categories']]))[1]){
    stop(paste0("Grouping column must be the same in 'Variables' and 'Categories'."))}

  group_names_cat <- group_keys(data_dict[['Categories']]) %>% pull

  if(!all(group_names_cat %in% group_names_var)) stop("
\nThese data dictionaries contain group of variables in 'Categories' which
cannot be found accross the variables declared in 'Variables'.")

  # if(length(group_names_var) == 1) return(data_dict)

  data_dicts_var <-
    data_dict[['Variables']] %>%
    group_split() %>% as.list() %>%
    stats::setNames(group_names_var)

  data_dicts_cat <-
    data_dict[['Categories']] %>%
    group_split() %>% as.list() %>%
    stats::setNames(group_names_cat)

  if(length(data_dicts_cat) == 0) data_dicts_cat <- NULL

  data_dict_list <-
    vector(mode = "list", length = length(group_names_var)) %>%
    stats::setNames(group_names_var)

  for(i in group_names_var){
    # stop()}

    data_dict_list[[i]] <- list(Variables  = NULL, Categories = NULL)
    data_dict_list[[i]] <-
      list(
        Variables  =
          bind_rows(data_dict_list[[i]][['Variables']], data_dicts_var[[i]]),
        Categories =
          bind_rows(data_dict_list[[i]][['Categories']], data_dicts_cat[[i]]))

    if(sum(nrow(data_dict_list[[i]][['Categories']])) == 0){
      data_dict_list[[i]][['Categories']] <- NULL }
  }
  return(data_dict_list)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict_list xxx xxx xxx
#' @param name_group xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_list_nest <- function(data_dict_list, name_group = NULL){

  # test if enough data_dict
  data_dict_list %>%
    lapply(
      FUN = function(x) as_data_dict_shape(x))

  data_dict = list(Variables = tibble(), Categories = tibble())

  for(i in 1:length(data_dict_list)){
    # stop()}

    data_dict[['Variables']] <- bind_rows(data_dict[['Variables']],data_dict_list[[i]][['Variables']])
    data_dict[['Categories']] <- bind_rows(data_dict[['Categories']],data_dict_list[[i]][['Categories']])
  }

  if(!is.null(name_group)){
    if(name_group %in% c(names(data_dict[['Variables']]),names(data_dict[['Categories']]))){
      warning("The column '",name_group,"' already exists in data dictionary and will not be added.")
    }else{

      name_group_col_var <- tibble()
      name_group_col_cat <- tibble()

      for(i in names(data_dict_list)){
        # stop()}

        name_group_col_var <-
          bind_rows(
            name_group_col_var,
            data_dict_list[[i]][['Variables']][1] %>%
              mutate(name_list_group = names(data_dict_list[i])) %>%
              select(.data$`name_list_group`) %>%
              rename_with(.cols = .data$`name_list_group`,~ name_group))

        if(!is.null(data_dict_list[[i]][['Categories']][1])){
          name_group_col_cat <-
            bind_rows(
              name_group_col_cat,
              data_dict_list[[i]][['Categories']][1] %>%
                mutate(name_list_group = names(data_dict_list[i])) %>%
                select(.data$`name_list_group`) %>%
                rename_with(.cols = .data$`name_list_group`,~ name_group))
        }

      }

      data_dict[['Variables']]  <- name_group_col_var %>% bind_cols(data_dict[['Variables']])
      data_dict[['Categories']] <- name_group_col_cat %>% bind_cols(data_dict[['Categories']])
    }
  }

  return(data_dict)
}


#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data_dict xxx xxx xxx
#' @param col xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_group_by <- function(data_dict, col){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  col <- substitute(col)
  if(typeof(col) == "character") col <- as.symbol(col)
  if(typeof(col) == "symbol")    col <- substitute(col)
  if(typeof(col) == "language")  col <- as.symbol(col)

  if(is.null(col)) return(data_dict)

  group_names_var <- c(sort(unique(data_dict[['Variables']][[col]]),na.last = TRUE))

  categories <- TRUE
  if(is.null(data_dict[['Categories']])) categories <- FALSE

  if(sum(nrow(data_dict[['Categories']])) == 0){
    data_dict[['Categories']] =
      tibble(col = as.character()) %>%
      rename_with(.cols = "col", ~ deparse(col))}

  group_names_cat <- c(sort(unique(data_dict[['Categories']][[col]]),na.last = TRUE))

  if(is.null(group_names_cat)) stop(paste0("Column '",col,"' not found in Categories."))

  if(!all(group_names_cat %in% group_names_var)) stop("
\nThese data dictionaries contain group of variables in 'Categories' which
cannot be found accross the variables declared in 'Variables'.")

  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    arrange(!! col) %>%
    group_by(!! col)

  data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    arrange(!! col) %>%
    group_by(!! col)

  if(categories == FALSE) data_dict[['Categories']] = NULL

  return(data_dict)
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
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_ungroup <- function(data_dict){

  # test if enough data_dict
  as_data_dict_shape(data_dict)

  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    ungroup()

  if(!is.null(data_dict[['Categories']]))
    data_dict[['Categories']] <-
    data_dict[['Categories']] %>%
    ungroup()

  return(data_dict)
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
data_dict_apply <- function(data, data_dict = NULL){

  # if data_dict empty
  if(is.null(data_dict)) data_dict <- data_dict_extract(data)

  # test
  data <- as_dataset(data)
  if(toString(attributes(data_dict)$`Mlstr::class`) == 'Mlstr_data_dict'){
    data_dict <- as_mlstr_data_dict(data_dict,as_data_dict = TRUE)
  }else{data_dict <- as_data_dict(data_dict)}

  # names must exist both in dataset and data dictionary
  # data dictionary is not applied to dataset, since it may come from an
  # automated datadict (text by default).
  if(suppressWarnings(check_dataset_variables(data, data_dict)) %>% nrow > 0){
    stop("Names across your data dictionary differ from names across the dataset.",
         crayon::bold("\n\nUseful tip:")," Use check_dataset_variables(data, data_dict) to get non existing variables")}

  # set cleaning prefix of Variables component (addition of Variables:: before all variables except name)
  names(data_dict[['Variables']])  <- make.unique(stringr::str_remove(names(data_dict[['Variables']]),"^Variables::"))
  names(data_dict[['Variables']])[-which(names(data_dict[['Variables']])=='name')] <-
    paste0("Variables::",
           names(data_dict[['Variables']]))[-which(names(data_dict[['Variables']])=='name')]


  # set cleaning prefix of Categories component (addition of Categories:: before all variables except variable, name, labels and na_values)
  if(!is.null(data_dict[['Categories']])){
    names(data_dict[['Categories']]) <- make.unique(stringr::str_remove(names(data_dict[['Categories']]),"^Categories::"))
    names(data_dict[['Categories']])[-which(names(data_dict[['Categories']]) %in% c('variable','name','labels', 'na_values'))] <-
      paste0("Categories::",
             names(data_dict[['Categories']])[-which(names(data_dict[['Categories']]) %in% c('variable','name','labels','na_values'))])}

  names_data <- names(data)
  names_data_dict <- data_dict[['Variables']]$`name`

  for (i in names_data) {
    # stop()}

    vT_list <- tinyPackage::valueType_list
    vT <- valueType_of(x = data[[i]])
    data[[i]] <- as_valueType(x = as.character(data[[i]]),valueType = vT)

    attrs_init <- attributes(data[[i]])
    attrs_var <-
      c(data_dict[['Variables']][which(data_dict[['Variables']]$`name` == i),] %>%
          janitor::remove_empty("cols") %>%
          unlist %>% as.list())
    attrs_var <- attrs_var[names(attrs_var) != 'name']

    attrs_cat <- list()
    attrs_fct <- list()
    attrs_na <- list(na_values = c())

    if (!is.null(data_dict[['Categories']])) {
      cat_i <-
        data_dict[['Categories']][which(data_dict[['Categories']]$`variable` == i),] %>%
        janitor::remove_empty("cols")

      if(is.null(cat_i[['na_values']])) cat_i[['na_values']] <- NA

      if(nrow(cat_i) > 0) {
        # create vector of data
        attributes(data[[i]])$`class` <- NULL
        vec_data <- as_valueType(x = cat_i$`name`,valueType = vT)

        names(vec_data) <- cat_i$`labels`
        attrs_na <- list(na_values = vec_data[which(!is.na(cat_i$`na_values`))])
        cat_i$`name`        <- NULL
        cat_i$`labels`      <- NULL
        cat_i$`variable`  <- NULL
        cat_i$`na_values` <- NULL

        if(ncol(cat_i) > 0) {
          for(j in 1:length(cat_i)){
            # stop()}
            vec_attr <- vec_data
            names(vec_attr) <-  cat_i[[j]]
            vec_attr <- vec_attr[which(!is.na(cat_i[[j]]))]
            attrs_cat[[names(cat_i[j])]] <- vec_attr
          }}

        # labelled::val_labels(data[[i]]) <- vec_data
        attributes(data[[i]])$`labels` <- vec_data
        attributes(data[[i]])$`class` <-
          c("haven_labelled","vctrs_vctr", vT_list[[which(vT_list$`valueType` == vT),"class"]])
        attrs_fct <- attributes(data[[i]])

      }}

    attrs_total <- c(attrs_fct,attrs_na, attrs_init, attrs_var,attrs_cat)
    attributes(data[[i]]) <- attrs_total

    # suppression of na_values if empty
    if(length(attrs_na[[1]]) == 0) attributes(data[[i]])$`na_values` <- NULL

  }

  data <-
    data[names_data_dict] %>%
    as_tibble() %>%
    as_dataset()

  return(data)
}

#' Create a data dictionary in the Maelstrom Research format from a dataset
#'
#' Creates a data dictionary in the Maelstrom Research formats (with "Variables" and "Categories"
#' in separate tibbles and standard columns in each) from any dataset in tibble format.
#' If the input dataset has no associated metadata, a data dictionary with a minimal
#' of information is created from the column (variable) names to create the data
#' dictionary structure required for harmonizR (all columns except variable names
#' will be blank).
#'
#' Must provide an input data dictionary in tibble format, with or without metadata
#' in addition to column names.
#'
#' @param data xxx xxx xxx.
#'
#' @return A list of two tibbles: Variables and Categories. Variables has all the
#' variable names extracted from the dataset column names as rows and the Maelstrom
#' Research descriptors standards of variables as columns. Categories has all the category
#' names extracted from the “categories” argument as rows and the Maelstrom Research
#' descriptors standard of categories as columns.
#'
#' @examples
#' \dontrun{
#' # use case 1: create a data dictionary from any dataset
#' data_dict_extract(iris)
#'
#' # use case 2: create a data dictionary with project and categorical variables
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
data_dict_extract <- function(data){

  # test
  as_dataset(data)

  data_dict <-
    list(
      Variables = tibble(name = as.character()),
      Categories = tibble(variable = as.character(), name = as.character()))

  for(i in names(data)){
    # stop()}

    attrs_i <- attributes(data[[i]])
    attrs_i$`class` <- NULL
    if(is.factor(data[[i]])){
      names(attrs_i$`levels`) <- make.unique(attrs_i$`levels`)
    }

    data_dict_var <- tibble(name = i)
    data_dict_cat <- tibble(variable = as.character())

    if(length(attrs_i) > 0){

      # if(length(attrs_i) > 0){
      for(j in 1:length(attrs_i)){
        # stop()}

        attr_col_name     <- attrs_i[j] %>% names
        attr_content_col  <- attrs_i[[j]] %>% names
        attr_name_cat     <- attrs_i[[j]] %>% as.character()

        if(is.null(attr_content_col)) {
          data_dict_var[attr_col_name] <- paste0(attr_name_cat,collapse = " _; ")
        }else{
          cat_attr <- tibble(variable = i, name = attr_name_cat)
          cat_attr[[attr_col_name]] <- attr_content_col
          data_dict_cat <- data_dict_cat %>% full_join(cat_attr, by = intersect(names(data_dict_cat),names(cat_attr)))
        }
      }
    }
    data_dict[['Variables']]  <- data_dict[['Variables']]  %>% bind_rows(data_dict_var)
    data_dict[['Categories']] <- data_dict[['Categories']] %>% bind_rows(data_dict_cat)
  }

  names(data_dict[['Variables']])  <- make.unique(stringr::str_remove(names(data_dict[['Variables']]),"^Variables::"))
  names(data_dict[['Categories']]) <- make.unique(stringr::str_remove(names(data_dict[['Categories']]),"^Categories::"))

  if(sum(nrow(data_dict[['Categories']])) == 0)data_dict[['Categories']] <- NULL

  data_dict <-
    valueType_adjust(from = data,to = data_dict) %>%
    as_mlstr_data_dict()

  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param data xxx xxx xxx
#' @param data_dict xxx xxx xxx
#' @param data_dict_apply xxx xxx xxx
#' @param output xxx xxx xxx
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
data_dict_match_dataset <- function(data, data_dict, data_dict_apply = FALSE, output = c("data","data_dict")){

  # test
  as_data_dict_shape(data_dict)
  as_dataset(data)

  names_data <-  paste0("name %in% c('",paste0(names(data),collapse = "','"),"')")
  data_dict <- data_dict_filter(data_dict, filter_var = names_data)

  data <- data %>% select(data_dict[['Variables']]$`name`)

  if(length(data) == 0)                           warning("No match found between data and data dictionary")
  if(data_dict_apply == TRUE)                     return(data_dict_apply(data, data_dict))
  if(all(output[2:1] %in% c("data","data_dict"))) return(list(data = data, data_dict = data_dict))
  if(output[1] == c("data"))                      return(data)
  if(output[1] == c("data_dict"))                 return(data_dict)

  # else
  stop("output parameter must be either 'data' or 'data_dict'. Leave blank to get both.")
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
as_data_dict_shape <- function(object){

  # if the Variables sheet is given in parameter
  if(object %>% names %in% c('Variables') %>% sum == 1 & !is.data.frame(object)){

    # name column must exist
    if(is.null(object[['Variables']][['name']])){
      stop("Column 'name' in 'Variables' is missing in your data dictionary.")}

    # if Categories exists
    if(!is.null(object[['Categories']])){

      #, variable column must exist
      if(is.null(object[['Categories']][['variable']])){
        stop("Column 'variable' in 'Categories' is missing in your data dictionary.")}

      #, name column must exist
      if(is.null(object[['Categories']][['name']])){
        stop("Column 'name' in 'Categories' is missing in your data dictionary.")}
    }

    # if all test pass:
    attributes(object)$`Mlstr::class` <- "data_dict_structure"
    return(object)

  }

  # else
  stop(
    "\n\nThis object is not a in a standard data dictionary format. It
must be a list containing at least 'Variables' list of elements.
Please refer to documentation.")
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
#' @import dplyr tidyr stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_data_dict <- function(object){

  data_dict <- as_data_dict_shape(object)

  # variable names must be unique and non-null
  if(check_data_dict_variables(data_dict) %>% nrow > 0){
    stop("Variable names must exist and be unique in your data dictionary.",
         crayon::bold("\n\nUseful tip:")," Use check_data_dict_variables(data_dict) to get duplicated/missing variable names")}

  # data shaping
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    ungroup() %>%
    mutate(across(everything() ,~str_squish(.))) %>%
    mutate(across(everything() ,~na_if(.,"")))

  if(sum(nrow(data_dict[['Categories']])) > 0){

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      ungroup() %>%
      mutate(across(everything(),~str_squish(.))) %>%
      mutate(across(everything(),~na_if(.,"")))
  }

  # if not exists, addition of typeof for categorical variables, text else
  if(length(data_dict[['Variables']][['typeof']]) == 0){

    # test if vT exists and is good
    if(length(data_dict[['Variables']][['valueType']]) > 0){

      test_vT <-
        data_dict[['Variables']] %>%
        select(.data$`name`, .data$`valueType`) %>%
        mutate(`valueType` = replace_na(.data$`valueType`,"character")) %>%
        left_join(
          tinyPackage::valueType_list %>%
            select(
              valueType = .data$`valueType`,
              typeof = .data$`typeof`) %>%
            distinct,
          by = "valueType")

      # si pas bon de base, mettre un message et garder tO
      if(all(!is.na(test_vT))){

        data_dict[['Variables']] <-
          data_dict[['Variables']] %>%
          left_join(test_vT %>% select(.data$`name`,.data$`typeof`), by = 'name')

      }else if(sum(nrow(data_dict[['Categories']])) > 0){

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
          group_by(.data$`variable`) %>% group_split() %>% as.list() %>%
          lapply(function(x){
            test_vT <- str_detect(x$valueType[1], "\\|")
            if(test_vT) x <- x %>% mutate(valueType = valueType_guess(unique(x$name)))
            return(x)
          }) %>%
          bind_rows() %>%
          select(.data$`variable`,.data$`valueType`) %>% distinct %>%
          left_join(tinyPackage::valueType_list, by = "valueType") %>%
          select(name = .data$`variable`,.data$`typeof`)

        data_dict[['Variables']] <-
          left_join(data_dict[['Variables']], category_outcomes, by = "name") %>%
          mutate(`typeof` = replace_na(.data$`typeof`,'character'))

      }else{data_dict[['Variables']][['typeof']] <- 'character'}
    }else{data_dict[['Variables']][['typeof']] <- 'character'}
  }

  if(sum(nrow(data_dict[['Categories']])) > 0){

    # addition of valueType for sorting elements
    # if index, preserve it.
    index <- data_dict[['Categories']][['index']]
    data_dict[['Categories']][['index']] <- NULL

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      select(.data$`variable`, .data$`name`) %>% add_index() %>%
      left_join(data_dict[['Variables']] %>% select(variable = .data$`name`,.data$`typeof`), by = "variable") %>%
      group_by(typeof) %>% group_split() %>% as.list %>%
      lapply(function(x) {
        x$name <- as_valueType(x$`name`, valueType_guess(unique(x$`name`)))
        x <- x %>% arrange(.data$`variable`, .data$`name`) %>%
          mutate(name = as.character(.data$`name`))
        return(x)
      }) %>% bind_rows() %>%
      select(-.data$`typeof`) %>%
      left_join(data_dict[['Categories']] %>% add_index() %>% select(-.data$`name`),by = c("index", "variable"))

    data_dict[['Categories']][['index']] <- index

    # add labels if not exists
    if(length(data_dict[['Categories']][['labels']]) == 0){
      if(length(data_dict[['Categories']][['levels']]) > 0){

        # check if levels equals name (that means the levels are factors)
        if(all(data_dict[['Categories']][['levels']] == data_dict[['Categories']][['name']],na.rm = TRUE)){
          data_dict[['Categories']]['levels'] <- NULL}}

      # name label as names
      data_dict[['Categories']]['labels'] <- data_dict[['Categories']]['name']
    } # else do nothing

    # add na_values (as NA, will be removed anyway) if not exists
    if(length(data_dict[['Categories']][['na_values']]) == 0){

      # name label as names
      data_dict[['Categories']]['na_values'] <- NA_character_
    } # else do nothing

    # gather haven and factors if levels remain
    if(length(data_dict[['Categories']][['levels']]) > 0){

      # check if levels isnt NA when labels is (recip.) and
      # check if na_values is NA when levels is (recip.) (that means labels and levels are factors)

      if(all(!is.na(data_dict[['Categories']][['levels']]) == is.na(data_dict[['Categories']][['labels']]))){
        data_dict[['Categories']] <-
          data_dict[['Categories']] %>%
          mutate(
            labels = ifelse(!is.na(.data$`levels`) & is.na(.data$`na_values`),.data$`levels`,.data$`labels`),
            levels = ifelse(!is.na(.data$`levels`) & (.data$`levels` == .data$`labels`),NA_character_ ,.data$`levels`))}
    }

    # rearrange elements by missingness, then name, then variable
    new_name <-
      setdiff(
        make.unique(c('missing',names(data_dict[['Categories']])))[-1],
        names(data_dict[['Categories']]))

    names(data_dict[['Categories']]) <-
      make.unique(c("missing",names(data_dict[['Categories']])))[-1]

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      mutate(
        missing = !is.na(.data$`na_values`),
        missing = ifelse(is.na(.data$`missing`),FALSE,.data$`missing`)) %>%
      mutate(missing = ifelse((.data$`name` < 0 & .data$`missing` == TRUE), 2, .data$`missing`)) %>%
      group_by(.data$`variable`) %>%
      arrange(.data$`variable`,.data$`missing`) %>%
      ungroup() %>%
      mutate(missing = ifelse(.data$`missing` == 2, 1, .data$`missing`)) %>%
      mutate(missing = as.logical(.data$`missing`)) %>%
      select(-.data$`missing`) %>%
      rename_with(.cols = any_of(new_name), .fn = ~"missing")

  }


  # reorder things
  # data shaping
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>% select(.data$`name`,.data$`typeof`,everything()) %>%
    janitor::remove_empty("cols")

  if(sum(nrow(data_dict[['Categories']])) > 0){

    data_dict[['Categories']] <-
      inner_join(
        data_dict[['Variables']] %>% select(variable = .data$`name`), data_dict[['Categories']],
        by = "variable") %>%
      select(.data$`variable`,.data$`name`,.data$`labels`,matches("^na_values$"), everything()) %>%
      janitor::remove_empty("cols")
  }

  # if all test pass:
  attributes(data_dict)$`Mlstr::class` <- "data_dict"
  return(data_dict)
}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param object xxx xxx xxx
#' @param as_data_dict xxx xxx xxx
#'
#' @return xxx xxx xxx.
#'
#' @examples
#' \dontrun{
#' # Example 1: xxx xxx xxx.
#'
#' }
#'
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
as_mlstr_data_dict <- function(object, as_data_dict = FALSE){

  # test if data_dict is already data dictionary
  data_dict <- as_data_dict(object)

  # if valueType exists, vT must be valid
  if(suppressWarnings(check_data_dict_valueType(data_dict)) %>% nrow > 0){
    stop("valueType are incompatible with Maelstrom standards.",
         crayon::bold("\n\nUseful tip:")," Use check_data_dict_valueType(data_dict) to get incompatible valueType names")}

  # check missing validity
  if(suppressWarnings(check_data_dict_missing_categories(data_dict)) %>% nrow() != 0){
    stop(
      "\n\nIncompatible missing value in the missing columns with Maelstrom standards",
      crayon::bold("\n\nUseful tip:")," Use check_data_dict_missing_categories(data_dict) to get non matching elements")}

  # Check standard for names
  if(check_name_standards(var_names = data_dict[['Variables']][['name']]) %>% nrow > 0){
    stop("names are incompatible with Maelstrom standards.",
         crayon::bold("\n\nUseful tip:")," Use check_name_standards(data_dict[['Variables']][['name']]) to get incompatible name variables.")}

  # assess if tO is good
  test_vT <-
    data_dict[['Variables']] %>%
    select(.data$`name`, .data$`typeof`) %>%
    mutate(`typeof` = replace_na(.data$`typeof`,"character")) %>%
    left_join(
      tinyPackage::valueType_list %>%
        select(
          valueType = .data$`toValueType`,
          typeof = .data$`toTypeof`) %>%
        distinct,
      by = "typeof")

  # si pas bon de base, mettre un message et garder tO
  if(!all(!is.na(test_vT))){
    warning(
      "The column 'typeof' in your data dictionary contains values that were impossible
to coerce in valueType. This column can be kept for further investigations.",
      "\n\nVariable(s) name : ",
      toString(pull(test_vT[which(is.na(test_vT[['valueType']])),'name'])))
  }else{data_dict[['Variables']][['typeof']] <- NULL}

  # si vT existe
  # add valueType if not exists
  if(length(data_dict[['Variables']][['valueType']]) == 0){

    # sinon on rajoute vT au dd
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      left_join(test_vT %>% select(.data$`name`,.data$`valueType`), by = 'name')
  } # else do nothing

  # add label(:xx) if not present
  lab_name_var <-
    names(data_dict[['Variables']] %>% select(matches(c("^label$","^label:[[:alnum:]]"))))

  # add label if does not exists
  if(length(lab_name_var) == 0){
    data_dict[['Variables']] <-
      data_dict[['Variables']] %>% mutate(label = .data$`name`)}


  if(sum(nrow(data_dict[['Categories']])) > 0){

    # addition of label(:xx) if not present
    lab_name <-
      names(data_dict[['Categories']] %>% select(matches(c("^label$","^label:[[:alnum:]]"))))

    if(length(lab_name) == 0){

      # check presence of labels, if identical to name, NULL, rename else
      lab_name_var <-
        names(data_dict[['Variables']] %>%
                select(matches(c("^label$","^label:[[:alnum:]]"))))[1]

      data_dict[['Categories']] <-
        data_dict[['Categories']] %>%
        rename_with(.cols = "labels", ~ all_of(lab_name_var))

    }else if(all(data_dict[['Categories']][['labels']] ==
                 data_dict[['Categories']][['name']],na.rm = TRUE)) {
      data_dict[['Categories']][['labels']] <- NULL}

    # addition of missing if not present
    missing_name <-
      names(data_dict[['Categories']] %>% select(matches(c("^missing$"))))

    if(length(missing_name) == 0){

      # check presence of na_values, if identical to label, NULL, rename else
      if(length(data_dict[['Categories']][['na_values']]) == 0){

        data_dict[['Categories']]$`missing` <- FALSE

      }else if(all(data_dict[['Categories']][['na_values']] ==
                   data_dict[['Categories']][['label']],na.rm = TRUE)){
        data_dict[['Categories']]$`missing`  <- !is.na(data_dict[['Categories']]$`na_values`)
        data_dict[['Categories']]$`na_values` <- NULL
      }else{data_dict[['Categories']]$`missing` <- FALSE}

      # check if missings and na_values are duplicated
    }else if(all(!is.na(data_dict[['Categories']][['na_values']]) ==
                 data_dict[['Categories']][['missing']])){
      data_dict[['Categories']]$`na_values` <- NULL
    }

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      mutate(
        missing = fabR::as_any_boolean(.data$`missing`),
        missing = ifelse(is.na(.data$`missing`),FALSE,.data$`missing`)) %>%
      mutate(missing = ifelse((.data$`name` < 0 & .data$`missing` == TRUE), 2, .data$`missing`)) %>%
      group_by(.data$`variable`) %>%
      arrange(.data$`variable`,.data$`missing`) %>%
      ungroup() %>%
      mutate(missing = ifelse(.data$`missing` == 2, 1, .data$`missing`)) %>%
      mutate(missing = as.logical(.data$`missing`))

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      select(
        .data$`variable`,.data$`name`,
        matches(c("^label$","^label:[[:alnum:]]")),matches("^missing$"),
        everything())
  }

  if(sum(nrow(data_dict[['Categories']])) == 0) data_dict[['Categories']] <- NULL

  if(as_data_dict == TRUE){

    # # check if label and name are duplicated
    # if(length(data_dict[['Variables']][['label']]) > 0){
    #   if(all(data_dict[['Variables']][['name']] == data_dict[['Variables']][['label']])){
    #     data_dict[['Variables']]$`label` <- NULL}}

    # valueType as typeof
    if(length(data_dict[['Variables']][['typeof']]) == 0){

      data_dict[['Variables']] <-
        data_dict[['Variables']] %>%
        left_join(
          tinyPackage::valueType_list %>%
            select(
              valueType = .data$`valueType`,
              typeof = .data$`toTypeof`) %>%
            distinct,
          by = "valueType") %>%
        select(-.data$`valueType`)}

    # protection of labels if already exists
    if(length(data_dict[['Categories']][['labels']]) > 0){
      new_name <-
        setdiff(
          make.unique(c('labels',names(data_dict[['Categories']])))[-1],
          names(data_dict[['Categories']]))

      warning(
        "Your data dictionary contains 'labels' column, which usage is protected in R.
new name: ",new_name)

      names(data_dict[['Categories']]) <- make.unique(c('labels',names(data_dict[['Categories']])))[-1]
    }

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      rename_with(.cols = starts_with("label")[1], ~ 'labels')

    # protection of na_values if already exists
    if(length(data_dict[['Categories']][['na_values']]) > 0){
      new_name <-
        setdiff(
          make.unique(c('na_values',names(data_dict[['Categories']])))[-1],
          names(data_dict[['Categories']]))

      warning(
        "Your data dictionary contains 'na_values' column, which usage is protected in R.
new name: ",new_name)

      names(data_dict[['Categories']]) <- make.unique(c('na_values',names(data_dict[['Categories']])))[-1]
    }

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      rename_with(.cols = "missing", ~ 'na_values') %>%
      mutate(
        na_values = ifelse(.data$`na_values` == TRUE,.data$`labels`, NA_character_))

    data_dict[['Variables']] <-
      data_dict[['Variables']] %>%
      select(.data$`name`,.data$`typeof`,everything())

    data_dict[['Categories']] <-
      data_dict[['Categories']] %>%
      select(.data$`variable`,.data$`name`,.data$`labels`,.data$`na_values`,everything())
  }

  # reorder things
  data_dict[['Variables']] <-
    data_dict[['Variables']] %>%
    select(.data$`name`,matches(c("^label$","^label:[[:alnum:]]")),
           matches('^valueType$'),everything()) %>%
    janitor::remove_empty("cols")

  if(sum(nrow(data_dict[['Categories']])) > 0){
    data_dict[['Categories']] <-
      data_dict[['Variables']]['name'] %>%
      rename(variable = .data$`name`) %>%
      inner_join(data_dict[['Categories']],by='variable') %>%
      select(.data$`variable`,.data$`name`,matches(c("^label$","^label:[[:alnum:]]")),everything()) %>%
      janitor::remove_empty("cols")}

  if(as_data_dict == TRUE) {attributes(data_dict)$`Mlstr::class` <- "data_dict"
  }else{                    attributes(data_dict)$`Mlstr::class` <- "Mlstr_data_dict"}

  return(data_dict)
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
is_data_dict_shape <- function(object){

  object <- object
  # if only the data dictionary shape is given in parameter
  if(class(try(fabR::silently_run(as_data_dict_shape(object))))[1] == 'try-error') return(FALSE)
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
is_data_dict <- function(object){

  object <- object
  # if only the tibble is given in parameter
  if(class(try(fabR::silently_run(as_data_dict_shape(object))))[1] == 'try-error') return(FALSE)
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
is_mlstr_data_dict <- function(object){

  object <- object
  # if only the tibble is given in parameter
  if(class(try(fabR::silently_run(as_mlstr_data_dict(object))))[1] == 'try-error') return(FALSE)
  return(TRUE)

}
