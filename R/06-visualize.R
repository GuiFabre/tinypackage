#' Generate a visual report for a study-specific dataset
#'
#' This function generates a visual report for a study-specific dataset in an html
#' bookdown document, showing descriptive statistics for each study-specific variable
#' to facilitate assessment of input data. Statistics and figures are generated
#' according to their valueTypes. The variable valueTypes is automatically detected
#' from the dataset and its data dictionary. The output is editable (using plotly
#' library) or static (using ggplot2 library).
#'
#' Must provide dataset and data dictionary in the Maelstrom Research formats.
#' If the data dictionary is not provided, a basic one is being created during the process.
#'
#' @param dataset A character string or tibble R object identifying the input dataset
#' (in the Maelstrom Research formats). Dataset in Maelstrom Research format has
#' the entity (usually participant) ID as the first column.
#' @param data_dict A tibble identifying the data dictionary (in the Maelstrom Research formats) associated
#' with the dataset. Automatically generated if not provided.
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param to A character string identifying the folder path where the bookdown report
#' will be saved.
#' @param out parameter that specifies the graphical outputs expected in the report:
#' can be either 'ggplot2' or 'plotly'. gglot2 renders static plots, plotly dynamic plots.
#' @param .keep_files Boolean value to say whether to keep the R-markdown files.
#'
#' @return A bookdown folder containing files in the specified output folder
#' (current folder/docs by default). To open the file in browser, open "index.html"
#'
#' @examples
#' \dontrun{
#'
#' # Create index of files in one folder and read the files
#' index_DEMO <- file_index_create(folder = "DEMO")
#' file_index_read(index = index_DEMO, file_name = "study_TOKYO")
#' file_index_read(index = index_DEMO, file_name = "dd_TOKYO")
#'
#' # use case 1: report of demo dataset TOKYO
#' study_visual_report(
#'   dataset = study_TOKYO,
#'   data_dict = dd_TOKYO_format_maelstrom_tagged,
#'   to = "DEMO/reports/TOKYO")
#'
#' # use case 2: report of demo dataset TOKYO, grouped by gndr
#' study_visual_report(
#'   dataset = study_TOKYO,
#'   data_dict = dd_TOKYO_format_maelstrom_tagged,
#'   to = "DEMO/reports/TOKYO_gndr",group_by = "gndr",out = "ggplot2")
#'
#'# re-index files to include new files created
#' index_DEMO <- file_index_create(folder = "DEMO")
#'
#' # read the book down
#' file_index_read(index_DEMO,file_path = "DEMO/reports/TOKYO_gndr/docs/index.html")
#' }
#'
#' @import dplyr bookdown utils rlang readr stringr grDevices fs janitor DT
#' @export
study_visual_report <- function(
    dataset,
    data_dict = NULL,
    group_by = NULL,
    to = paste0("reports/"),
    out = "ggplot2",
    .keep_files = FALSE){

  if(is.null(data_dict)){data_dict <- data_dict_extract(dataset)}

  if(!"summary_1" %in% (data_dict$Variables %>% names)){

    data_dict$Variables <- data_dict$Variables %>% fabR::add_index(.force = TRUE)
    data_dict <-
      data_dict %>%
      identify_visual_type(data_dict = ., dataset = dataset) %>%
      identify_plot_type(data_dict = ., dataset = dataset, group_by = group_by, out = out)
  }

  if(nrow(data_dict$Variables) == 0){

    return(message(
"[Error]: the name of your dataset has not been found in your data dictionary. Please verify
the name of your dataset in your datadictionary (column 'name' in 'Variables' sheet)
and reprocess."))
  }

  # global data
  ## dataset must have ID in first column

  # silently_run(count_tag <- count_tag(data_dict))
  all_na_column <- get_all_na_cols(dataset)
  nb_unique_participants <- dataset %>% select(1) %>% unique %>% nrow()

  template_visual_report(to)
  save(to,data_dict,group_by ,file = paste0(file = paste0(to,"/temp_bookdown_report/bookdown_report.RData")))

  ## markdown writing elements

  ##### HEADER ##########

  paste0(
    '# About the study dataset {.unnumbered #about}

```{r echo = FALSE, message = FALSE, warning = FALSE}

library(tinyPackage)
load(file = paste0(file = paste0("',getwd(),"/",to,'/temp_bookdown_report/bookdown_report.RData")))

```
--------------------------------------------------------------------------------


**Number of unique participants**: ',dataset %>% nrow,'

**Number of variables (including id column)**: ',dataset %>% ncol,'

**Variables where observations are all NA**: ',all_na_column %>% toString(),'

## Variables

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(
  data_dict$Variables %>%
    select(-viz_type, -code_dd, -plot_1, -plot_2, -plot_3, -plot_4, -summary_1) %>%
    filter(name != "',dataset[1] %>% names,'") %>%
    remove_empty("cols") %>%
    mutate(name = paste0("<a href=\\"./var",index,".html\\" >",name,"</a>")),
  options = list(scrollX = TRUE),rownames = FALSE,escape = FALSE)

```
--------------------------------------------------------------------------------

## Categories

```{r echo = FALSE, message = FALSE, warning = FALSE}

datatable(data_dict$Categories %>% remove_empty("cols"),
  options = list(scrollX = TRUE),rownames = FALSE)

```
--------------------------------------------------------------------------------

') %>% write_lines(.,file = paste0(to,"/temp_bookdown_report/file/bookdown-template-master/index.Rmd"), append = TRUE)


  # ## Areas of information
  #
  # ```{r echo = FALSE, message = FALSE, warning = FALSE}
  #
  # # fabR::plot_bar("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  # # fabR::plot_pie("data_dict$Variables", "mlstr_area_1", out = "plotly-code")
  #
  # ```
  #

  ##### CONTENT ##########

  increment <-   paste0(rep(0,((nrow(data_dict$Variables)) %>% nchar)) %>% paste(collapse = ""))

  for(i in 1:nrow(data_dict$Variables)){

    rmd_file_name <-
      paste0(to,"/temp_bookdown_report/file/bookdown-template-master/",
             str_sub(paste0(increment,i),-(increment %>% nchar + 1),-1),"-",
             data_dict$Variables$name[i],".Rmd")
    file.create(rmd_file_name)

    paste0(
      "# ",data_dict$Variables$name[i],"{.unnumbered #var",i,"}\n\n") %>%

      paste0("\n**VARIABLE CHARACTERISTICS**\n") %>%

      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0(
        "\n```{r echo = FALSE, message = FALSE, warning = FALSE, knitr.figure = TRUE}\n",
        "datatable(
   data_dict$Variables %>%
     filter(name == '",data_dict$Variables$name[i],"') %>%
     select(-viz_type, -code_dd, -plot_1, -plot_2, -plot_3, -plot_4, -summary_1) %>%
     gather %>% filter(!is.na(value)) %>%
     mutate(key = paste0('<b>' , key, '</b>')),
   options = list(dom = 't', scrollX = TRUE, ordering = FALSE,paging = TRUE),
   rownames = FALSE, colnames = rep('', 2),filter = 'none' ,  escape = FALSE)",
        "\n```\n") %>%

      paste0("\n</div>\n\n") %>%
      paste0(
        ifelse(data_dict$Categories %>% filter(.data$`variable` == data_dict$Variables$name[i]) %>% nrow > 0,
               paste0("\n* **Categories**: ","\n\n") %>%
                 paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
                 paste0(
                   "\n```{r echo = FALSE, message = FALSE, warning = FALSE}\n",
                   "datatable(
   data_dict$Categories %>% filter(variable == '",data_dict$Variables$name[i],"'),
   options = list(scrollX = TRUE),rownames = FALSE)",
                   "\n```\n") %>%
                 paste0("\n</div>\n\n")
               ,"")) %>%
      paste0("\n--------------------------------------------------------------------------------\n") %>%
      paste0("\n**SUMMARY STATISTICS**\n") %>%
      paste0("\n<div style= \"display:flex; margin:auto\" > \n\n") %>%
      paste0("\n```{r echo = FALSE, message = FALSE, warning = FALSE, knitr.figure = TRUE}\n\n",data_dict$Variables$summary_1[i],"\n\n```\n") %>%
      paste0("\n</div>\n\n") %>%
      paste0("\n--------------------------------------------------------------------------------\n") %>%
      paste0("\n**VISUAL REPRESENTATION**\n") %>%

      paste0("\n```{r, figures-plot12-",i,", fig.show='hold',fig.align = 'center', echo = FALSE, message = FALSE, warning = FALSE}\n",
             "\n","try({",data_dict$Variables$plot_1[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_2[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_3[i],"}, silent = TRUE)","\n",
             "\n","try({",data_dict$Variables$plot_4[i],"}, silent = TRUE)","\n",
             "\n```\n") %>%
      paste0("\n") %>%

      write_lines(.,file = rmd_file_name, append = FALSE)
  }

  wd <- getwd()
  graphics.off()

  setwd(paste0(wd,"/",to,"/temp_bookdown_report/file/bookdown-template-master/"))
  silently_run(file.remove(list.files() %>% str_subset(dataset[1] %>% names)))
  try({render_book(paste0(wd,"/",to,"/temp_bookdown_report/file/bookdown-template-master/index.Rmd"))},silent = FALSE)
  setwd(wd)

  if(dir.exists(paste0(to,"/docs"))){  dir_delete(paste0(to,"/docs"))}
  dir_copy(paste0(to,"/temp_bookdown_report/file/bookdown-template-master/docs"),
           paste0(to,"/docs"))

  if(.keep_files == FALSE){dir_delete(paste0(to,"/temp_bookdown_report/"))}

  browseURL(paste0(to,"/docs/index.html"))


  return(message(paste0("\n\nTo edit your file, open: ",
                        to,"/docs/index.html in your browser (Compatibility tested on Chrome and Mozilla)\n\n")))

}


#' Identify visual type of a variable based on valueType
#'
#' This helper function analyses the content of a dataset and it data dictionary
#' to extract the type of visualization to generate in a report. This function can
#' be used to manually personalize the report parameters.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param data_dict tibble of the data dictionary (automatically generated if not provided)
#'
#' @return A list of two tibbles which makes up the data dictionary in Maelstrom Research format
#' where a column 'viz_type' has been added to the datadictionary provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: viz type of iris dataset
#' library(tidyverse)
#' identify_visual_type(dataset = iris) %>% .$Variables %>% select(name,viz_type)
#' }
#'
#' @import readr dplyr tibble
#' @export
identify_visual_type <- function(dataset, data_dict){

  try({

    if(sum(nrow(data_dict[['Categories']])) > 0 ){

      var_names_cat_dd <-
        data_dict[['Categories']] %>%
        select(.data$`variable`, code = .data$`name`) %>% unique %>%
        group_by(.data$`variable`) %>%
        summarise(code_dd = paste(.data$`code`,collapse = "','")) %>%
        mutate(code_dd = paste0("c('NA','",.data$`code_dd`,"')")) %>%
        filter(!is.na(.data$`variable`))
    }else{
      var_names_cat_dd <-
        tibble(variable = as.character(), code = as.character(), code_dd = as.character())
    }

    if(nrow(var_names_cat_dd) == 0){
      data_dict$Variables <- data_dict$Variables %>%
        rename(variable = .data$`name`) %>%
        filter(.data$`variable` %in% (dataset %>% names)) %>%
        mutate(viz_type = .data$`valueType`) %>%
        rename(name = .data$`variable`)
    }else{

      name_var <- dataset[dataset %>% names() %in% (var_names_cat_dd$variable)] %>% names
      var_name_cat_dataset <- tibble(variable = as.character(),code_dataset = as.character())
      for(i in name_var){
        var_name_cat_dataset <-
          add_row(
            var_name_cat_dataset,
            dataset[i] %>% unique %>% pull(.) %>% toString(.) %>%
              str_replace_all(", ","','") %>% paste0("c('",.,"')") %>%
              as_tibble() %>% mutate(variable = i) %>%
              select(.data$`variable`, code_dataset = .data$`value`))
      }

      to_eval <-
        var_name_cat_dataset %>% inner_join(var_names_cat_dd) %>%
        mutate(to_eval = paste0("all(",.data$`code_dataset`," %in% ",.data$`code_dd`,")"))

      to_eval <-
        to_eval %>% rowwise %>% mutate(to_eval = parceval(.data$`to_eval`)) %>%
        mutate(viz_type = ifelse(.data$`to_eval` == TRUE,"categorical","dual")) %>%
        select(.data$`variable`,.data$`viz_type`)

      data_dict$Variables <-
        data_dict$Variables %>% rename(variable = .data$`name`) %>%
        left_join(.,.data$`to_eval`) %>%
        filter(.data$`variable` %in% (dataset %>% names)) %>%
        mutate(viz_type = ifelse(is.na(.data$`viz_type`), .data$`valueType`,.data$`viz_type`)) %>%
        rename(name = .data$`variable`)
    }

  },silent = TRUE)


  return(data_dict)

}

#' Generate R script for plots based on the 'viz_type' of the variable
#'
#' This helper function uses the visual type attributed to a variable in a data dictionary
#' to generate R script to generate plots in a report. This function can be used to
#' manually personalize the report parameters. The plots can use an additional variable
#' to group each variable shown by the grouping variable.
#'
#' The user must run identify_visual_type first to run this function.
#'
#' @param dataset A character string or tibble specifying the input dataset
#' @param data_dict tibble of the data dictionary (automatically generated if not provided)
#' @param group_by A character string of one column in the dataset that can be
#' taken as a grouping column. The visual element will be grouped and displayed
#' by this column.
#' @param out parameter that specifies the graphical outputs expected in the report:
#' can be either 'ggplot2' or 'plotly'. gglot2 renders static plots, plotly dynamic plots.
#'
#' @return A list of two tibbles which make up the data dictionary in Maelstrom Research
#' format where columns plots and summary have been added to the data dictionary
#' provided as an input.
#'
#' @examples
#' \dontrun{
#' # Example 1: plot R stripts for iris variables.
#' data_dict_extract(iris, categories = "Species") %>%
#' identify_visual_type(data_dict = ., dataset = iris) %>%
#'   identify_plot_type(data_dict = ., dataset = iris) %>% .$Variables %>%
#'   select(name,viz_type, contains("plot"),contains("summary"))
#' }
#'
#' @import dplyr ggplot2
#' @export
identify_plot_type <- function(dataset = NULL, data_dict, group_by = NULL, out = "plotly"){

  if(! "viz_type" %in% colnames(data_dict$Variables)){
    data_dict$Variables <- data_dict$Variables %>% mutate(viz_type = .data$`valueType`)
  }

  if(sum(nrow(data_dict[['Categories']])) == 0 ){
    data_dict[['Categories']] <-
      tibble(variable = as.character(), name = as.character(), missing = as.logical(), code_dd = as.character())}

  data_dict$Variables <-
    data_dict$Variables %>%
    left_join(
      data_dict$Categories %>%
        select(name = .data$`variable`, code = .data$`name`, .data$`missing`) %>%
        filter(.data$`missing` == 1) %>%
        group_by(.data$`name`) %>%
        summarise(code_dd = paste(.data$`code`,collapse = "','")) %>%
        mutate(code_dd = paste0("c('",.data$`code_dd`,"')")) %>%
        filter(!is.na(.data$`name`))) %>%
    mutate(code_dd = replace_na(.data$`code_dd`,"c()"))

  group_by <- ifelse(is.null(group_by),'NULL', paste0("'",group_by,"'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_1 = case_when(
        .data$`viz_type` == "text"                              ~ paste0('fabR::plot_main_word(tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "decimal"                           ~ paste0('fabR::plot_box(      tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "integer"                           ~ paste0('fabR::plot_box(      tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "date"                              ~ paste0('fabR::plot_date(     tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,' , time = "year")'),

        .data$`viz_type` == "dual"  & .data$`valueType` == "text"       ~ paste0('fabR::plot_main_word(tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"    ~ paste0('fabR::plot_density(  tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"    ~ paste0('fabR::plot_box(      tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "date"       ~ paste0('fabR::plot_date(     tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,' , time = "year")') ,
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_2 = case_when(
        .data$`viz_type` == "decimal"                           ~ paste0('fabR::plot_density(  tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "integer"                           ~ paste0('fabR::plot_histogram(tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "categorical"                       ~ paste0('fabR::plot_bar(      tbl = dataset,col = "',.data$`name`,'"                                         , out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"    ~ paste0('fabR::plot_box(      tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"    ~ paste0('fabR::plot_histogram(tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')') ,
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      plot_3 = case_when(
        .data$`viz_type` == "categorical"                       ~ paste0('plot_pie(      dataset = dataset,col = "',.data$`name`,'" ,                                 out = "',out,'-code", group_by = ',group_by,')'),
        TRUE                      ~ "'message(\"\")'"))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(plot_4 =                              paste0('fabR::plot_pie_valid_value(   tbl = dataset,col = "',.data$`name`,'" , missing_values = "',.data$`code_dd`,'", out = "',out,'-code", group_by = ',group_by,')'))

  data_dict$Variables <-
    data_dict$Variables %>%
    mutate(
      summary_1 = case_when(
        .data$`viz_type` == "text"                              ~ paste0('fabR::summary_text(     tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "decimal"                           ~ paste0('fabR::summary_numerical(tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "integer"                           ~ paste0('fabR::summary_numerical(tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "date"                              ~ paste0('fabR::summary_text(     tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "categorical"                       ~ paste0('fabR::summary_category( tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),

        .data$`viz_type` == "dual"  & .data$`valueType` == "text"       ~ paste0('fabR::summary_text(     tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "decimal"    ~ paste0('fabR::summary_numerical(tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "integer"    ~ paste0('fabR::summary_numerical(tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        .data$`viz_type` == "dual"  & .data$`valueType` == "date"       ~ paste0('fabR::summary_text(     tbl = dataset,col = "',.data$`name`,'", missing_values = "',.data$`code_dd`,'", out = "DT-code", group_by = ',group_by,')'),
        TRUE                      ~ NA_character_))
  # this_dd <<- data_dict

  for (i in 1:length(data_dict$Variables$index)) {
    data_dict$Variables$plot_1[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_1[i])))
    data_dict$Variables$plot_2[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_2[i])))
    data_dict$Variables$plot_3[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_3[i])))
    data_dict$Variables$plot_4[i]    <- eval(parse(text = str_squish(data_dict$Variables$plot_4[i])))
    data_dict$Variables$summary_1[i] <- eval(parse(text = str_squish(data_dict$Variables$summary_1[i])))
  }

  return(data_dict)

}

#' xxx xxx xxx
#'
#' xxx xxx xxx.
#'
#' @param report_name xxx xxx xxx
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
open_report <- function(report_name){

  utils::browseURL(paste0(report_name,"/docs/index.html"))

}


