movie_form_savr <- function(style, number = NULL, n_max = 50){
  drive_file <-
    googledrive::drive_find(
      pattern = "Quarantine",
      type = "spreadsheet",
      n_max = n_max
    ) %>% 
    dplyr::filter(
      stringr::str_detect(
        name, style
      )
    ) %>% 
    {if (is.null(number)) {
      dplyr::arrange(., dplyr::desc(name)) %>%
        dplyr::slice(1)
    } else {
      dplyr::filter(
        ., stringr::str_detect(
          name, paste0("#", number, " \\(Responses\\)")
        )
      )
    }
    }
  
  googledrive::drive_download(
    file = drive_file,
    path = paste0(
      "data/q",
      stringr::str_to_lower(style),
      stringr::str_extract(drive_file$name, "\\d+")
    ),
    type = "csv",
    overwrite = TRUE
  )
}

movie_form_readr <- function(style, number = NULL) {
  list.files("data/", full.names = TRUE) %>% 
    stringr::str_subset(
      stringr::str_to_lower(style)
    ) %>% 
    {if (is.null(number)) {
      stringr::str_sort(
        ., decreasing = TRUE, numeric = TRUE
      )
    } else {
      stringr::str_subset(
        ., paste0(
          stringr::str_to_lower(style),
          number, ".csv")
      )
    }
    } %>% 
    dplyr::first() %>% 
    readr::read_csv()
}

movie_form_cleanr <- function(data) {
  data %>% 
    dplyr::rename_at(
      dplyr::vars(contains("[")),
      ~stringr::str_extract(.x, "(?<=\\[).*(?=\\])")
    )
}


  