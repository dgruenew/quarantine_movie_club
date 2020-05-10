movie_form_savr <- function(number = NULL, n_max = 50){
  drive_file <-
    googledrive::drive_find(
      pattern = "Quarantine",
      type = "spreadsheet",
      n_max = n_max
    ) %>% 
    {if (is.null(number)) {
      dplyr::arrange(., dplyr::desc(
        stringr::str_extract(name, "\\d+")
        )) %>%
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
      stringr::str_extract(drive_file$name, "\\d+")
    ),
    type = "csv",
    overwrite = TRUE
  )
}

movie_form_readr <- function(number = NULL) {
  tibble::tibble(
    data = list.files("data/", full.names = TRUE)
  ) %>% 
    dplyr::mutate(
      id = stringr::str_extract(data, "\\d+") %>% 
        as.numeric()
    ) %>% 
    {if (is.null(number)) {
      dplyr::arrange(., desc(id))
    } else {
      stringr::str_subset(
        ., paste0(
          number, ".csv")
      )
    }
    } %>% 
    dplyr::pull(data) %>% 
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

# tibble(data = list.files("data/", full.names = TRUE))
# list.files("data/", full.names = TRUE)


  