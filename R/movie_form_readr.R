movie_form_savr <- function(number = NULL, n_max = 50){
  drive_file <-
    googledrive::drive_find(
      pattern = "Quarantine",
      type = "spreadsheet",
      n_max = n_max
    ) %>% 
    dplyr::mutate(
      num = stringr::str_extract(name, "\\d+") %>% 
        as.numeric()
    ) %>% 
    dplyr::arrange(dplyr::desc(num)) %>% 
    {if (is.null(number)) {
      dplyr::slice(., 1)
    } else {
      dplyr::filter(
        .,
        num == number
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
      dplyr::filter(
        ., 
        data == paste0(
          "data/q", number, ".csv")
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

