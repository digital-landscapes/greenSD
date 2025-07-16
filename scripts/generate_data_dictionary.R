library(dplyr)

get_data <- function() {
  part_2022 <- c(
    24954680,
    24954784,
    24954947
  )

  part_2021 <- c(
    24954250,
    24954411,
    24954591
  )

  part_2020 <- c(
    24947338,
    24948461,
    24949091
  )

  part_2019 <- c(
    24943073,
    24945059,
    24945394
  )
  ids <- c(part_2019, part_2020, part_2021, part_2022)
  result <- list()

  info_extract <- function(filename) {
    year <- stringr::str_extract(filename, "^\\d{4}")
    id <- stringr::str_extract(filename, "(?<=UID_)\\d+")
    return(list(year, id))
  }

  for (i in 1:length(ids)) {
    res <- httr2::request(paste0("https://api.figshare.com/v2/articles/", ids[i], "/versions/", 2)) %>%
      httr2::req_perform()
    data <- res %>% httr2::resp_body_json()
    file_info <- do.call(rbind, lapply(data$files, function(f) {
      info <- info_extract(f$name)
      data.frame(
        name = f$name,
        year = info[[1]],
        uid = info[[2]],
        download_url = f$download_url
      )
    }))
    result[[i]] <- file_info[!duplicated(file_info$name),]
  }
  return(dplyr::bind_rows(result))
}

data_dictionary <- get_data()
