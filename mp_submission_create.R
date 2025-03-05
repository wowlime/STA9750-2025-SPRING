source("https://michael-weylandt.com/STA9750/load_helpers.R")
mp_submission_create <- function(N, github_id){
    library(rvest)
    library(glue)
    library(tidyverse)
    library(httr2)
    library(gh)
    
    if(missing(N)){
      N <- menu(title="Which Mini-Project would you like to submit on GitHub?", 
                choices=c(0, 1, 2, 3, 4))
    }
    
    mp_url <- glue("https://michael-weylandt.com/STA9750/miniprojects/mini0{N}.html")
    
    mp_text <- read_html(mp_url) |> html_element("#submission-text") |> html_text()
    
    if(missing(github_id)){
      github_id <- readline("What is your GitHub ID? ")
    }
    
    title <- glue("{course_short} {github_id} MiniProject #0{N}")
    
    body <- mp_text |> str_replace("<GITHUB_ID>", github_id)
    
    r <- request("https://github.com/") |>
      req_url_path_append("michaelweylandt") |>
      req_url_path_append(course_repo) |>
      req_url_path_append("issues/new") |>
      req_url_query(title=title, 
                    body=body) 
    
    browseURL(r$url)
  }