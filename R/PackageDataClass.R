NULL

#' PackageDataClass
#'
#' A helper class
#' @export
#'
PackageDataClass <- R6::R6Class(
  "PackageDataClass",
  inherit = BaseClass,
  public = list(

    CacheFolder = "data-raw/cache",
    PreCached = FALSE,

    GetHtmlTable = function(url, indexTable = 1, colNames = NA_character_){

      Page <- xml2::read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")

      HtmlTable <- Page %>%
        rvest::html_nodes("table") %>%
        .[[indexTable]] %>%
        rvest::html_table() %>%
        tibble::as_tibble()

      if (!all(is.na(colNames))){
        names(HtmlTable) <- colNames
      }
      return(HtmlTable)
    },

    DownloadAndExtractZip = function(url, folder = NA_character_){

      Folder <- if_else(is.na(folder), self$CacheFolder, folder)

      if (!self$PreCached) {
        tmp <- tempfile(fileext = ".zip")
        download.file(url, tmp, quiet = TRUE)
        unzip(tmp, exdir = Folder)
        unlink(tmp)
      }

    },

    initialize = function(
      cacheFolder = NA
    ){
      # library(dplyr, warn.conflicts = FALSE)
      # library(reshape2)
      # library(tidyr)
      # library(readr)
      # library(tidyverse)
      # library(rvest)


      if (!is.na(cacheFolder)){
        self$CacheFolder <- file.path(self$CacheFolder, cacheFolder)
      }

      self$PreCached <- dir.exists(self$CacheFolder)

      if(!self$PreCached){
        dir.create(self$CacheFolder, recursive = TRUE)
      }

    }
  )
)




