require(tidyverse)
require(janitor)
require(curl)
#^this package will help u download the files
require(zip)
#^this package will help u unzip the files
require(here)
#^this is a very cool package that helps with filepaths
#but it's only an R thing, other languages u have to setwd()
#here's some info about the here library it is cool: https://here.r-lib.org/

#' Title
#'
#' @param dropbox_link a link to a dropbox *FOLDER* that you *HAVE ACCESS TO*
#'
#' @returns a link to the local directory the contents of the dropbox folder have been downloaded to
#' @export
#'
#' @examples
#' dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&dl=1")
#' ^but you need access to the Speckle_Imaging folder in dropbox for this to work
dropbox_downloader <- function(dropbox_link) {
  #if you don't have a data folder to put your data in, make one right now!
  if (!dir.exists(here("input_data", "unzipped"))) {
    if (!dir.exists(here("input_data"))) {
      dir.create(here("input_data"))
    }
    dir.create(here("input_data", "unzipped"))
  }
  #dropbox link to  SandboxAQ/Data Related
  #when you copy it, replace the "0" at the end with a "1"
  #1 signals to ur computer to download the docs at the link, not just open it
  #why? i have no idea
  # dropbox_link <- "https://www.dropbox.com/scl/fo/cpe30c13afjyc7n8p59iv/AGWgm1eXv4BNtDiBCAJyQu0?rlkey=wsepne126byuhkbz1fj6gepiu&dl=1"
  #if user doesn't replace 0 at end with a 1, i'll do that here
  if (substr(dropbox_link, nchar(dropbox_link), nchar(dropbox_link)) == 0) {
    dropbox_link <- str_replace(dropbox_link, "0$", "1")
  }
  destination_dropbox <- file.path(here("input_data"), "dropbox_data.zip")
  #download dropbox folder as a zip file
  curl::multi_download(url = dropbox_link, destfile = destination_dropbox)
     #unzip the file
  local_path <- here("input_data", "unzipped")
  # zip::unzip(zipfile = destination_dropbox, exdir = local_path)
  system(paste("unzip", destination_dropbox, "-d", local_path))
  return(local_path)
}
