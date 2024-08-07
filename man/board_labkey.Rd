% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board_labkey.R
\name{board_labkey}
\alias{board_labkey}
\title{Use a LabKey folder as a board}
\usage{
board_labkey(
  base_url,
  folder,
  subdir = "pins",
  versioned = TRUE,
  api_key = Sys.getenv("LABKEY_API_KEY"),
  cache_alias = NULL
)
}
\arguments{
\item{base_url}{The baseUrl of the Labkey server}

\item{folder}{The folder path (aka folderPath) within the LabKey server to read/write pins}

\item{subdir}{The subdirectory within the LabKey folder (aka remoteFilePath) where pin should be stored (default "pins")}

\item{versioned}{Boolean; whether to version the pin (default TRUE)}

\item{api_key}{API key to use for LabKey authentication. If not specified, will use \code{LABKEY_API_KEY}}

\item{cache_alias}{Alias of the board to use in cache (if not specified will use default pins cache location)}
}
\value{
A board object of class "pins_board_labkey"
}
\description{
Pin data to a folder on a LabKey server
}
\details{
\code{board_labkey()} is powered by the Rlabkey package \url{https://github.com/cran/Rlabkey}
}
\examples{
\dontrun{
board <- board_labkey(
  base_url = "https://learn.labkey.com/",
  folder = "LabKey_Board/"
)
board \%>\% pin_write(mtcars)
board \%>\% pin_read("mtcars")
}
}
