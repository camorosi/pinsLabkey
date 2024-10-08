#' @title Use a LabKey folder as a board
#'
#' @description Pin data to a folder on a LabKey server
#'
#' @details `board_labkey()` is powered by the Rlabkey package <https://github.com/cran/Rlabkey>
#'
#' @param base_url The baseUrl of the Labkey server
#' @param folder The folder path (aka folderPath) within the LabKey server to read/write pins
#' @param subdir The subdirectory within the LabKey folder (aka remoteFilePath) where pin should be stored (default "pins")
#' @param versioned Boolean; whether to version the pin (default TRUE)
#' @param api_key API key to use for LabKey authentication. If not specified, will use `LABKEY_API_KEY`
#' @param cache_alias Alias of the board to use in cache (if not specified will use default pins cache location)
#'
#' @return A board object of class "pins_board_labkey"
#'
#' @examples
#' \dontrun{
#' board <- board_labkey(
#'   base_url = "https://learn.labkey.com/",
#'   folder = "LabKey_Board/"
#' )
#' board %>% pin_write(mtcars)
#' board %>% pin_read("mtcars")
#' }
#' @export
board_labkey <- function(
    base_url,
    folder,
    subdir = "pins",
    versioned = TRUE,
    api_key = Sys.getenv("LABKEY_API_KEY"),
    cache_alias = NULL) {
  if (nchar(base_url) == 0) {
    stop("The 'labkey' board requires a 'base_url' parameter for the LabKey server.")
  }
  if (nchar(folder) == 0) {
    stop("The 'labkey' board requires a 'folder' parameter for top folder to house the subdirectory for all pins")
  }

  # Globally sets the api_key and base_url
  Rlabkey::labkey.setDefaults(apiKey = api_key, baseUrl = base_url)

  dirExists <- Rlabkey::labkey.webdav.pathExists(folderPath = folder, remoteFilePath = subdir)

  # Make labkey directory if does not exist
  if (!dirExists) {
    labkey_check_permissions(folder = folder, subdir = "", permission_to_check = "canUpload") # need to check parent if dir doesn't exist
    Rlabkey::labkey.webdav.mkDir(folderPath = folder, remoteFilePath = subdir)
  }

  # Use domain and folder name is cache alias not provided
  if (is.null(cache_alias)) {
    # TODO include subdir as well?
    domain <- strsplit(gsub("http://|https://|www\\.", "", base_url), "/")[[c(1, 1)]]
    folder_cleaned <- paste0(domain, "-", gsub(" ", "-", basename(folder)))
    cache <- pins::board_cache_path(folder_cleaned)
  } else {
    cache <- pins::board_cache_path(cache_alias)
  }
  pins:::new_board_v1(
    board = "pins_board_labkey",
    name = "labkey",
    base_url = base_url,
    folder = folder,
    subdir = subdir,
    api_key = api_key,
    cache = cache,
    versioned = versioned
  )
}

#' @importFrom pins pin_list
#' @importFrom purrr map_chr
#' @export
pin_list.pins_board_labkey <- function(board, ...) {
  resp <- Rlabkey::labkey.webdav.listDir(
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = board$subdir,
    fileSet = "@files"
  )
  final_list <- resp$files

  paths <- fs::path_file(purrr::map_chr(final_list, ~ .$id))
  paths
}

#' @importFrom pins pin_exists
#' @export
pin_exists.pins_board_labkey <- function(board, name, ...) {
  dir_exists <- Rlabkey::labkey.webdav.pathExists(
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = fs::path(board$subdir, name)
  )
  # if the directory exists, make sure there's files within it
  if (dir_exists) {
    pin_files <- Rlabkey::labkey.webdav.listDir(
      baseUrl = board$base_url,
      folderPath = board$folder,
      remoteFilePath = fs::path(board$subdir, name)
    )
    length(pin_files$files) > 0
  } else {
    return(FALSE)
  }
}

#' @importFrom pins pin_delete
#' @export
pin_delete.pins_board_labkey <- function(board, names, ...) {
  for (name in names) {
    pins:::check_pin_exists(board, name)
    labkey_check_permissions(folder = board$folder, subdir = board$subdir, permission_to_check = "canDelete")
    Rlabkey::labkey.webdav.delete(
      baseUrl = board$base_url,
      folderPath = board$folder,
      remoteFilePath = fs::path(board$subdir, name),
      fileSet = "@files"
    )
  }
  invisible(board)
}


#' @importFrom pins pin_version_delete
#' @export
pin_version_delete.pins_board_labkey <- function(board, name, version, ...) {
  labkey_check_permissions(folder = board$folder, subdir = board$subdir, permission_to_check = "canDelete")
  Rlabkey::labkey.webdav.delete(
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = fs::path(board$subdir, name, version),
    fileSet = "@files"
  )
}


#' @importFrom pins pin_versions
#' @export
pin_versions.pins_board_labkey <- function(board, name, ...) {
  pins:::check_pin_exists(board, name)

  resp <- Rlabkey::labkey.webdav.listDir(
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = fs::path(board$subdir, name),
    fileSet = "@files"
  )

  final_list <- resp$files

  paths <- fs::path_file(map_chr(final_list, ~ .$id))
  pins:::version_from_path(paths)
}


#' @importFrom pins pin_meta
#' @export
pin_meta.pins_board_labkey <- function(board, name, version = NULL, ...) {
  pins:::check_pin_exists(board, name)
  version <- pins:::check_pin_version(board, name, version)
  metadata_key <- fs::path(name, version, "data.txt")

  # this should be helpful but not necessary
  key_exists <- Rlabkey::labkey.webdav.pathExists(
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = fs::path(board$subdir, metadata_key),
  )
  if (!key_exists) {
    pins:::abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  labkey_download(board, metadata_key)

  pins:::local_meta(
    pins:::read_meta(fs::path(board$cache, name, version)),
    name = name,
    dir = path_version,
    version = version
  )
}

#' @importFrom pins pin_fetch
#' @export
pin_fetch.pins_board_labkey <- function(board, name, version = NULL, ...) {
  meta <- pins::pin_meta(board, name, version = version)
  pins:::cache_touch(board, meta) ## note this changes to read only, which is expected

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    labkey_download(board, key)
  }

  meta
}

#' @importFrom pins pin_store
#' @export
pin_store.pins_board_labkey <- function(board, name, paths, metadata,
                                        versioned = NULL, x = NULL, ...) {
  ellipsis::check_dots_used()
  pins:::check_pin_name(name)
  # version name is timestamp + first 5 chr of hash
  version <- pins:::version_setup(board, name, pins:::version_name(metadata), versioned = versioned)

  version_dir <- fs::path(name, version)
  # write data.txt to tmp file
  yaml_path <- fs::path_temp("data.txt")
  yaml::write_yaml(x = metadata, file = yaml_path)
  withr::defer(fs::file_delete(yaml_path))
  labkey_check_permissions(folder = board$folder, subdir = board$subdir, permission_to_check = "canUpload")
  Rlabkey::labkey.webdav.put(
    localFile = yaml_path,
    baseUrl = board$base_url,
    folderPath = board$folder,
    remoteFilePath = fs::path(board$subdir, version_dir, "data.txt")
  )
  for (path in paths) {
    Rlabkey::labkey.webdav.put(
      localFile = path,
      baseUrl = board$base_url,
      folderPath = board$folder,
      remoteFilePath = fs::path(board$subdir, version_dir, fs::path_file(path))
    )
  }

  name
}

labkey_download <- function(board, key) {
  path <- fs::path(board$cache, key)
  if (!fs::file_exists(path)) {
    Rlabkey::labkey.webdav.get(
      baseUrl = board$base_url,
      folderPath = board$folder,
      remoteFilePath = fs::path(board$subdir, key),
      localFilePath = path
    )
    fs::file_chmod(path, "u=r")
  }
  path
}

labkey_check_permissions <- function(folder, subdir, permission_to_check = "canUpload") {
  checks <- match.arg(
    arg = permission_to_check,
    choices = c(
      "canRead", "canUpload", "canEdit", "canRename",
      "canDelete"
    ),
    several.ok = F
  )
  tryCatch(
    {
      resp <- Rlabkey::labkey.webdav.listDir(
        folderPath = folder,
        remoteFilePath = subdir,
        fileSet = "@files"
      )
      if (resp$permissions[[checks]]) {
        return(TRUE)
      } else {
        warning(
          paste(
            "Invalid LabKey permissions: missing", tolower(gsub("can", "", permission_to_check)),
            "permissions for this action. Check credentials using Rlabkey::labkey.whoAmI() and try again."
          ),
          call. = F
        )
      }
    },
    # Still throw an error but add a more helpful message
    error = function(cond) {
      stop(paste(conditionMessage(cond),
                 "Unable to list permissions for LabKey board. Check credentials using Rlabkey::labkey.whoAmI() and try again. ",
                 sep = "\n"),
        call. = F
      )
      return(NA)
    }
  )
}

#' @export
pins::pin_read

#' @export
pins::pin_write

#' @export
pins::pin_versions

#' @export
pins::pin_list

#' @export
pins::pin_exists

#' @export
pins::pin_version_delete

#' @export
pins::pin_delete
