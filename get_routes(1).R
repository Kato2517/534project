library(httr)
library(jsonlite)

#' Fetch Ferries Data from BC Ferries API
#'
#' This function is designed to interact with the BC Ferries API,
#' specifically targeting the capacity endpoint. It sends a GET request to the
#' API and if successful, parses the returned JSON content into an R-readable
#' format. The function is robust against failed requests, providing an error
#' message if the data retrieval is unsuccessful.
#'
#' @return
#' A list or data frame containing the ferries data, if the request is
#' successful. If the request fails, the function stops with an error message.
#'
#' @export
#'
#' @examples
#' get_data()

get_data <- function() {
  response <- GET("https://www.bcferriesapi.ca/v2/capacity/")

  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    return(data)
  } else {
    stop("Failed to retrieve data")
  }
}


#' Showing Ferry Route Information
#'
#' This function obtains ferry route information.
#'
#' @return
#' A df contains ferry routes information: routeCode, fromTerminalCode
#' and to TerminalCode.
#'
#' @export
#'
#' @examples
#' get_routes()

get_routes <- function() {
  api_response <- get_data()
  routes_df <- api_response$routes
  code_info <- routes_df[, c("routeCode", "fromTerminalCode", "toTerminalCode")]

  return(code_info)
}


#get_routes()
#function1 = get_routes()





# terminal_codes <- data.frame(
#   Code = c("TSA", "SWB", "SGI", "DUK", "FUL", "HSB", "NAN", "LNG", "BOW"),
#   Name = c("Tsawwassen",
#            "Swartz Bay",
#            "Southern Gulf Islands",
#            "Duke Point (Nanaimo)",
#            "Fulford Harbour (Salt Spring Island)",
#            "Horseshoe Bay",
#            "Departure Bay (Nanaimo)",
#            "Langdale",
#            "Bowen Island"),
#   stringsAsFactors = FALSE
# )
#
# translate_code <- function() {
#   print(terminal_codes)
# }
#
# translate_code()
