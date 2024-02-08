library(httr)
library(jsonlite)
library(dplyr)
library(purrr)


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
#' @import httr
#' @import jsonlite
#'
#' @export
#'
#' @examples
#' api_data = get_data()
#' print(api_data)

get_data <- function() {
  response <- GET("https://www.bcferriesapi.ca/v2/capacity/")

  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    return(data)
  } else {
    stop("Failed to retrieve data")
  }
}


#' Combine Sailings Data with Route Information
#'
#' This function fetches data from the BC Ferries API and processes it to
#' combine sailings data with corresponding route information. It extracts
#' specific details such as route codes, terminal codes,sailing duration,
#' and other sailings information like time, arrival time, sailing status,
#' and vessel name.
#'
#' @return
#' A data frame that combines sailings data with route information. Each row
#' represents a specific sailing, including details such as route code, from
#' and to terminal codes, sailing duration, time, arrival time, sailing status,
#' vessel name, fill percentages for cars and oversize vehicles.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#'
#' @export
#'
#' @examples
#' combined_sailings_df <- get_combined_sailings_df()
#' head(combined_sailings_df)

get_combined_sailings_df <- function() {
  api_response <- get_data()
  routes_df <- api_response$routes

  combined_sailings_df <- map2_df(
    routes_df$sailings,
    1:nrow(routes_df),
    ~ {
      current_route_info <- routes_df[.y, c("routeCode", "fromTerminalCode", "toTerminalCode", "sailingDuration")]
      current_sailings_info <- .x %>%
        select(time, arrivalTime, sailingStatus, vesselName, fill, carFill, oversizeFill)
      bind_cols(current_route_info, current_sailings_info)
    }
  )

  return(combined_sailings_df)
}


#' Retrieve Capacity Information for a Given Ferry Route
#'
#' This function fetches the capacity information for a specific ferry route
#' from the combined sailings data frame. It filters the data by the given
#' route code and selects relevant columns, including route code, time of
#' sailing, sailing status, vessel name, and fill percentages for cars, and
#' oversize vehicles.
#'
#' @param routeCode
#' A character string specifying the route code for which capacity information is desired.
#'
#' @return
#' A data frame containing the capacity information for the specified route.
#' The data frame includes columns for route code, time of sailing, sailing
#' status, vessel name, and fill percentages for cars and oversize vehicles.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#'
#' @export
#'
#' @examples
#' capacity_info <- get_capacity("TSASWB")
#' print(capacity_info)

get_capacity <- function(routeCode) {
  combined_sailings_df <- get_combined_sailings_df()  # 调用函数获取数据框

  capacity<- combined_sailings_df %>%
    filter(routeCode == !!routeCode) %>%
    select(routeCode, time, sailingStatus, vesselName, fill, carFill, oversizeFill)

  return(capacity)
}

