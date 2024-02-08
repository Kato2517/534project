library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)


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


#' Generate Capacity Plot for Sailings
#'
#' This function takes a dataset of sailings and generates a bar plot showing
#' the fill percentages for cars and oversize vehicles for current and future
#' sailings, distinguished by time. The plot uses different colors to represent
#' different vehicle types and displays the fill percentage across various times
#' of the day.
#'
#'
#' @param sailings_data
#' A dataframe containing at least four columns: `sailingStatus`, `time`,
#' `carFill`, and `oversizeFill`. `sailingStatus` should indicate whether
#' a sailing is current or future, `time` should represent the time of each
#' sailing, and `carFill` and `oversizeFill`should contain fill percentages
#' for cars and oversize vehicles, respectively.
#'
#' @return
#' A ggplot object representing the fill percentages of vehicles for current
#' and future sailings. The plot is color-coded to distinguish between car and
#' oversize vehicle fill percentages.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' sallings_data = get_capacity("TSASWB")
#' get_capacity_plot(sallings_data)

get_capacity_plot <- function(sailings_data) {
  current_future_sailings <- sailings_data %>%
    filter(sailingStatus %in% c("current", "future")) %>%
    mutate(time = factor(time, levels = unique(time)))

  current_future_sailings_long <- current_future_sailings %>%
    gather(key = "VehicleType", value = "FillPercentage", carFill, oversizeFill)

  ggplot(current_future_sailings_long, aes(x = time, y = FillPercentage, fill = VehicleType)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +  # Set bar width here
    scale_fill_manual(values = c("carFill" = "orange", "oversizeFill" = "blue")) +
    labs(x = "Time", y = "Fill Percentage", fill = "Vehicle Type", title = "Current and Future Sailings Fill Percentage") +
    theme_minimal() +
    theme(legend.position = "top")
}



