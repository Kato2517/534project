% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_capacity.R, R/get_capacity_plot.R
\name{get_capacity}
\alias{get_capacity}
\title{Retrieve Capacity Information for a Given Ferry Route}
\usage{
get_capacity(routeCode)

get_capacity(routeCode)
}
\arguments{
\item{routeCode}{A character string specifying the route code for which capacity information is desired.}
}
\value{
A data frame containing the capacity information for the specified route.
The data frame includes columns for route code, time of sailing, sailing
status, vessel name, and fill percentages for cars and oversize vehicles.

A data frame containing the capacity information for the specified route.
The data frame includes columns for route code, time of sailing, sailing
status, vessel name, and fill percentages for cars and oversize vehicles.
}
\description{
This function fetches the capacity information for a specific ferry route
from the combined sailings data frame. It filters the data by the given
route code and selects relevant columns, including route code, time of
sailing, sailing status, vessel name, and fill percentages for cars, and
oversize vehicles.

This function fetches the capacity information for a specific ferry route
from the combined sailings data frame. It filters the data by the given
route code and selects relevant columns, including route code, time of
sailing, sailing status, vessel name, and fill percentages for cars, and
oversize vehicles.
}
\examples{
capacity_info <- get_capacity("TSASWB")
print(capacity_info)
capacity_info <- get_capacity("TSASWB")
print(capacity_info)
}
