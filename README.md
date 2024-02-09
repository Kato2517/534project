# 534project

# BC Ferries Capacity API Client

This R package provides a set of functions to interact with the BC Ferries API, enabling users to fetch and visualize the current capacity and sailing information for various routes offered by BC Ferries.

## Features

- Fetch real-time ferries data from the BC Ferries API.
- Retrieve and display ferry route information.
- Combine sailings data with route information.
- Retrieve capacity information for specific ferry routes.
- Generate a capacity plot for sailings with fill percentages for different vehicle types.

## Installation

- You can download the R package file on your desktop, and install it in your R:

```r
> setwd("~/Desktop")
> devtools::install("BCFerriesConnector")
```

## Usage
- Load the package and use the functions as follows:

```r
>library(bcferriescapacity)

# Fetching API data
> api_data <- get_data()
> print(api_data)

# Getting route information
> route_info <- get_routes()
> print(route_info)

# Fetching combined sailings and route information
> combined_sailings_df <- get_combined_sailings_df()
> head(combined_sailings_df)

# Retrieve schedule information for a specific ferry route
schedule_info <- get_schedule("TSASWB")
print(schedule_info)

# Retrieve capacity information for a given ferry route
> capacity_info <- get_capacity("TSASWB")
> print(capacity_info)

# Generate capacity plot for sailings
> sailings_data <- get_capacity("TSASWB")
> capacity_plot = get_capacity_plot(sailings_data)
> print(capacity_plot)
```

## Acknowledgments
- Thanks to BC Ferries for providing the API that made this package possible.
- Contributions from the MDS community are always appreciated.
