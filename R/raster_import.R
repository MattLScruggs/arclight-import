library(terra)
library(sf)
library(httr2)

#' Fetch and Load Raster Data from an API Endpoint
#'
#' @description Queries a raster data API placeholder, writes the output to a temporary
#'   file, and reads it into memory/disk as a SpatRaster object.
#'
#' @param base_url Character string. Base API endpoint URL.
#' @param api_key Optional character string for authentication headers.
#' @param query_params Named list of query parameters (e.g., product, resolution, bbox).
#' @param dest_dir Character string. Local directory to store retrieved raster files.
#'
#' @return A SpatRaster object representing the downloaded layers.
#' @export


fetch_raster_layer <- function(base_url,
                               api_key = NULL,
                               query_params = list(),
                               dest_dir = tempdir()) {
  # Build request pipeline with retry logic
  req <- httr2::request(base_url) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 2)
  
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, "Authorization" = paste("Bearer", api_key))
  }
  
  # Generate deterministic destination filename
  dest_file <- file.path(dest_dir, paste0("raster_", as.numeric(Sys.time()), ".tif"))
  
  # Perform download
  resp <- tryCatch({
    httr2::req_perform(req, path = dest_file)
  }, error = function(e) {
    stop("API request failed: ", e$message, call. = FALSE)
  })
  
  # Ingest into terra SpatRaster
  raster_obj <- terra::rast(dest_file)
  return(raster_obj)
}

#' Extract Raster Attributes for Specific Spatial Features and Timeframes
#'
#' @description Subsets a multi-layer SpatRaster based on temporal layer names
#'   or time attributes, aligns coordinate reference systems, and extracts values 
#'   at specified locations.
#'
#' @param raster_data A SpatRaster object.
#' @param locations An sf object representing points or polygons.
#' @param start_date Character string or Date (e.g., "2024-01-01").
#' @param end_date Character string or Date (e.g., "2024-01-31").
#' @param time_indices Optional vector of layer indices if time metadata is not embedded in the raster.
#'
#' @return A data.frame containing the extracted raster attributes merged with location IDs.
#' @export
extract_raster_attributes <- function(raster_data,
                                      locations,
                                      start_date = NULL,
                                      end_date = NULL,
                                      time_indices = NULL) {
  # 1. Coordinate Reference System (CRS) Alignment
  if (sf::st_crs(locations) != sf::st_crs(terra::crs(raster_data))) {
    locations <- sf::st_transform(locations, crs = terra::crs(raster_data))
  }
  
  # 2. Temporal Subsetting Logic
  # Check if terra time metadata is natively available
  raster_times <- terra::time(raster_data)
  
  if (!all(is.na(raster_times)) && !is.null(start_date) && !is.null(end_date)) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    selected_layers <- which(raster_times >= start_date & raster_times <= end_date)
    
    if (length(selected_layers) == 0) {
      stop("No raster layers matched the specified date window.", call. = FALSE)
    }
    raster_data <- raster_data[[selected_layers]]
  } else if (!is.null(time_indices)) {
    # Manual band/layer index fallback
    raster_data <- raster_data[[time_indices]]
  }
  
  # 3. Spatial Extraction
  # Convert sf vector features to terra SpatVector for optimized C++ extraction
  spat_locs <- terra::vect(locations)
  extracted_vals <- terra::extract(raster_data, spat_locs, bind = TRUE)
  
  # 4. Return as standard tabular data frame
  return(as.data.frame(extracted_vals))
}