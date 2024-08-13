
buda_url <- 'https://www.buda.com/api/v2'
req <- httr2::request(buda_url)

market_check <- function(market_id){
  available_markets <- markets() |>
    dplyr::pull(id)
  return(market_id %in% available_markets)
}

#' Buda API Available Markets
#'
#' @description
#'
#' `markets()` produces a `tibble` with the available markets information on Buda
#' cripto-exchange, the column names are kept the same as received from the
#' exchange.
#'
#' Detailed descriptions about fields and Buda REST API can be found in
#' \link{https://api.buda.com}
#'
#' @param market_id A character string with valid market id (optional)
#'
#' @return A tibble with a row for each available market on the exchange, if a
#' market id is defined the tibble will have a single row with the information
#' of the selected market id.
#'
#' @examples
#' buda_markets <- markets()
#' BTC_CLP_market <- markets("BTC-CLP")
#'
#' @export
markets <- function(market_id = NULL){
  resp <- req |>
    httr2::req_url_path_append("markets") |>
    httr2::req_perform()
  resp_json <- resp |>
    httr2::resp_body_json()
  # code <- resp |>
  #   httr2::resp_check_status()
  # status <- resp |>
  #   httr2::resp_status_desc()
  I <- length(resp_json$markets)
  for(i in 1:I){
    J <- length(resp_json$markets[[i]])
    for(j in 1:J){
      if(is.null(resp_json$markets[[i]][[j]])){
        resp_json$markets[[i]][[j]] <- FALSE
      }
      if(is.list(resp_json$markets[[i]][[j]])){
        resp_json$markets[[i]][[j]] <- paste(resp_json$markets[[i]][[j]], collapse = " ")
      }
    }
  }
  df_out <- purrr::map_df(resp_json$markets, \(x) tibble::as_tibble_row(x))
  if(is.null(market_id)){
    # message(paste("Markets retrieve with status:", code, status))
    return(df_out)
  }else{
    if(sum(market_id %in% df_out$id)==1){
      # message(paste("Market id", market_id, "done with status:", code, status))
      return(df_out |> dplyr::filter(id == market_id))
    }else{
      stop("Please provide a correct market_id, use markets() without any argument to see the possible market ids",
           call. = FALSE)
    }
  }
}


#' Buda API Transactions Volume of a Market
#'
#' Transactions volume for an specified `market_id`. Details regarding
#' descriptions about fields and Buda REST API can be found in
#' [https://api.buda.com/#rest-api-llamadas-publicas-volumen-transado]
#'
#' @param market_id A character string with valid market id (required)
#'
#' @return A tibble row with transaction volumes information for the specified market id.
#' @export
#'
#' @examples
#' BTC_COP_volume <- volume("BTC-COP")
#'
#' # Volumes for all markets available
#' market_ids <- markets() |> dplyr::pull(id)
#' volumes <- purrr::map_df(market_ids, volume)
volume <- function(market_id){
  available_markets <- markets() |>
    dplyr::pull(id)
  if(!(market_id %in% available_markets)){
    stop("Please provide a correct market_id, use markets() without any argument to see the possible market ids",
         call. = FALSE)
  }
  resp <- req |>
    httr2::req_url_path_append("markets") |>
    httr2::req_url_path_append(market_id) |>
    httr2::req_url_path_append("volume") |>
    httr2::req_perform()
  resp_json <- resp |>
    httr2::resp_body_json()
  # status <- httr2::last_response() |>
  #   httr2::resp_body_json()
  I <- length(resp_json$volume)
  for(i in 1:I){
    if(is.list(resp_json$volume[[i]])){
      resp_json$volume[[i]] <- paste(resp_json$volume[[i]], collapse = " ")
    }
  }
  df_out <- tibble::as_tibble_row(resp_json$volume)
  print(paste("Volume for market id", market_id, "done"))
  return(df_out)
}

#' Buda API Ticker of a Market
#'
#' A ticker allows to know the current status of a specific `market_id`. Details
#' regarding descriptions about fields and Buda REST API can be found in
#' [https://api.buda.com/#rest-api-llamadas-publicas-volumen-transado]
#'
#' @param market_id A character string with valid market id (required)
#'
#' @return A tibble row with ticker for the specified market id.
#' @export
#'
#' @examples
#' BTC_COP_ticker <- ticker("BTC-COP")
ticker <- function(market_id){
  if(!market_check(market_id)){
    stop("Please provide a correct market_id, use markets() without any argument to see the possible market ids",
         call. = FALSE)
  }
  resp <- req |>
    httr2::req_url_path_append("markets") |>
    httr2::req_url_path_append(market_id) |>
    httr2::req_url_path_append("ticker") |>
    httr2::req_perform()
  resp_json <- resp |>
    httr2::resp_body_json()
  I <- length(resp_json$ticker)
  for(i in 1:I){
    if(is.list(resp_json$ticker[[i]])){
      resp_json$ticker[[i]] <- paste(resp_json$ticker[[i]], collapse = " ")
    }
  }
  df_out <- tibble::as_tibble_row(resp_json$ticker)
  print(paste("Ticker for market id", market_id, "done"))
  return(df_out)
}
