
buda_url <- 'https://www.buda.com/api/v2'
req <- httr2::request(buda_url)


#' Buda API Available Markets
#'
#' @param market_id A character string with valid market id
#'
#' @return A tibble with a row for each available market on the exchange, if a
#' market id is defined the tibble will have a single row with the information
#' of the selected market id.
#' @export
#'
#' @examples
#' buda_markets <- markets()
#' BTC_CLP_market <- markets("BTC-CLP")
markets <- function(market_id = NULL){
  resp <- req |>
    httr2::req_url_path_append("markets") |>
    httr2::req_perform()
  resp_json <- resp |>
    httr2::resp_body_json()
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
    return(df_out)
  }else{
    if(sum(market_id %in% df_out$id)==1){
      return(df_out |> dplyr::filter(id == market_id))
    }else{
      stop("Please provide a correct market_id, use markets() without any parameter to see the possible market ids",
           call. = FALSE)
    }
  }
}
