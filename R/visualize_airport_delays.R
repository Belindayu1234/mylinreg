#' Visualize airport delays
#'
#' @return ggplot plot object
#' @importFrom dplyr group_by summarise inner_join
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export
visualize_airport_delays <- function(){

  if(!requireNamespace("nycflights13", quietly = TRUE)) stop("install nycflights13")
  if(!requireNamespace("dplyr", quietly = TRUE)) stop("install dplyr")
  if(!requireNamespace("ggplot2", quietly = TRUE)) stop("install ggplot2")

  flights <- nycflights13::flights
  airports <- nycflights13::airports

  df <- flights |>
    dplyr::group_by(dest) |>
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) |>
    dplyr::inner_join(airports, by = c("dest" = "faa"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = lon, y = lat, colour = mean_delay)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Mean arrival delay per airport",
                  x="Longitude", y="Latitude", color = "Mean Arrival Delay")
  return(p)
}

#' @keywords internal
utils::globalVariables(c("dest","arr_delay","lon","lat","mean_delay"))
