#' Sending map the data.
#'
#' This function send map object the data which are mapped and change map object to data.frame object.
#'
#' @param map map
#' @param data data mapped
#' @param map_merge_key key in map object used to link map object and data.
#' @param map_key key used in map object.
#' @param data_key kep in data used to link map object and data.
#' @example
#'  map_f <- data_to_map(map, data,map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong")
#' @return
#' @export
#' @importFrom ggplot2 fortify
#' @importFrom dplyr left_join

data_to_map <- function(map, data, map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong"){
  map@data$id <- rownames(map@data)
  map$id <- iconv(map$id, "euc-kr", "UTF-8")
  map_f <- fortify(map, region = "id")

  merge_data <- merge(map@data[, c(map_key, map_merge_key)], data, by.x=map_merge_key, by.y = data_key)

  map@data <- left_join(map@data, merge_data, by = map_key)
  map_f_d <- left_join(map_f, map@data, by = "id")
  return(map_f_d)
}

#' plot the map
#'
#' This function plot choropleth.
#'
#' @param map map having data(data.frame)
#' @param re_var variable mapped
#' @param index_type 1 or 2
#' @param path_color color of map boundary
#' @param legend_label label of legend
#' @param color_type blue_red or viridis
#' @param color_good
#' @param color_bad
#' @param fixed_mid
#' @example
#'  plot_map(map_f, re_var = "c_rate", index_type = 1, legend_label = "조율(%)")
#' @return
#' @export
#' @importFrom ggplot2 ggplot
#'


plot_map <- function(map , re_var, index_type,
                     path_color = "white", legend_label,
                     color_type = "blue_red", color_good = "blue", color_bad = "red", fixed_mid = NULL) {

  map$re_var <- map[[re_var]]

  color <- c(color_bad, color_good)
  midpoint<- median(map$re_var)
  range_rate <- range(map$re_var)

  if (index_type != 1) {
    color <- c(color_good, color_bad)
    index_type <- -1
  }
  if(!is.null(fixed_mid)) {
    midpoint <- fixed_mid
  }

  return (
    ggplot(map, aes(long, lat, group = group, fill = re_var)) +
      geom_polygon() +
      geom_path(color = path_color) +
      coord_equal() +
      labs(x="", y="", fill = legend_label) +
      scale_x_continuous(labels=NULL) +
      scale_y_continuous(labels = NULL) +
      theme_bw() +
      if (color_type == "viridis") {
        scale_fill_viridis(direction = index_type)
      } else if (color_type == "blue_red") {
        scale_fill_gradient2(low = color[1], mid ="white", midpoint = midpoint,
                             high = color[2], limits = c(range_rate[1], range_rate[2]))
      }

  )

}
