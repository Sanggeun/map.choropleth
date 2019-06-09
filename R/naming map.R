#' naming choropleth.
#'
#' This function calculate means latitude and longitude in choropleth.
#'
#' @param map map object
#' @param name_var the variable of name mapped in map object
#' @param name name mapped if map object don't have name variable
#' @example
#'  map_f <- data_to_map(map, data,map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong")
#' @return
#' @export
#' @importFrom plyr ddply
#' @importFrom ggplot2 fortify
#'

name_in_map <- function(map, name_var, name = NULL) {

  map@data$id <- rownames(map@data)
  map_f <- fortify(map, region = "id")
  # 중심좌표를 계산
  distcenters <- ddply(map_f, .(id), summarize, clat = mean(lat), clong = mean(long))
  # 읍면동 이름 붙이기
  if (is.null(name)) {
    naming <- map@data[[name_var]]
  }
  if (!is.null(name)) {
    naming <- name
  }

  names(naming) <- map@data[["id"]]
  distcenters$name <- naming[distcenters$id]
  return(distcenters)
}
