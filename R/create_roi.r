
create_roi <- function(bottom_left_x,
                       bottom_left_y,
                       top_right_x,
                       top_right_y,
                       crs_in=4326,
                       crs_out="+proj=utm +zone=37 +datum=WGS84") {
  region_of_interest_sf <- sf::st_sfc(sf::st_polygon(list(
    rbind(
      c(bottom_left_x, bottom_left_y),
      c(top_right_x, bottom_left_y),
      c(top_right_x, top_right_y),
      c(bottom_left_x, top_right_y),
      c(bottom_left_x, bottom_left_y)
    )
  )))

  # region_of_interest_sf <-st_sfc(st_polygon(list(
  #   rbind(c(35.67,-1.43285),
  #         c(38.19,-1.43285),
  #         c(38.19,0.54543),
  #         c(35.67,0.54543),
  #         c(35.67,-1.43285)
  #   )
  # )
  # )
  # )
  region_of_interest_sf <- sf::st_set_crs(region_of_interest_sf, crs_in)
  region_of_interest_sf_utm <- sf::st_transform(region_of_interest_sf, crs = crs_out)
  return(region_of_interest_sf_utm)
}

# region_of_interest_sf_utm <- create_roi(bottom_left_x=35.67,
#                                        bottom_left_y=-1.43285,
#                                        top_right_x=38.19,
#                                        top_right_y=0.54543)
