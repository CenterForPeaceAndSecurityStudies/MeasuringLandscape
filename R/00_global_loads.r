


global_loads <- function() {

  # !diagnostics off
  gc()
  # sudo dnf install libcurl-devel
  # install.packages('devtools', dependencies=T)
  library(devtools)
  devtools::session_info("DT")
  if (!require(pacman)) {
    install.packages("pacman", dependencies = T)
    library(pacman)
  }
  p_load(mosaic, stringr, stringi)
  p_load(lubridate, stringr)
  p_load(janitor)
  p_load(digest)
  p_load(tidyverse, dplyr, knitr, DT, magrittr) # install.packages('DT', repos = 'http://cran.rstudio.com')
  p_load(rgeos) # yum install -y geos-devel
  p_load(rgdal) # dnf install gdal* , sudo yum install proj*
  p_load(digest)
  p_load(ggmap) # sudo dnf install libjpeg*
  p_load(data.table)
  p_load(bookdown)
  # p_load(feather)
  p_load(stringdist)
  p_load(sf)
  p_load(tidyverse)
  p_load(viridis)
  p_load(rvest)
  p_load(tidyverse)
  p_load(re2r)

  p_load(plyr)

  p_load(mosaic)
  p_load(lubridate, stringr)
  p_load(janitor)
  p_load(digest)
  p_load(tidyverse, dplyr, knitr, DT) # install.packages('DT', repos = 'http://cran.rstudio.com')

  # Need the development version for geom_sf
  # devtools::install_github("tidyverse/ggplot2")
  # This is much slower than other plotting I've been doing. Not great.
  # library(ggplot2)
  # flatfiles_sf_roi %>% ggplot() +
  #  geom_sf(size=.1) +
  #  #scale_fill_viridis("Area") +
  #  ggtitle("Gazeteer Points (All)") +
  #  theme_bw()

  # devtools::load_all(".")
}

# global_loads()
