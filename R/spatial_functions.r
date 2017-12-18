
library(pacman)

# This function strips out a few special characters that kept appearing
# I placed it here because it breaks Rstudio's parsing of rmarkdown files
clean_noascii <- function(text) {

  # Further verify what weird characters got by me
  text <- gsub("–", "", text, fixed = T)
  text <- gsub("+", "", text, fixed = T)
  text <- gsub("<", "", text, fixed = T)
  text <- gsub("=", "", text, fixed = T)
  text <- gsub(">", "", text, fixed = T)
  text <- gsub("|", "", text, fixed = T)
  text <- gsub("🇪", "", text, fixed = T)
  text <- gsub("🇰", "", text, fixed = T)
  text <- gsub("$", "", text, fixed = T)

  return(text)
}

common_cleaning <- function(q) {
  q %>%
    janitor::clean_names() %>%
    janitor::remove_empty_rows() %>%
    janitor::remove_empty_cols() %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, funs(stringi::stri_enc_toascii)) %>%
    mutate_if(is.character, funs(gsub("\032", "", .))) %>% # converts everything to character and proper UTF8
    mutate_if(is.character, funs(trimws)) %>% # remove whitespace
    distinct() %>%
    return()
}

# Until bind_row works with SF objects, have to roll my own
rbind_sf <- function(a, b) {
  names_a <- names(a)
  names_b <- names(b)
  cat(" ", names_a, ";", names_b, ";")
  try({
    a[setdiff(names_b, names_a)] <- NA
  })
  try({
    b[setdiff(names_a, names_b)] <- NA
  })
  newnames <- sort(intersect(names(a), names(b)))

  return(rbind(a[, newnames], b[, newnames]))
}


stlusterify <- function(coords, clusters) {
  hull_list <- list()
  for (i in na.omit(unique(clusters))) {
    print(i)
    try({
      hull_list[[i]] <- to_hull_polygon(coords[clusters == i], id = i)
    })
  }
  hulls_df <- rbindlist(lapply(hull_list, tidy))
  return(list(hull_list = hull_list, hulls_df = hulls_df))
}

to_hull_polygon <- function(dat, id) {
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ] # closed polygon
  library("sp")
  library("rgdal")
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = id)))
  return(sp_poly)
}


to_polygon_df <- function(coords, ID=1, df=NULL) {
  rownames(df) <- ID
  coords <- rbind(coords, coords[1, ])
  coords <- data.matrix(data.frame(lapply(coords, FUN = function(x) as.numeric(as.character(x))))) # double check numeric
  library(sp)
  p <- Polygon(coords)
  ps <- Polygons(list(p), ID)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  sps_df <- SpatialPolygonsDataFrame(sps, data = df)

  return(sps_df)
}

p_load(aspace)
to_sde <- function(coords, id=1) {
  # taken entirely from aspace::calc_sde
  points <- coords

  centre.xy <- NULL
  calccentre <- TRUE
  weighted <- FALSE
  weights <- NULL

  errorcode <- 1000
  if (length(dim(points)) != 2) {
    errorcode <- 61
    cat("\\n\\nWARNING: Provided points input matrix has fewer than 2 columns.")
    cat("\\nERROR CODE: ", errorcode, "\\n\\n", sep = "")
    return("ERROR")
  }
  if (dim(points)[2] != 2) {
    errorcode <- 60
    cat("\\n\\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
    cat("\\nERROR CODE: ", errorcode, "\\n\\n", sep = "")
    return("ERROR")
  } else {
    n <- dim(points)[1]
    if (calccentre) {
      if (length(centre.xy) == 2) {
        errorcode <- 21
        cat("\\n\\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
        cat("\\nERROR CODE: ", errorcode, "\\n\\n", sep = "")
        return("ERROR")
      } else {
        if (weighted) {
          wt.x <- points[, 1] * weights
          wt.y <- points[, 2] * weights
          WMC.x <- c(sum(wt.x) / sum(weights))
          WMC.y <- c(sum(wt.y) / sum(weights))
          centre.xy[1] <- WMC.x
          centre.xy[2] <- WMC.y
        } else {
          meanx <- sum(points[, 1]) / n
          meany <- sum(points[, 2]) / n
          centre.xy[1] <- meanx
          centre.xy[2] <- meany
        }
      }
    }
  }
  points <- cbind(points, points[, 1] ^ 2, points[, 2] ^ 2)
  points <- cbind(points, points[, 1] - centre.xy[1], points[
    ,
    2
  ] - centre.xy[2])
  points <- cbind(points, points[, 5] ^ 2, points[, 6] ^ 2, points[
    ,
    5
  ] * points[, 6])
  names(points) <- c(
    "x", "y", "x2", "y2", "x'", "y'", "x'2",
    "y'2", "x'y'"
  )
  if (weighted) {
    top1 <- sum(weights * points[, 7]) - sum(weights * points[
      ,
      8
    ])
    top2 <- sqrt((sum(weights * points[, 7]) - sum(weights *
      points[, 8])) ^ 2 + 4 * (sum(weights * points[, 9])) ^ 2)
    bottom <- (2 * sum(weights * points[, 9]))
    tantheta <- (top1 + top2) / bottom
  } else {
    top1 <- sum(points[, 7]) - sum(points[, 8])
    top2 <- sqrt((sum(points[, 7]) - sum(points[, 8])) ^ 2 +
      4 * (sum(points[, 9])) ^ 2)
    bottom <- (2 * sum(points[, 9]))
    tantheta <- (top1 + top2) / bottom
  }
  if (tantheta < 0) {
    theta <- 180 + (atan_d(tantheta))
  } else {
    theta <- atan_d(tantheta)
  }
  sintheta <- sin_d(theta)
  costheta <- cos_d(theta)
  sin2theta <- sintheta ^ 2
  cos2theta <- costheta ^ 2
  sinthetacostheta <- sintheta * costheta
  if (weighted) {
    sigmax <- sqrt(2) * sqrt(((sum(weights * points[, 7])) *
      (cos2theta) - 2 * (sum(weights * points[, 9])) *
        (sinthetacostheta) + (sum(weights * points[, 8])) *
        (sin2theta)) / ((sum(weights)) - 2))
    sigmay <- sqrt(2) * sqrt(((sum(weights * points[, 7])) *
      (sin2theta) + 2 * (sum(weights * points[, 9])) *
        (sinthetacostheta) + (sum(weights * points[, 8])) *
        (cos2theta)) / ((sum(weights)) - 2))
  } else {
    sigmax <- sqrt(2) * sqrt(((sum(points[, 7])) * (cos2theta) -
      2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[
        ,
        8
      ])) * (sin2theta)) / (n - 2))
    sigmay <- sqrt(2) * sqrt(((sum(points[, 7])) * (sin2theta) +
      2 * (sum(points[, 9])) * (sinthetacostheta) + (sum(points[
        ,
        8
      ])) * (cos2theta)) / (n - 2))
  }
  if (sigmax > sigmay) {
    Major <- "SigmaX"
    Minor <- "SigmaY"
  } else {
    Major <- "SigmaY"
    Minor <- "SigmaX"
  }
  lengthsigmax <- 2 * sigmax
  lengthsigmay <- 2 * sigmay
  areaSDE <- pi * sigmax * sigmay
  eccentricity <- sqrt(1 - ((min(sigmax, sigmay) ^ 2) / (max(
    sigmax,
    sigmay
  ) ^ 2)))
  B <- min(sigmax, sigmay)
  A <- max(sigmax, sigmay)
  d2 <- (A - B) * (A + B)
  phi <- 2 * pi * seq(0, 1, len = 360)
  sp <- sin(phi)
  cp <- cos(phi)
  r <- sigmax * sigmay / sqrt(B ^ 2 + d2 * sp ^ 2)
  xy <- r * cbind(cp, sp)
  al <- (90 - theta) * pi / 180
  ca <- cos(al)
  sa <- sin(al)
  coordsSDE <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(
    centre.xy[1],
    360
  ), rep(centre.xy[2], 360))

  if (sigmax < sigmay) {
    Theta.Corr <- theta
  } else {
    Theta.Corr <- theta + 90
  }

  r.SDE <- list(
    id = id, points = points, coordsSDE = coordsSDE,
    calccentre = calccentre, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2],
    Major = Major, Minor = Minor, theta = theta, Sigma.x = sigmax,
    Sigma.y = sigmay, Eccentricity = eccentricity, Area.sde = areaSDE,
    TanTheta = tantheta, SinTheta = sintheta, CosTheta = costheta,
    SinThetaCosTheta = sinthetacostheta, Sin2Theta = sin2theta,
    Cos2Theta = cos2theta, ThetaCorr = Theta.Corr, weighted = weighted,
    weights = weights
  )
  # assign("r.SDE", r.SDE, pos = 1)
  result.sde <- list(
    id = id, CALCCENTRE = calccentre, weighted = weighted,
    CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], Sigma.x = sigmax,
    Sigma.y = sigmay, Major = Major, Minor = Minor, Theta = theta,
    Eccentricity = eccentricity, Area.sde = areaSDE, TanTheta = tantheta,
    SinTheta = sintheta, CosTheta = costheta, SinThetaCosTheta = sinthetacostheta,
    Sin2Theta = sin2theta, Cos2Theta = cos2theta, ThetaCorr = Theta.Corr
  )
  # print(result.sde)
  result.sde <- as.data.frame(result.sde)
  # assign("sdeatt", result.sde, pos = 1)
  sdeloc <- as.data.frame(cbind(id, coordsSDE))
  colnames(sdeloc) <- c("id", "x", "y")
  # write.table(sdeloc, sep = ",", file = filename, col.names = FALSE)
  # assign("sdeloc", sdeloc, pos = 1)

  return(list(coords = sdeloc[, 2:3], df = result.sde))
}
