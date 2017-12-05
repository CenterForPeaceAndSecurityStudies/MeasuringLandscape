cat("\014")
# This is a custom function to convert from East Africa Grid System to Latitude Longitude
# Originally had a bug in it. Need to make sure everything is treated as float internally no matter what
# This version takes a full letter code

# EGSToLatLng(band="G",block="A",subblock="F", easting=310 , northing=885)
EAGS2LatLong <- function(band, block, subblock, easting, northing) {
  temp <- try({
    # Convert the band into a final easting offset
    long0 <- list(
      "D" = 17.5,
      "E" = 22.5,
      "F" = 27.5,
      "G" = 32.5,
      "H" = 37.5,
      "J" = 42.5,
      "K" = 47.5,
      "L" = 52.5
    )[band][[1]]

    # Convert the block into a north offset (for only the two letters in Kenya)
    block_north <- list(
      "A" = 0,
      "Z" = -300000.0
    )[block][[1]]

    if (block_north >= 0) northernHemisphere <- T else northernHemisphere <- F

    # Convert the sublock into a northing offset
    # NPRSTU then add 200,000
    # GHJKLM then add 100,000
    # abcdef, then the meter northing is just the number given
    subblock_north <- list(
      "N" = 200000.0, "P" = 200000.0, "R" = 200000.0, "S" = 200000.0, "T" = 200000.0, "U" = 200000.0,
      "G" = 100000.0, "H" = 100000.0, "J" = 100000.0, "K" = 100000.0, "L" = 100000.0, "M" = 100000.0,
      "A" = 0.0, "B" = 0.0, "C" = 0.0, "D" = 0.0, "E" = 0.0, "F" = 0.0
    )[subblock][[1]]


    # Convert the sublock into a easting offset
    # I think it said that d is north east of 0,so some of these will be negatives
    # No that's crazy lets just do horizontal steps and if it doesn't work we'll try something else

    # I have changed these to 1 to 6 from 0 to 5. I do not know if that is right, but it matches up
    # It may be that I don't understand where they start, D could be center for example
    subblock_easting <- list(
      "N" = 100000.0, "P" = 200000.0, "R" = 300000.0, "S" = 400000.0, "T" = 500000.0, "U" = 600000.0,
      "G" = 100000.0, "H" = 200000.0, "J" = 300000.0, "K" = 400000.0, "L" = 500000.0, "M" = 600000.0,
      "A" = 100000.0, "B" = 200000.0, "C" = 300000.0, "D" = 400000.0, "E" = 500000.0, "F" = 600000.0
    )[subblock][[1]]

    # Ok now multiply the numbers given
    # If 3 digits than move over one
    if (easting >= 100) easting <- easting / 10.0
    if (northing >= 100) northing <- northing / 10.0

    easting <- easting * 1000.0
    northing <- northing * 1000.0

    easting <- easting + subblock_easting
    northing <- northing + block_north + subblock_north

    # Book example
    # easting=645027.92
    # northing=3306703.90
    # long0=

    # East African Grid System using the Clark 1880 Centroid
    f <- 1 / 293.465
    esq <- 2.0 * f - f ^ 2.0
    e <- sqrt(esq)
    e1sq <- esq / (1 - esq)
    a <- 6378249.145
    k0 <- 0.9995
    falseeasting <- 400000.0
    falsenorthing <- 4500000.0

    # northing = northing-falsenorthing #killed this line because the false northing is built into the grid designation

    arc <- northing / k0
    mu <- arc / (a * (1.0 - e ^ 2.0 / 4.0 - 3.0 * e ^ 4.0 / 64.0 - 5.0 * e ^ 6.0 / 256.0))

    ei <- (1.0 - (1.0 - e * e) ^ (1.0 / 2.0)) / (1.0 + (1.0 - e * e) ^ (1.0 / 2.0))

    ca <- 3.0 * ei / 2.0 - 27.0 * ei ^ 3 / 32.0

    cb <- 21.0 * ei ^ 2.0 / 16.0 - 55.0 * ei ^ 4 / 32.0
    cc <- 151.0 * ei ^ 3.0 / 96.0
    cd <- 1097.0 * ei ^ 4.0 / 512.0
    phi1 <- mu + ca * sin(2 * mu) + cb * sin(4 * mu) + cc * sin(6.0 * mu) + cd * sin(8.0 * mu)

    n0 <- a / (1 - (e * sin(phi1) ^ 2)) ^ (1 / 2.0)

    # r0 = a * (1.0 - e * e) / math.pow((1.0 - math.pow((e * math.sin(phi1)), 2.0)), (3 / 2.0))
    r0 <- a * (1.0 - e * e) / (1.0 - (e * sin(phi1)) ^ 2.0) ^ (3 / 2.0)
    fact1 <- n0 * tan(phi1) / r0

    Qa1 <- falseeasting - easting # apparently have to keep the false easting
    dd0 <- Qa1 / (n0 * k0)
    fact2 <- dd0 * dd0 / 2.0

    t0 <- (tan(phi1) ^ 2.0)
    Q0 <- e1sq * (cos(phi1) ^ 2.0)
    fact3 <- (5.0 + 3.0 * t0 + 10.0 * Q0 - 4.0 * Q0 * Q0 - 9.0 * e1sq) * (dd0 ^ 4.0) / 24.0

    fact4 <- (61.0 + 90.0 * t0 + 298.0 * Q0 + 45.0 * t0 * t0 - 252.0 * e1sq - 3.0 * Q0 * Q0) * (dd0 ^ 6) / 720.0

    lof1 <- Qa1 / (n0 * k0)
    lof2 <- (1.0 + 2.0 * t0 + Q0) * (dd0 ^ 3.0) / 6.0
    lof3 <- (5.0 - 2.0 * Q0 + 28.0 * t0 - 3.0 * (Q0 ^ 2.0) + 8.0 * e1sq + 24.0 * (t0 ^ 2.0)) * (dd0 ^ 5.0) / 120.0
    Qa2 <- (lof1 - lof2 + lof3) / cos(phi1)
    Qa3 <- Qa2 * 180.0 / pi

    latitude <- 180.0 * (phi1 - fact1 * (fact2 + fact3 + fact4)) / pi

    # This line wasn't necessary when I tried it
    # if(!northernHemisphere) latitude = -latitude #Apparently flip it if northHemsphere is true

    # longitude = ((zone > 0) and (6 * zone - 183.0) or 3.0) - Qa3 #original
    longitude <- long0 - Qa3 # insert my own band here

    if (is.finite(latitude) & is.finite(longitude)) {
      return(list(latitude = latitude, longitude = longitude))
    } # In an edge case latitude is numeric(0)
  }, silent = T)

  return(list(latitude = NA, longitude = NA)) # If anything errors out return NA
}


# I converted the wrong one. This goes from UTM. I'll do the correct one eventually.
# EAGS2LatLong(band="G",block="A",subblock="F", easting=310 , northing=885) #close by not exact
# (0.8003110797877706, 34.57585381772091)
# Pass it a Z block to see if lat is negative
# Now it's not working with A blocks
# EAGS2LatLong(band="G",block="A",subblock="F", easting=310 , northing=885) #close by not exact
# EAGS2LatLong(band="G",block="Z",subblock="F", easting=310 , northing=885) #close by not exact
