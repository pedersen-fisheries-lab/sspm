
# Biomass -----------------------------------------------------------------

#' @export
biomass <- function(x, ...){
  x <- new_biomass(x, ...)
  validate_biomass(x)
}

new_biomass <- function(x = double(), units = "kg") {

  stopifnot(is.double(x))
  stopifnot(is.character(units))

  x <- units::as_units(x, value = units)

  x <- structure(x, class = c("biomass", "units"))
}

validate_biomass <- function(x){
  num <- units(x)$numerator
  den <- units(x)$denominator

  stopifnot(length(num) == 1)
  stopifnot(length(den) == 0)

  stopifnot(num == "kg")
  return(x)
}

# Density -----------------------------------------------------------------

#' @export
biomass_density <- function(x, ...){
  x <- new_biomass_density(x, ...)
  validate_biomass_density(x)
}

new_biomass_density <- function(x = double(), units = "kg/km^2") {

  stopifnot(is.double(x))
  stopifnot(is.character(units))

  x <- units::as_units(x, value = units)

  x <- structure(x, class = c("biomass_density", "units"))
}

validate_biomass_density <- function(x){
  num <- units(x)$numerator
  den <- units(x)$denominator

  stopifnot(length(num) == 1)
  stopifnot(length(den) == 2)

  stopifnot(num == "kg")
  stopifnot(den == c("km", "km"))
  return(x)
}

# Print -------------------------------------------------------------------

#' @export
print.biomass <- function(x, ...){
  cat("SSPM Variable Type: [biomass] \n")
  class(x) <- "units"
  NextMethod()
}

#' @export
print.biomass_density <- function(x, ...){
  cat("SSPM Variable Type: [biomass density] \n")
  class(x) <- "units"
  NextMethod()
}

# Conversions -------------------------------------------------------------

#' @export
as_biomass <- function(x, ...){
  UseMethod("as_biomass")
}

#' @export
as_biomass.biomass_density <- function(x, area){
  validate_area(area)
  x <- x * area
  class(x) <- c("biomass", "units")
  return(x)
}

validate_area <- function(x){
  stopifnot("units" %in% class(x))

  num <- units(x)$numerator
  den <- units(x)$denominator

  stopifnot(length(num) == 2)
  stopifnot(length(den) == 0)

  stopifnot(num == c("km", "km"))

  stopifnot()
}
