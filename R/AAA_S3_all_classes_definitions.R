
# Biomass -----------------------------------------------------------------

#' @export
biomass <- function(x, ...){
  x <- new_biomass(x, ...)
  validate_biomass(x)
}

new_biomass <- function(x = double(), units = "kg", mode = "standard") {

  stopifnot(is.double(x))
  stopifnot(is.character(units))

  if (is_units){
    x <- units::set_units(x, value = units, mode = mode)
  } else {
    x <- units::as_units(x, value = units)
  }

  x <- structure(x, class = c("biomass", "units"))
}

validate_biomass <- function(x){
  stopifnot(is.biomass(x))
  return(x)
}

# Density -----------------------------------------------------------------

#' @export
biomass_density <- function(x, ...){
  x <- new_biomass_density(x, ...)
  validate_biomass_density(x)
}

new_biomass_density <- function(x = double(), units = "kg/km^2", mode = "standard") {

  # browser()

  stopifnot(is.double(x))
  stopifnot(is.character(units))

  if (is_units(x)){
    x <- units::set_units(x, value = units, mode = mode)
  } else {
    x <- units::as_units(x, value = units)
  }

  x <- structure(x, class = c("biomass_density", "units"))
}

validate_biomass_density <- function(x){
  stopifnot(is.biomass_density(x))
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
  stopifnot(is.area(x))
  return(x)
}

# Detect and cast ---------------------------------------------------------

detect_and_cast <- function(x){

  if (is.numeric(x)){

    if (has_biomass_units(x)){
      x <- biomass(x)
    } else if (has_biomass_density_units(x)){
      x <- biomass_density(x)
    }

  }

  return(x)
}

is_units <- function(x){
  checkmate::test_class(x, "units")
}

is.biomass <- function(x){
  checkmate::test_class(x, "biomass") && has_biomass_units(x)
}

is.biomass_density <- function(x){
  checkmate::test_class(x, "biomass_density") && has_biomass_density_units(x)
}

has_biomass_units <- function(x){
  if (is_units(x)){
    num <- units(x)$numerator
    den <- units(x)$denominator
    (length(num) == 1) && (length(den) == 0) && (num == "kg")
  } else {
    FALSE
  }
}

has_biomass_density_units <- function(x){
  if (is_units(x)){
    num <- units(x)$numerator
    den <- units(x)$denominator
    (length(num) == 1) && (length(den) == 2) && (num == "kg") && (den == c("km", "km"))
  } else {
    FALSE
  }
}

has_area_units <- function(x){
  if (is_units(x)){
    num <- units(x)$numerator
    den <- units(x)$denominator
    (length(num) == 2) && (length(den) == 0) && (num == c("km", "km"))
  } else {
    FALSE
  }
}
