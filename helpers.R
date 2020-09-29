library(shiny)
library(shinydashboard)
library(PSF)
library(forecast)
library(ForecastTB)
library(GlobalOptions)
library(gridExtra)
library(shape)
library(circlize)
library(decomposedPSF)
library(curl)
library(tinytex)

test1 <- function(data, nval){
  return(lpsf(data = data, n.ahead = nval))
}

test2 <- function(data, nval){
  a <- psf(data = data, cycle = 12)
  b <- predict(object = a, n.ahead = nval)
  return(b)
}

