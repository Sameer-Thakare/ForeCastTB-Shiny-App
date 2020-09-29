library(forecast)
library(ForecastTB)
library(GlobalOptions)
library(gridExtra)
library(shape)
library(circlize)
library(PSF)
library(decomposedPSF)

test1 <- function(data, nval){
  return(lpsf(data = data, n.ahead = nval))
}

test2 <- function(data, nval){
  a <- psf(data = data, cycle = 12)
  b <- predict(object = a, n.ahead = nval)
  return(b)
}

test3 <- function(data, nval){
  b <- as.numeric(forecast(ets(data), h = nval)$mean)
  return(b)
}

choices1 <- c(ARIMA= "ARIMA", ETS = "test3(data, nval)", LPSF = "test1(data, nval)")

a1 <- prediction_errors(data = nottem, nval = 12,
                        Method = c(choices1),
                        MethodName = c(names(choices1)),
                        append_ = 0)



#c1 <- append_(object = a1, Method = c("test2(data,nval)"), MethodName = c('PSF'))
plot(a1)
a1@output


tabBox(id
  tabPanel("diamonds", DT::dataTableOutput("results1")),
  tabPanel("mtcars", DT::dataTableOutput("results1")))