library(tidyverse)
library(broom)
library(dplyr)
choices1 <- data.frame(methodsnames = c("ARIMA", "LPSF", "PSF", "ETS"),
 methods= c("ARIMA", "test1(data, nval)", "test2(data, nval)", "test3(data, nval)"))

#c(names(choices1)[choices1 == "test3(data, nval)"], names(choices1)[choices1 == "test2(data, nval)"])


choices2 <- c("LPSF", "PSF", "ETS", "ARIMA")


# sameer <- names(choices1)[choices1 == choices2]
# 
# sameer


# index <- choices1$methods == choices2
# choices1[index, ]$methodsnames

# index <- match(choices2, choices1$methodsnames)
# choices1[index, ]$methodsnames
# index

choices1 <- choices1 %>% add_row(methodsnames = "Nepal",methods = "india")
choices1