library(readr); library(tidyverse); library(grid); library(gridExtra); library(e1071);

  #import the required rds obojects
soil_aft <- read_rds("RDS files/soil_aft.rds") # the final data after initial tidying

soilcat <- read_rds("RDS files/soilcat.rds") # Categorical varaibles

soilcon <- read_rds("RDS files/soilcon.rds") # Continuous Varaibles

############### Functions for super quick drawings of hist and density ################

histPlot <- function(data, col) {
  dt <- data.frame(x = data[[col]])
  p <- ggplot(dt, aes(x = factor(x)))+
    geom_bar()+
    xlab(colnames(data)[col])+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

denPlot <- function(data, col){
  dt <- data.frame(x=data[[col]], SoilErosion = data$`soil erosion`)
  p <- ggplot(data= dt) + 
    geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(dt)[col]), '\n', 'Skewness: ',
                round(e1071::skewness(dt[[col]], na.rm = TRUE), 2))) + 
    theme_light() 
  return(p)
}


doPlots <- function(data, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data = data, col=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

soilcat <- soilcat %>% select(-c(musym))
soil_aft <- soil_aft %>% select(-c(musym))

############## Histograms for Categorical Variables ################

doPlots(soilcat, fun = histPlot, ii = 1:4, ncol = 2)

doPlots(soilcat, fun = histPlot, ii = 5:8, ncol = 2)

doPlots(soilcat, fun = histPlot, ii = 9:12, ncol = 2)

############## Density Plot for Continuous Variables ################

ggplot(data = soilcon) + geom_density(aes(x = muacres))

ggplot(data = soilcon) + geom_density(aes(x = comppct_r))

ggplot(data = soilcon) + geom_density(aes(x = awc_r))

ggplot(data = soilcon) + geom_density(aes(x = yield1))

ggplot(data = soilcon) + geom_density(aes(x = yield2))

ggplot(data = soilcon) + geom_density(aes(x = yield3))

ggplot(data = soilcon) + geom_density(aes(x = `soil erosion`))

