library(readr); library(tidyverse); 
library(mice); library(lattice); library(purrr); library(VIM) # for MDs
library(corrplot) #for correlation plot

soil <- read_csv("cmz-5-2011-yields.csv")


####################### Drop Unused Variables (Initial Approach) #######################

#areasymbol, mukey, muname (represents ID and other likes)

#watereros, winderos (Obviously linear dependent of `soil erosion`)

soil <- soil %>%
  dplyr::select(-c(areasymbol, mukey, muname, watereros, winderos, texdesc))


# output variable names

varname <- as.data.frame(variable.names(soil))

write.csv(varname, file = "soil_vars.csv")

  #transform irrcapcl and irrcapscl variables' NULL value into `other`
    #nirrcapscl should be character instead of numeric, transform it than begin checking

soil$nirrcapscl <- as.character(soil$nirrcapscl)

table(is.na(soil$irrcapcl))
table(is.na(soil$irrcapscl))
table(is.na(soil$nirrcapcl))
table(is.na(soil$nirrcapscl))# all returns no na values

table(soil$irrcapcl == "NULL") #21380 trues
table(soil$irrcapscl == "NULL") #25280 trues
table(soil$nirrcapscl == "NULL") #160 trues

soil$irrcapcl <- str_replace(soil$irrcapcl,"NULL", "other")
soil$irrcapscl <- str_replace(soil$irrcapscl, "NULL", "other")
soil$nirrcapscl <- str_replace(soil$nirrcapscl, "NULL", "other")
soil$nirrcapcl <- str_replace_na(soil$nirrcapcl, replacement = "other")

table(soil$irrcapcl == "other") #21380 trues, successfully transformed
table(soil$irrcapscl == "other") #25280 trues, successfully transformed
table(soil$nirrcapscl == "other") #160 trues, successfully transformed
table(soil$nirrcapcl == "other") #140 trues, successfully transformed

  # Transform yield2 and yield3 "NULL" into 0
table(soil$yield2 == "NULL") #14696 trues
table(soil$yield2 == "null") #44088 trues

table(soil$yield3 == "NULL") #29392 trues
table(soil$yield3 == "null") #0 trues

soil$yield2 <- str_replace_all(soil$yield2, c("NULL" = "0", "null" = "0"))
soil$yield3 <- str_replace(soil$yield3, "NULL", "0")

table(soil$yield2 == "NULL") #0 trues, successfully transformed
table(soil$yield2 == "null") #0 trues, successfully transformed

table(soil$yield3 == "NULL") #0 trues

soil$yield1 <- as.numeric(soil$yield1) #class(soil$yield1) NUM
soil$yield2 <- as.numeric(soil$yield2) #class(soil$yield2) NUM
soil$yield3 <- as.numeric(soil$yield3) #class(soil$yield3) NUM

soil.int <- soil

###################### Missing Values Proportions and Patterns #################

md.values <- map_df(soil.int, function(x) sum(is.na(x)))

which(md.values != 0) # 2, 11, 26, 27, 28, 29, 30, 31 cols have missing values

# musym awc_r crop1" "Rem Res - crop1" "crop2" "Rem Res - crop2" 
# "crop3" "Rem Res - crop3" 

# The Missing Values Pattern graph
md.pattern(soil.int, rotate.names = T)

#Draw the proportions of missing values
soil.int <- soil.int[, -c(2, 26:31)]

missing <- write_rds(soil.int, "RDS files/missing.rds")
aggreplot <- VIM::aggr(soil.int, numbers = T, prop = T, sortVars = T,
                       labels = names(soil.int), cex.axis = 0.7, gap = 3,
                       ylabs = c("Histogram of missing data", "Patterns"))

aggreplot

# crop2, Rem Res - crop2, crop3, Rem Res - crop3 needs to be deleted (4 vars.)

# counts of the remaining mds that needs to be imputed
md.values$crop1               # 36740 mds #class(soil$crop1)             CHR
md.values$`Rem Res - crop1`   # 36740 mds #class(soil$`Rem Res - crop1`) NUM
md.values$musym               # 8920 mds  #class(soil$musym)             NUM
md.values$awc_r               # 220 mds   #class(soil$awc_r)             NUM

# Drop the variables with too much missing values
soil.int <- soil.int %>%
  dplyr::select(-c(crop2, `Rem Res - crop2`, crop3, `Rem Res - crop3`))

#################### Draw the Correlation Plot ######################

# First we seperate the categorical and continuous variables
variable.names(soil.int)

soilcat <- soil.int[, c(1, 2, 5, 6:10, 12, 13, 17, 24, 26)]
soilcon <- soil.int[, -c(1, 2, 5, 6:10, 12, 13, 17, 24, 26)]

soilcon2 <- soilcon[complete.cases(soilcon),]
  # 36630 obs, maybe we should draw the correlation plot AFTER imputting for more precise results.  

corr.soil <- cor(soilcon2)
corrplot(corr.soil, method = "color", diag = F, type = "upper",
         tl.cex = 0.5 )
# sci and scier have EXTREME correlations with soil erosion var.

which(abs(corr.soil["soil erosion",]) > 0.9) #soil erosion, which is normal
                                              # sci and scier

#sci and scier should be dropped to avoid multicollinearity

soil.fin <- soil.int %>%
  dplyr::select(-c(sci, scier))

  # 73480 obs with 25 variables. 13 categorical and 12 continuous varaibles.

############################# Transfrom soil erosion to binary response ##################

# easy drawing of soil erosion (y variable) density plot
# for later transforming into a binary variable


summary(soil$`soil erosion`) # Median = 1.540 Mean = 8.965 3rd Q = 7.08
# soil erosion is EXTREMELY right-skewed

table(is.na(soil$`soil erosion`)) # FALSE 73480 (no NAs at all)

ggplot(data = soil, aes(x = `soil erosion`)) + geom_density() + xlim(0,10)

table(soil$`soil erosion` > 10) # 14659 obs have soil erosion > 10 
table(soil$`soil erosion` > 100) # 856 obs > 100
soil[soil$`soil erosion`>400,] %>% View() # max soil erosion is 453.62


# Further discuss is needed to set a standard for transforming `soil erosion` to binary response

#https://www.esf.edu/for/briggs/FOR345/erosion.htm?fbclid=IwAR0UOVhUWQvdb1xDZvYO_AXjaS_I4Pto2CbPUOxwW3R-xZ1IPMYZQBGFnB0

# According to the above reference: 5 T/acre/year is a reasonable criterion for binary classification
# > 5 T/acre/year, the soil is deemed to be no long term productivity

prop.table(table(soil.fin$`soil erosion` >= 5)) # 30.34567 % have severe erosion problems


## Start transforming Y variable

soil.fin$`soil erosion` <- ifelse(soil.fin$`soil erosion` >= 5, "Severe", "Minor")
prop.table(table(soil.fin$`soil erosion`))
 # Severe 30.34567 %  Minor 69.65433 %

soil_aft <- soil.fin

# output soil_aft vars for later EDA purposes

varss <- as.data.frame(variable.names(soil_aft))
write.csv(varss, file = "soil_aft_variables.csv")

soil_aft <- soil_aft %>%
  dplyr::select(-c(`Rem Res - crop1`))


soil_aft$farmlndcl <- str_replace(soil_aft$farmlndcl, "NULL", "other")

soilcat$farmlndcl <- str_replace(soilcat$farmlndcl, "NULL", "other")

# cleanup your global environment
#rm(list = c("varname", "varss", "soilcon2", "soil.int", "soil.fin", "missing"))

# Final Data: soil_aft, 24 vars (13 cat., 11 con.) 

################ write an RDS object for other codescripts #####################

write_rds(soil_aft, "RDS files/soil_aft.rds")
write_rds(soilcat, "RDS files/soilcat.rds")
write_rds(soilcon, "RDS files/soilcon.rds")
