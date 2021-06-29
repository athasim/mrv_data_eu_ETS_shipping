## Supplementary R file code of the paper: 
## "Inclusion of Shipping in the EU-ETS: Assesing the direct costs for the maritime sector using the MRV data".
## You are free to use it but please make a reference to the paper.
## For any questions about the R code you ar free to contact the author (Athanasios Simoulis) at 
## at: thanasis @ simoulis.eu. The code author has the full responsibility for the code.
## For any questions about the results interpretation and the paper please contact:
## Dr Anastasia Christodoulou  at: ac @ wmu.se or at: anastasiachristo @ gmail.com

library(readxl)
library(httr)
library(tidyverse)

##download data from EMSA website. We used the 2019 data.
url2019<- "https://mrv.emsa.europa.eu/api/public-emission-report/reporting-period-document/binary/2019?"
download.file(url2019,destfile = "./a2019.xlsx",mode = "wb")

#create the  the new work dataset
set2019 <- read_excel("a2019.xlsx", skip = 2) #import all the data

dataset2019 <- set2019 %>% select(contains(c("emissions","Fuel","time","consumption"))) %>% lapply(as.numeric) %>% as.tibble #import the data that we want, create a new tibble
dataset2019$`Ship type` <- set2019$`Ship type` %>% as.factor #add the ship type as factor



###Calculate the Total emissions etc
#create the matrix
study <- matrix(rnorm(16*4), ncol = 5, nrow = 16)
colnames(study) <- c("Total emissions", "Emissions within EU", "Total Emissions and 50%", "Average Co2 emissions per transport work with the outliers", "Average Co2 emissions per transport work without the outliers")
nam_ship <- levels(dataset2019$`Ship type`) # types of ships
rownames(study) <- c(nam_ship,"Sum of all types") # give name to the rows

#
#total co2
study[1:15,1] <- rowsum(dataset2019$`Total CO₂ emissions [m tonnes]`,dataset2019$`Ship type`, reorder = TRUE, na.rm = TRUE)
study[1:15,2] <- rowsum(dataset2019$`CO₂ emissions from all voyages between ports under a MS jurisdiction [m tonnes]`,dataset2019$`Ship type`, reorder = TRUE, na.rm = TRUE)
study[1:15,3] <- study[1:15,1] - (study[1:15,1] - study[1:15,2])/2

#
study[16,1] <- sum(study[1:15,1])
study[16,2] <- sum(study[1:15,2])
study[16,3] <- sum(study[1:15,3])
study[16,4] <- NA
study[16,5] <- NA
View(study)


#####Calculate the mean of Annual average CO₂ emissions per transport work (mass) [g CO₂ / m tonnes · n miles] without outliers


co2_annual_average_per_MASS <- select(dataset2019,`Ship type`, `Annual average CO₂ emissions per transport work (mass) [g CO₂ / m tonnes · n miles]`) #create the dataset step2 only with the two
ship_types <- co2_annual_average_per_MASS #create the dataset step3
ship_types <- split.data.frame(co2_annual_average_per_MASS,co2_annual_average_per_MASS$`Ship type`, drop = FALSE) #create the dataset step4

##We want to do this for all types of ships
an <- ship_types[[c(1,2)]]
for (n in 1:15) {an[n] <- as.data.frame(ship_types[[c(n,2)]]) }
ind_an <- an
for (n in 1:15) {ind_an[n] <- as.data.frame(boxplot.stats(an[[n]])$out)}
index_an <- an
for (n in 1:15) {index_an[n] <- as.data.frame(which(an[[n]] %in% ind_an[[n]]))}
an_kath <- an
for (n in 1:15) {an_kath[n] <- as.data.frame(an[[n]][-index_an[[n]]])}
tot_co2_MEAN <- c(1:15)
tot_co2_MEAN_AKATH <- c(1:15)
for (n in 1:15) { tot_co2_MEAN[n] <- mean(an_kath[[n]], na.rm = T)} # without the outliers
for (n in 1:15) { tot_co2_MEAN_AKATH[n] <- mean(an[[n]], na.rm = T)} #with the outliers in

tot_co2_MEAN[5] <- tot_co2_MEAN_AKATH[5] #When we calculate this without outliers it gives NA because it has small sample, we use the initial
tot_co2_MEAN[8] <- tot_co2_MEAN_AKATH[8] #When we calculate this without outliers it gives NA because it has small sample, we use the initial
study[1:15,4] <- tot_co2_MEAN_AKATH
study[1:15,5] <- tot_co2_MEAN

study = round(study, digits = 2)
View(study)

##export to csv
write.csv(study, file = "./apotelesmata.csv", col.names = TRUE, row.names = T, append = F)

