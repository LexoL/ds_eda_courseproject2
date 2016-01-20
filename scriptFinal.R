NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot1.R
##########
## QUESTION 1 
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.
##########


## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")


## Split the data by year, sum within each year, construct a data frame

emissionsSum <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))
dfEmissions <- data.frame(year = names(emissionsSum), total.emissions = emissionsSum)


## Write the plot to a proper file

png("plot1.png")
plot(dfEmissions, xlab = "Year", ylab = "Total emissions", main = "Total emissions from PM2.5 in US")
dev.off()



####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot2.R
##########
## QUESTION 2 
## Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.
##########

## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")


## Select Baltimore data

baltimoreNEI <- subset(NEI,fips == "24510")


## Split the selected data by year, sum within each year, construct a data frame

baltimoreEmissionsSum <- with(baltimoreNEI, tapply(Emissions, year, sum, na.rm = TRUE))
dfBaltimoreEmissions <- data.frame(year = names(baltimoreEmissionsSum), total.emissions = baltimoreEmissionsSum)


## Write the plot to a proper file

png("plot2.png")
plot(dfBaltimoreEmissions, xlab = "Year", ylab = "Total emissions", main = "Total emissions from PM2.5 in Baltimore")
dev.off()

####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot3.R
##########
## QUESTION 3 
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999â2008 for Baltimore City? 
## Which have seen increases in emissions from 1999â2008? Use the ggplot2 plotting system to make 
## a plot answer this question.
##########

## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")


## Select Baltimore data

baltimoreNEI <- subset(NEI,fips == "24510")


## Split the selected data by year and type, sum within each year*type

spBaltimore <- split(baltimoreNEI$Emissions,list(baltimoreNEI$year, baltimoreNEI$type) )
spBaltimoreSum <- lapply(spBaltimore,sum, na.rm = TRUE)


## Construct correct titles, and then assembly a data frame 

year.type <- strsplit(names(spBaltimoreSum), ".", fixed=TRUE)
years <- unlist(lapply(year.type, f <- function(x) x[1]))
types <- unlist(lapply(year.type, f <- function(x) x[2]))
dfBaltimoreSumByYearType <- data.frame(year = years,
                                       type = types,
                                       Emissions = unlist(spBaltimoreSum))


## Write the plot to a proper file

library(ggplot2)

png("plot3.png")
g <- ggplot(dfBaltimoreSumByYearType, aes(year, Emissions)) +
        geom_bar(stat = "identity") + facet_grid(.~type) +
        ggtitle("Total emissions from PM2.5 in Baltimore") + xlab("Year")
print(g)
dev.off()


####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot4.R
##########
## QUESTION 4 
## Across the United States, how have emissions 
## from coal combustion-related sources changed from 1999â2008?


## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Select IDs of the coal combustion-related sources, then select records containing selected IDs

comb.coal <- grepl(" Comb(.)*Coal", SCC$Short.Name)
comb.coal.scc <- SCC$SCC[comb.coal]
NEI.comb.coal.idx <- NEI$SCC %in% comb.coal.scc
NEI.comb.coal <- NEI[NEI.comb.coal.idx,]


## Split the selected data by year, sum within each year, construct a data frame

emissionsSum.comb.coal <- with(NEI.comb.coal, tapply(Emissions, year, sum, na.rm = TRUE))
dfEmissions.comb.coal <- data.frame(year = names(emissionsSum.comb.coal), total.emissions = emissionsSum.comb.coal)


## Write the plot to a proper file

png("plot4.png")
plot(dfEmissions.comb.coal, xlab = "Year", ylab = "Total emissions", main = "Total emissions from PM2.5 \n from coal combustion-related sources in US")
dev.off()


####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot5.R
##########
## QUESTION 5 REMEMBER LOADING DATA v
## How have emissions from motor vehicle sources 
## changed from 1999â2008 in Baltimore City?


## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Select IDs of the motor vehicle-related sources, then select records containing selected IDs

motor.veh <- grepl("[V|v]ehicle", SCC$EI.Sector)
motor.veh.scc <- SCC$SCC[motor.veh]
NEI.motor.veh.idx <- NEI$SCC %in% motor.veh.scc
NEI.motor.veh <- NEI[NEI.motor.veh.idx,] 


## Now select only Baltimore data

baltimoreNEI.motor.veh <- subset(NEI.motor.veh,fips == "24510")


## Split the selected data by year, sum within each year, construct a data frame

baltimoreEmissionsSum.motor.veh <- with(baltimoreNEI.motor.veh, tapply(Emissions, year, sum, na.rm = TRUE))
dfBaltimoreEmissions.motor.veh <- data.frame(year = names(baltimoreEmissionsSum.motor.veh), total.emissions = baltimoreEmissionsSum.motor.veh)


## Write the plot to a proper file

png("plot5.png")
plot(dfBaltimoreEmissions.motor.veh, xlab = "Year", ylab = "Total emissions", main = "Total emissions from PM2.5 \n from motor vehicle sources in Baltimore city")
dev.off()


####################################################################################
## JHU Coursera DS Exploratory Data Analysis Assignment for Course Project 2
## Script: Plot6.R
##########
## QUESTION 6 REMEMBER LOADING DATA v
############
## Compare emissions from motor vehicle sources in Baltimore City 
## with emissions from motor vehicle sources 
## in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?

## Load the data 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Select Baltimore or LA data by fips code

balt.lang.NEI <- subset(NEI,fips == "24510" | fips == "06037")
balt.lang.NEI$city <- ifelse(balt.lang.NEI$fips == "24510", "Baltimore", "Los_Angeles") 


## Now select IDs of the motor vehicle-related sources, then select records containing selected IDs
motor.veh <- grepl("[V|v]ehicle", SCC$EI.Sector)
motor.veh.scc <- SCC$SCC[motor.veh]
balt.lang.NEI.motor.veh.idx <- balt.lang.NEI$SCC %in% motor.veh.scc
balt.lang.NEI <- balt.lang.NEI[balt.lang.NEI.motor.veh.idx,]


## Split the selected data by year and city, sum within each year*city, correct titles

sp.balt.lang <- split(balt.lang.NEI$Emissions,list(balt.lang.NEI$year, balt.lang.NEI$city) )
sp.balt.lang.Sum <- lapply(sp.balt.lang,sum, na.rm = TRUE)
year.city <- strsplit(names(sp.balt.lang.Sum ), ".", fixed=TRUE)
years <- unlist(lapply(year.city, f <- function(x) x[1]))
cities <- unlist(lapply(year.city, f <- function(x) x[2]))
cities <- sub("_", " ", cities, fixed=TRUE)


## Assembly a data frame

df.balt.lang.Sum <- data.frame(year = years,
                                       city = cities,
                                       Emissions = unlist(sp.balt.lang.Sum))


## Write the plot to a proper file

library(ggplot2)

png("plot6.png")
g <- ggplot(df.balt.lang.Sum, aes(year, Emissions)) +
        geom_bar(stat = "identity") + facet_grid(.~city) +
        ggtitle("Total emissions from PM2.5 from motor vehicle sources") + xlab("Year")
print(g)
dev.off()
