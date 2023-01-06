NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.
colnames(NEI)
head(NEI)

# Year 1999 
total = with(NEI, tapply(Emissions, year, sum, na.rm =TRUE))

data = data.frame(year = c(1999, 2002, 2005, 2008), 
           emission = total)
plot(data, xlab = "Year", ylab= "Emission", type = "o", col = "steelblue3",
     main = "Total US PM2.5 Over the Years")

png("plot1.png",width=480,height=480)
plot(data, xlab = "Year", ylab= "Emission", type = "o", col = "steelblue3",
     main = "Total US PM2.5 Over the Years")
dev.off()


# Q2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
Balt = subset(NEI, fips == 24510)
head(Balt)

total2 = with(Balt, tapply(Emissions, year, sum, na.rm =TRUE))

data2 = data.frame(year = c(1999, 2002, 2005, 2008), 
                  emission = total2)

png("plot2.png",width=480,height=480)

plot(data2, xlab = "Year", ylab= "Emission", type = "o", col = "steelblue3",
     main = "Total Baltimore PM2.5 in Mayrland")

dev.off()

# Q3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.
install.packages("ggplot2")
library(ggplot2)
head(Balt)
str(Balt)

sapply(split(Balt, Balt$type), nrow)

point = subset(Balt, type == "POINT")
nonpoint = subset(Balt, type == "NONPOINT", na.rm=TRUE)
onroad = subset(Balt, type == "ON-ROAD", na.rm=TRUE)
nonroad = subset(Balt, type == "NON-ROAD", na.rm=TRUE)

bat_type = aggregate(Emissions ~ year + type, Balt, sum)
head(bat_type)
help(aggregate)

png("plot3.png",width=480,height=480)

ggplot(bat_type, aes(year, Emissions, col= type)) +
       geom_line() + geom_point() + 
     ggtitle(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions by Type and Year")) +
     ylab(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions")) +
     xlab("Year") +
     scale_colour_discrete(name = "Type of sources") +
     theme(legend.title = element_text(face = "bold"))

dev.off()

# Q4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

View(SCC)
sapply(split(SCC, SCC$Short.Name), nrow)

SCC_coal = SCC[grep("coal", SCC$Short.Name, ignore.case = T), ]
head(SCC_coal)
dim(SCC_coal)
NEI_coal = NEI[NEI$SCC %in% SCC_coal$SCC, ] 
dim(NEI_coal)

png("plot4.png",width=480,height=480)

NEI_coal_tot = aggregate(Emissions~year+type, NEI_coal, sum)
ggplot(NEI_coal_tot, aes(year, Emissions, col = type)) + 
     geom_line() + geom_point() +
     ggtitle(expression("Coal-Related Sources Total Emission by Type and Year")) +
     ylab(expression("Total Emissions")) +
     xlab("Year") +
     scale_colour_discrete(name = "Type of sources") +
     theme(legend.title = element_text(face = "bold"))

dev.off()
     

# Q5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
MV= subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")

Bat_MV = aggregate(Emissions~ year+type, MV, sum)

png("plot5.png",width=480,height=480)

ggplot(Bat_MV, aes(year, Emissions)) +
     geom_line(col = "steelblue3") +
     geom_point(col = "steelblue3") +
     ggtitle(expression("Baltimore " ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) +
     xlab("Year") +
     ylab(expression(~PM[2.5]~ "Motor Vehicle Emissions"))

dev.off()

# Q6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
# sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

Both_MV = subset(NEI, NEI$fips %in% c("06037", "24510") & NEI$type == "ON-ROAD")

Both_plot = aggregate(Emissions~ year + fips, Both_MV, sum)

png("plot6.png",width=480,height=480)

ggplot(Both_plot, aes(year, Emissions, col = fips)) +
     geom_line() +
     geom_point() +
     ggtitle(expression("Baltimore vs. Los Angeles")) +
     xlab("Year") +
     scale_colour_discrete(name = "City", labels = c("Los Angeles", "Baltimore"))+
     ylab(expression(~PM[2.5]~ "Motor Vehicle Emissions"))

dev.off()





