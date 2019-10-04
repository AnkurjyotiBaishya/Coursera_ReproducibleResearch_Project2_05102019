# QUESTIONS
#
#     1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
#                   are most harmful with respect to population health?
#     2. Across the United States, which types of events have the greatest economic consequences?
# 
# Consider writing your report as if it were to be read by a government or municipal manager who
# might be responsible for preparing for severe weather events and will need to prioritize resources for 
# different types of events. However, there is no need to make any specific recommendations in your report.

#######################################################################################################################################

#Reading and PreProcessing Data
weather <- read.csv(paste(getwd(),"/Raw Data/repdata_data_StormData.csv", sep = ""), header = TRUE, sep = ",")

#1: Event Type vs. Population Health
weather2 <- weather[,c(8,23,24)]

#collecting event type data for which casualities > 0
popu.damage <- data.frame(event = weather2[which(as.character(weather2$FATALITIES) != "0" | 
                                                   as.character(weather2$INJURIES) != "0"),1],
                            fatalities = weather2[which(as.character(weather2$FATALITIES) != "0"| 
                                                   as.character(weather2$INJURIES) != "0"),2],
                            injuries = weather2[which(as.character(weather2$FATALITIES) != "0"  | 
                                                        as.character(weather2$INJURIES) != "0"),3])

#grouping and calculating total damage according to event type
popu.damage2 <- aggregate(popu.damage[,2:3], by = list(popu.damage$event), FUN = sum)
colnames(popu.damage2) <- c("Events", "Fatalities", "Injuries")
summary(popu.damage2)

#Number of disasters is too large to plot, and the mean is very skewed. Therefore, we select
# a few major disasters, with a large number of casualities
major.disasters <- popu.damage2[which(popu.damage2$Fatalities >= 200 & popu.damage2$Injuries >= 200),]
major.disasters$Total <- major.disasters$Fatalities + major.disasters$Injuries


#Plotting
library(ggplot2)
png("plot1.png", height = 550, width = 550)
ggplot(major.disasters, aes(Events)) +
  geom_bar(aes(y = Injuries/10000, fill = "Injuries"), stat = "Identity") +               #for Injuries
  geom_bar(aes(y = Fatalities/10000, fill = "Fatalities"), stat = "Identity") +           #for Fatalities
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +                 #rotating axis labels
  labs(y = "Count (x10^4)", title = "Most Harmful Weather Events \non Population Health in the US") + 
  scale_fill_discrete(name = "Type of Casuality") +                                       #editing legend title
  theme(plot.title = element_text(hjust = 0.5))                                           #aligning plot title
dev.off()

#summary of data
max1.A <- major.disasters[which.max(major.disasters$Injuries),1]
total1.A <- major.disasters[which(major.disasters$Events == max1.A),3]
max2.A <- major.disasters[which.max(major.disasters$Fatalities),1]
total2.A <- major.disasters[which(major.disasters$Events == max2.A),2]
overall.A <- major.disasters[which.max(major.disasters$Total),1]
overall.total.A <- major.disasters[which(major.disasters$Events == overall.A),4]


print(paste("The Event Type causing Maximum no. of Injuries in the US is ", max1.A, ". It caused a total of ", total1.A, " injuries over the years.", sep = ""))
print(paste("The Event Type causing Maximum no. of Deaths in the US is ", max2.A, ". It caused a total of ", total2.A, " deaths over the years.", sep = ""))
print(paste("The Event Type causing Maximum Damage to Human Population in the US is ", overall.A, ". It caused a total of ", overall.total.A, " casualities over the years.", sep = ""))


#######################################################################################################################################

#2. Event Type vs. Economic Consequences
weather3 <- weather[,c(8,25,27)]

#collecting event type data for which casualities > 0
econ.damage <- data.frame(event = weather3[which(weather3$PROPDMG != 0 | 
                                                   weather3$CROPDMG != 0),1],
                          prop.dmg = weather3[which(weather3$PROPDMG != 0| 
                                                   weather3$CROPDMG != 0),2],
                          crop.dmg = weather3[which(weather3$PROPDMG != 0 | 
                                                   weather3$CROPDMG != 0),3])

#grouping and calculating total damage according to event type
econ.damage2 <- aggregate(econ.damage[,2:3], by = list(econ.damage$event), FUN = sum)
colnames(econ.damage2) <- c("Events", "Property_Damage", "Crop_Damage")
summary(econ.damage2)

#Number of disasters is too large to plot, and the mean is very skewed. Therefore, we select
# a few major disasters, with a large number of casualities
major.disasters2 <- econ.damage2[which(econ.damage2$Property_Damage >= 50000 & econ.damage2$Crop_Damage >= 10000),]
major.disasters2$Total <- major.disasters2$Property_Damage + major.disasters2$Crop_Damage

#Plotting
png("plot2.png", height = 550, width = 550)
ggplot(major.disasters2, aes(Events)) +
  geom_bar(aes(y = Property_Damage/1000000, fill = "Property Damage"), stat = "Identity") +               #for Injuries
  geom_bar(aes(y = Crop_Damage/1000000, fill = "Crop Damage"), stat = "Identity") +           #for Fatalities
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +                 #rotating axis labels
  labs(y = "Count (x10^6)", title = "Most Destructive Weather Events \nfor the Economy (Crop and Property) in the US") + 
  scale_fill_discrete(name = "Type of Damage") +                                       #editing legend title
  theme(plot.title = element_text(hjust = 0.5))                                           #aligning plot title
dev.off()

#summary of data
max1.B <- major.disasters2[which.max(major.disasters2$Property_Damage),1]
total1.B <- major.disasters2[which(major.disasters2$Events == max1.B),2]
max2.B <- major.disasters2[which.max(major.disasters2$Crop_Damage),1]
total2.B <- major.disasters2[which(major.disasters2$Events == max2.B),3]
overall.B <- major.disasters2[which.max(major.disasters2$Total),1]
overall.total.B <- major.disasters2[which(major.disasters2$Events == overall.B),4]

print(paste("The Event Type causing maximum Property Damage in the US is ", max1.B, ". It caused a total of ", total1.B, " worth of damage over the years.", sep = ""))
print(paste("The Event Type causing maximum Crop in the US is ", max2.B, ". It caused a total of ", total2.B, " worth of damage over the years.", sep = ""))
print(paste("The Event Type causing Maximum Damage to the Economy in the US is ", overall.B, ". It caused a total of ", overall.total.B, " worth of damage over the years.", sep = ""))

#######################################################################################################################################

