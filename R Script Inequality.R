# New packages
install.packages("forcats")
install.packages("reshape2")

# Load stats package
library(stats)

# Add dataframe
data <- X2019_data_for_R_UK

# Define variables
equality <- data$`Gender Equality Index`
work <- data$WORK
money <- data$MONEY
knowledge <- data$KNOWLEDGE
time <- data$TIME
power <- data$POWER
health <- data$HEALTH
country <- data$Country

# Multiple regression analysis
model <- lm(equality ~ work + money + knowledge + time + power + health)
summary(model)

# Load ggplot2
library(ggplot2)

# Create bar chart of GIE
barchart_GIE <- ggplot(data = data, aes(x=reorder(country, -equality), y=equality)) +
  geom_bar(colour="black", stat="identity", position="dodge", fill=factor(ifelse(country=="EU","black","seagreen4"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y= "Gender Equality Index Score", x = "Country")
barchart_GIE

# Create bar chart of Power
barchart_power <- ggplot(data = data, aes(x=reorder(country, -POWER), y=POWER)) +
  geom_bar(colour="black", stat="identity", position="dodge", fill=factor(ifelse(country=="EU","black","seagreen4"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y= "Power", x = "Country")
barchart_power

# % Women Parliament and Minister Barchart

# Prepare data in new dataframe
power <- subset(data, select = c("Country", "Share of ministers (%) W", "Share of members of parliament (%) W"))

# Transform to long form
# Install reshape2 package
library(reshape2)

# Transform
power_long <- melt(power,id.vars="Country")

# Reorder by Parliament
library(dplyr)
library(forcats)

power_long_parl <- mutate(power_long, Country = fct_relevel(Country, "SE", "FR", "FI", "ES", "AT", "NL", "DE", "BG", "DK", "PT", "SI", "UK", "EU", "LT", "BE", "LU", "IT", "RO", "LV", "CZ", "HR", "SK", "IE", "EE", "PL", "CY", "EL", "HU", "MT"))

# Barchart with Parliament and Ministers
library(ggplot2)
plot_min <- ggplot(power_long_parl,aes(x=Country,y=value,fill=factor(variable)))+
  geom_bar(stat="identity", position="dodge") +
  xlab("Country")+ylab("Percentage") +
  scale_fill_manual(values = c("seagreen", "black"), labels=c("Female ministers (%)", "Female parliament (%)")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  guides(fill=guide_legend(title=""))
plot_min

# Plot of difference between Parliament and Ministers
#Make new column
data$difference <- data$`Share of members of parliament (%) W` - data$`Share of ministers (%) W`

#Barchart
barchart_difference <- ggplot(data = data, aes(x=reorder(country, difference), y=difference)) +
  geom_bar(colour="black", stat="identity", position="dodge", fill=factor(ifelse(country=="EU","black","seagreen4"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y= "Female Parliament - Female Ministers (%)", x = "Country")
barchart_difference

# Add New column with ministerial 2022 data
data$Q12022 <- c(32.4, 53.3, 23.8, 16.7, 30, 47.1, 40, 26.7, 8.7, 60.9, 52.4, 27.8, 33.3, 25, 14.3, 46.7, 35.3, 14.3, 9.5, 50, 46.7, 9.1, 42.1, 4.5, 11.1, 18.8, 52.6, 52.2, 26.1)


# Barchart change
# New column with change
data$min_change <- data$Q12022 - data$`Share of ministers (%) W`

barchart_change <- ggplot(data = data, aes(x=reorder(country, -min_change), y=min_change)) +
  geom_bar(colour="black", stat="identity", position="dodge", fill=factor(ifelse(country=="EU","black","seagreen4"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y= "Change in % Female Ministers 2018/9 to 2022", x = "Country")
barchart_change

# Barchart of 2022 ministers

barchart_min2022 <- ggplot(data = data, aes(x=reorder(country, -Q12022), y= Q12022)) +
  geom_bar(colour="black", stat="identity", position="dodge", fill=factor(ifelse(country=="EU","black","seagreen4"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(y= "Female Ministers (%) Q1 2022", x = "Country")
barchart_min2022