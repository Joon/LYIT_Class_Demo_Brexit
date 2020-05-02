# install.packages("mice")
library("mice")

# Load
brexit_raw <- read.csv("Data\\data_brexit_referendum.csv")

# Inspect
head(brexit_raw)

# Clean Leave variable -1 to NA
print(sprintf("Count of rows with -1 Leave Value: %s", length(brexit_raw$Leave[brexit_raw$Leave == -1])))
brexit_raw$Leave[brexit_raw$Leave == -1] <- NA

# Check cleaned Data
print(sprintf("Count of rows with NA Leave Value: %s", length(brexit_raw$Leave[is.na(brexit_raw$Leave)])))

md.pattern(brexit_raw, plot = TRUE)

# Calculate proportion of leave votes
brexit_raw$Proportion <- brexit_raw$Leave / brexit_raw$NVotes
brexit_raw$Proportion[is.na(brexit_raw$Leave)] <- 0

brexit_raw$Vote[brexit_raw$Proportion <= 0.5] <- 'Remain'
brexit_raw$Vote[brexit_raw$Proportion > 0.5] <- 'Leave'

brexit_raw$RegionName <- sapply(brexit_raw$RegionName, switch, 
       'London' = 'L', 
       'North West' = 'NW',
       'North East' = 'NE',
       'South West' = 'SW',
       'South East' = 'SE',
       'East Midlands' = 'EM',
       'West Midlands' = 'WM',
       'East of England' = 'EE',
       'Yorkshire and the Humber' = 'Y')

sapply(brexit_raw, summary)

# Rest of this file from here down was adapted from example code provided by 
# James Connolly (lecturer)

brexit_raw <- brexit_raw[complete.cases(brexit_raw),]

# Use double-tab for structure of sapply()
numeric_variable_list <- sapply(brexit_raw, is.numeric)
numeric_variable_list

# Remove the ID field as it has no meaning
numeric_variable_list["ID"] <- FALSE

# Then remove this again from the main file_data dataset
numerical_data <- brexit_raw[numeric_variable_list]
colnames(numerical_data)

# We can use the lapply() function to return a named list
# where each list member has a corresponding name
lapply(numerical_data, summary)

# Call cbind on each output from the summary function, which gives stats about the variable
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

alt_numerical_summary <-  do.call(rbind, lapply(numerical_data, summary))
alt_numerical_summary


# See the max proportion of leave votes
numerical_summary["Max.", "Proportion"]

# And the min proportion of leave votes
numerical_summary["Min.", "Proportion"]

# Check the variance
numerical_summary["Max.", "Proportion"] - numerical_summary["Min.", "Proportion"]

# Lets look at values of those who wanted to leave and stay
display_variables <- c("NoQuals", "Proportion", "AdultMeanAge", "L4Quals_plus", "RegionName")

# Values show that those with higher age and fewer qualifications
# voted to leave
brexit_raw[which.max(brexit_raw$Proportion), display_variables]

# Those who wanted to stay were younger with higher qualifications
brexit_raw[which.min(brexit_raw$Proportion), display_variables]

table(brexit_raw$RegionName)

prop.table(table(brexit_raw$RegionName))

barplot(height = prop.table(table(brexit_raw$RegionName)),
        main = "Vote proportion by Region", 
        ylab = "Frequency", 
        xlab = "Region",
        col = "white")

# chart where each point Each point represents a ward
# observation, and it shows the proportion of Leave votes for each ward, arranged in
# vertical lines corresponding to RegionName and coloured by the proportion of white
# population for each ward. As you can see, we have another interesting finding; it seems that
# the more diversified a ward's population is (seen in the darker points), the more likely it is
# for the ward to vote in favor of remaining in the EU (a lower Proportion value).

# Creates the plot
#install.packages("ggplot2")
# Colour each chart point witha colour palette
#install.packages("viridis")
library(ggplot2)
library(viridis)


plot <- ggplot(brexit_raw, aes(x = RegionName, y = Proportion, color = White))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))

hist(brexit_raw$NoQuals, main = "Histogram for NoQuals", xlab = "")
hist(brexit_raw$L4Quals_plus, main = "Histogram for L4 qualifications +", xlab = "")
hist(brexit_raw$MeanAge, main = "Histogram for mean adult age", xxlab = "")

# Now that we understand more about qualifications
# we'll have a look at the scatter plots for distribution
# both charts resemble each other
plot(x = brexit_raw$NoQuals, y = brexit_raw$AdultMeanAge, ylab = "Adult Mean age", xlab = "No qualifications")
plot(x = brexit_raw$L4Quals_plus, y = brexit_raw$AdultMeanAge, ylab = "Adult mean age", xlab = "L4 qualifications+")


variables_of_interest <- c("AdultMeanAge", "White", "Owned", "NoQuals", "L4Quals_plus", "Unemp", "HigherOccup", "Deprived", "Proportion")
pairs(brexit_raw[variables_of_interest])

# Now lets look at some charts in more detail
plot <- ggplot(brexit_raw, aes(x = NoQuals, y = AdultMeanAge, color = Proportion))
plot <- plot + stat_smooth(method = "lm", col = "darkgrey", se = FALSE)
plot <- plot + scale_color_viridis()
plot <- plot + geom_point()
print(plot)

