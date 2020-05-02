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
