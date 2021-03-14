#######################################################################################

# Lab 1: data wrangling and visualization
# Lily Cheng
# 10/3/2018
######################################################################################
# Packages needed: tidyverse
# Datasets needed: Nationals parks visualization Data.csv
######################################################################################

######################################################################################
# Part1. Loading the tidyverse
######################################################################################

library(tidyverse) # Load the tidyverse package

######################################################################################
# Part2. Read in National Parks data (csv)
######################################################################################

np_visits <- read_csv("National Parks Visitation Data.csv")
# View(np_visits)

######################################################################################
# Part3. Basic data frame exploration
######################################################################################

names(np_visits) # report all column names
dim(np_visits) # report all dimensions of the data frame
#np_names <- names(np_visits) 
head(np_visits,18) # report the first 18 lines
tail(np_visits, 14)#report the last 14 lines

######################################################################################
# Part4. Basic data wrangling in dplyr
######################################################################################

# select(): select specific columns of a data frame
# filter(): create subsets of ROWS based on condition
# arrange(): sort data (alphabetically and/or numerically)
# mutate(): add a new column based on calculation from exisiting column

# create a subset of the np_visits data frame that only has columns from Stats through YearRaw

df1 <- select(np_visits, State:YearRaw)
View(df1)

# Filter df1 to only retain observations for California National Parks for years since 1950

df2 <- filter(df1, State == "CA" & Type == "National Park" & YearRaw >= 1950)
View(df2)

# Arrange data alphabetically by park Code, and then numerically by YearRaw

df3 <- arrange(df2, Code, YearRaw)
View(df3)

#  Add a new column that is Visitors/1000 (so we can make a graph with thousands of visitors)

df4 <- mutate(df3, kVis = Visitors/1000)
View(df4)

# Use the filter function to only keep rows where the "YearRaw" column is NOT matching "Total"

df5 <- filter(df4, YearRaw != "Total")
View(df5)

# Coerce the YearRaw variable into a numeric class

df5$YearRaw <- as.numeric(df5$YearRaw)
# class(df5$YearRaw) # to read data format
 
######################################################################################
# Part5. More elegant code for data wrangling with piping
######################################################################################

# We use the pipe operator ( %>% ) (shortcut is command + shift + M)

# Let's say we want to do some wrangling on the original np_visits dataset, and we want to:

# 1. only select parks in UT that are national parks
# 2. select only columns names, type, visitors, and yearraw
# 3. add a column that is millions of visitors
# 4. arrange by name, then by millions of visitors column

utah_parks <- np_visits %>% 
  filter(State == "UT", Type == "National Park") %>% 
  select("Name", "Type", "Visitors", "YearRaw") %>% 
  mutate(m_vis = Visitors/1000000) %>% 
  arrange(Name, m_vis)

View(utah_parks)

######################################################################################
# Part6. Create a scatterplot of California NP visitation using the data frame
# we created (df5)
######################################################################################

# 1. Using ggplot
# 2. What data we're using, including what are our x & y variables (if relevant)
# 3. What type of graph we're creating

np_graph <- ggplot(df5, aes(x= YearRaw, y = kVis)) + 
  geom_point(aes(color = Code))+
  labs(x = "Year", y = "Annual Visitors (thousands)",
       title = "Lily Cheng")

# windows ()
quartz()
np_graph
