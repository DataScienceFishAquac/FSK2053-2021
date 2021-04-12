####### Data visualization using ggplot2

# We will work with the data of fishing vessels (tonnage and fishing hours per country)

################################################################################
#################### SLICE 1
################################################################################

library(tidyverse)

# We will load the dataset from the Github repository and we will tidy it following the instructions from Session 1
url <- "https://raw.githubusercontent.com/DataScienceFishAquac/FSK2053-2021/main/datasets/GFW-fishing-vessels-v2.csv"
dat_vessels <- read_csv(url)

# Now we will select the variables we want to keep and stored it in a wide_table format
wide_data_vessels <- dat_vessels %>% 
  select(mmsi,flag_gfw,vessel_class_gfw,tonnage_gt_gfw,paste0("fishing_hours_",2012:2020))
wide_data_vessels

# We are going to add the continent information. We can do this with the countrycode library
library(countrycode)
wide_data_vessels <- wide_data_vessels %>% mutate(continent = countrycode(wide_data_vessels$flag_gfw,origin="iso3c",destination="continent"))

# We will now tidy up the table, and will remove all observations with NA
tidy_data_vessels <- wide_data_vessels %>% 
  pivot_longer(names_to = "year",values_to = "fishing_hours", paste0("fishing_hours_",2012:2020)) %>%
  mutate(year = gsub("fishing_hours_","",year)) %>%
  drop_na()
tidy_data_vessels

################################################################################
#################### SLICE 2
################################################################################

###################################################
# Counts of total vessels per country 
###################################################

# We want to explore the data on how many vessels are included in the dataset for each country,
# We will use the continent as a factor to compare differences among countries.

# We could use count() on the tidy data table
vessel_count_tidy <- tidy_data_vessels %>% group_by(continent, flag_gfw) %>% count()

# However, if we use the long (tidy) dataset, there can be several lines for each ship. 
# We have to use the wide dataset, since it has a row for every vessel.
vessel_count <- wide_data_vessels %>% group_by(continent, flag_gfw) %>% count()

# Anyway, we want to  be sure that the identifiers of the vessels are unique, using distinct() on the wide dataset
unique_ships <- wide_data_vessels %>% select(mmsi) %>% distinct()
# The number of rows is the same than the original. So all vessel mmsi values are unique

###############################################################
# Plots with a single numeric variable and a categorical factor
###############################################################

# We are going to produce our first ggplot:

#### geom_point() (one numeric variable vs a categorical factor)
vessel_count %>%
  ggplot(aes(x=continent,y=n)) +
  geom_point()

# There is a country that has many more vessels than the others (possibly China). We can filter it out
vessel_count_no_china <- vessel_count %>% filter(n != max(vessel_count$n))

vessel_count_no_china %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_point()
# This looks better

# We can improve the visualization using geom_jitter and alpha colours:
vessel_count_no_china %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_jitter(width = 0.25, alpha = 0.4) 

# And we can add some colour
vessel_count_no_china %>% 
  ggplot(aes(x=continent,y=n, color=continent)) +
  geom_jitter(width = 0.25, alpha = 0.4, size = 3) 

################################################################################
#################### SLICE 3
################################################################################

# We can select boxplot geometry:
vessel_count_no_china %>% 
  ggplot(aes(x=continent,y=n, color=continent)) +
  geom_boxplot() 

# Compare to this
vessel_count_no_china %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_boxplot(aes(fill=continent)) 

# Or violin plots:
vessel_count_no_china  %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_violin(aes(fill=continent))  

# Perhaps it is a good idea to transform the y axis to log
vessel_count_no_china  %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_violin(aes(fill=continent)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

# So we can recover China again:
vessel_count  %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_violin(aes(fill=continent)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

vessel_count  %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_boxplot(aes(fill=continent)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

# We can add two layers of geometries to the same plot.
vessel_count  %>% 
  ggplot(aes(x=continent,y=n)) +
  geom_boxplot(aes(fill=continent)) + 
  geom_jitter(width = 0.25, alpha = 0.4, size = 2) + 
    scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

# We can reorder the x axis by the median value
vessel_count  %>% 
  ggplot(aes(x=reorder(continent,n,median),y=n)) +
  geom_boxplot(aes(fill=continent)) + 
  geom_jitter(width = 0.25, alpha = 0.4, size = 2) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 


################################################################################
#################### SLICE 4
################################################################################

##########################
### Barplots
##########################
## Barplots are useful to represent a summarizing variable for each group of the data.
# We want to generate a barplot of the average number of vessels per country included in our dataset 
# for every continent

# Now we will produce a barplot of the total number of vessels per continent
vessel_count %>% group_by(continent) %>% summarise(Total=sum(n)) %>% 
  ggplot(aes(x=reorder(continent,Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=continent)) 

# If we want horizontal bars, labels and log scale in the axis:
vessel_count %>% group_by(continent) %>% summarise(Total=sum(n)) %>% 
  ggplot(aes(x=reorder(continent,Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=continent),color="black") +
  coord_flip() +
  geom_text(aes(label=Total), hjust=-0.1, size=2.5) +
  scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) 

# If we want to add the error bars, we have to calculate the sd in the summarise function
vessel_count %>% group_by(continent) %>% summarise(Total=sum(n), sd=sd(n)) %>% 
  ggplot(aes(x=reorder(continent,Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=continent),color="black") +
  coord_flip() +
  geom_text(aes(label=Total), hjust=-1.2, size=2.5) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    geom_errorbar(aes(ymin=Total, ymax=Total+sd), width=.2)

################################################################################
#################### SLICE 5
################################################################################

###########################################################
## Plots with one numerical variable and an ordered factor
###########################################################
# We will return to the tidy dataset
### Line plots
# We will group by continent and will summarise the total sum of fishing hours
# We will plot the total hours by year, to see the temporal trend by continent

tidy_data_vessels %>% group_by(continent, year) %>% 
  summarise(Total_hours=sum(fishing_hours),sd=sd(fishing_hours),count=n()) %>%
  ggplot(aes(x=year,y=Total_hours)) +
  geom_line(aes(color=continent,group=continent))+
  scale_y_log10(
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))) 

################################################################################
#################### SLICE 6
################################################################################

###########################################################
## Plots with two numerical variables
###########################################################
#### Scatter plot
# We will calculate the total tonnage and the total fishing hours for each country 
# And we will represent this in a scatterplot
# As usual, we need to log-transform due to the values of China 
tidy_data_vessels %>% group_by(continent, flag_gfw) %>% 
  summarise(Total_hours=sum(fishing_hours),Total_tonnage=sum(tonnage_gt_gfw),count=n()) %>%
  ggplot(aes(x=Total_tonnage,y=Total_hours)) +
  geom_point() +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

# We will add colours by continent
tidy_data_vessels %>% group_by(continent, flag_gfw) %>% 
  summarise(Total_hours=sum(fishing_hours),Total_tonnage=sum(tonnage_gt_gfw),count=n()) %>%
  ggplot(aes(x=Total_tonnage,y=Total_hours)) +
  geom_point(aes(color=continent)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) 

################################################################################
#################### SLICE 7
################################################################################

#######################################################3
### Facet_wrap
# We will draw several panels depending on the values of a categorical variable
# Here the categorical variable is continent
tidy_data_vessels %>% group_by(continent, flag_gfw) %>% 
  summarise(Total_hours=sum(fishing_hours),Total_tonnage=sum(tonnage_gt_gfw),count=n()) %>%
  ggplot(aes(x=Total_tonnage,y=Total_hours)) +
  geom_point(aes(color=continent)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~ continent)

# Add a trend line (from a linear model):
tidy_data_vessels %>% group_by(continent, flag_gfw) %>% 
  summarise(Total_hours=sum(fishing_hours),Total_tonnage=sum(tonnage_gt_gfw),count=n()) %>%
  ggplot(aes(x=Total_tonnage,y=Total_hours)) +
  geom_point(aes(color=continent)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~ continent) +
  geom_smooth(formula = y ~ x, method="lm",color="black",size=0.5) 
   
################################################################################
#################### SLICE 8
################################################################################

######################################################
#### Distribution of a quantitative variable
######################################################
#### Histograms and density plots
# Now we want to explore how the fishing_time per vessel is distributed for the different continents
tidy_data_vessels  %>% 
ggplot(aes(x=log(fishing_hours))) +
  geom_histogram(aes(group=continent,fill=continent))+
  facet_wrap(~ continent) 
# The counts are very different for each continent. We would like to have independent scale in Y
tidy_data_vessels  %>% 
  ggplot(aes(x=log(fishing_hours))) +
  geom_histogram(aes(group=continent,fill=continent))+
  facet_wrap(~ continent,scales = "free_y") 

################################################################################
#################### SLICE 9
################################################################################

# We can smooth the curves using geom_density()
tidy_data_vessels  %>% 
  ggplot(aes(x=log(fishing_hours))) +
  geom_density(aes(group=continent, fill=continent), alpha=.5)+
  facet_wrap(~ continent,scales = "free_y") 

# And now we can explore the change over the years
tidy_data_vessels  %>% 
  ggplot(aes(x=log(fishing_hours))) +
  geom_density(aes(group=continent, fill=continent),alpha=0.5)+
  facet_wrap(~ year,scales = "free_y") 

# There are some apparent changes, but they are difficult to appreciate with these plots
# Let us change to geom_boxplot()
tidy_data_vessels  %>% 
  ggplot(aes(x=year)) +
  geom_boxplot(aes(y=log(fishing_hours),group=year, fill=continent), width=0.2)+
  geom_violin(aes(y=log(fishing_hours),group=year, fill=continent), alpha=.5)+
    facet_wrap(~ continent,scales = "free_y") 

################################################################################
#################### SLICE 10
################################################################################

######################################################
# Plots with three variables
######################################################

## Heatmaps
tidy_data_vessels  %>% group_by(continent,year) %>% 
  summarise(Total_hours=sum(fishing_hours)) %>%
  ggplot(aes(x=year,y=continent,fill=log(Total_hours))) +
  geom_tile() +
  scale_fill_gradient(low="yellow", high="red3") 

## Bubbleplot
tidy_data_vessels  %>% group_by(continent,year) %>% 
  summarise(Total_hours=sum(fishing_hours)) %>%
  ggplot(aes(x=year,y=continent)) +
  geom_point(aes(size=log(Total_hours),color=log(Total_hours))) +
  scale_color_gradient(low="skyblue", high="red3") +
  theme_classic()

######################################################
# Cumulative areas over time
######################################################

## Cummulative area plot
tidy_data_vessels  %>% group_by(continent,year) %>% 
  summarise(Total_hours=sum(fishing_hours)) %>%
  ggplot(aes(x=year,Total_hours)) +
  geom_area(aes(group=continent,fill=continent)) +
  theme_minimal()
