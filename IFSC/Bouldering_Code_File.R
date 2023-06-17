#Harvardx Certificate in Data Science Course 9 Choose Your Own Project

#NOTE: While (almost) all of the code included here does appear in the associated Rmd and PDF files, the exact
  #ordering of the code may vary. Similarly, the comments included here are meant to guide readers but 
  #are not necessarily written with a report-specific level of formalness. As such, many of the comments
  #have been rewritten in the final report Rmd and PDF files. 

#SETTING UP THE LIBRARIES 

#Installing all of the libraries/packages that will be used here.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

#Calling all of the relevant libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(gridExtra)
library(ggrepel)
library(randomForest)
library(ggridges)
library(knitr)
library(kableExtra)

#GOAL OF THE THIS REPORT: Predict whether a given climber could have won finals. I will determine this by
  #considering those who qualified for finals and completed at least one zone as people who could have won.
  #All other people are to be considered as non-contenders.

#OVERVIEW OF CODE: 
  #(1) Reading in data from GitHub
  #(2) Cleaning the data and creating the data frame to be used for analysis & visualization
  #(3) Exploring the data set via visualization and summary statistics
  #(4) Building classification models to predict potential winners.
  #(5) Reporting the results of the models.
  #(6) Concluding remarks (improvements and possible issues)


#PREPARING THE INITITIAL DATA SET

#I am using data about Bouldering Competitions held by the International Federation of
  #Sport Climbing (IFSC). Though the original data source on Kaggle also has information
  #for Lead, Speed, and Combined-Format climbing, I am limiting this project to bouldering for
  #two reasons:
    #(1): Each type of climbing (except for combined) requires a distinct skill set. Though there 
      #is significant overlap in the training and skills between disciplines, the differences make
      #comparing the performance of athletes across disciplines essentially speculative.
    #(2): Many of the climbers only participate in one discipline. Since I have no non-speculative means
      #of comparing performances across disciplines, I would have to exclude too many athletes in any
      #combined format to maintain a sufficiently large data set.
#Thus, I am limiting my focus to bouldering alone. That said, if the goal were to be
  #broadened, it would be feasible to make predictions about which country's athletes are most likely to 
  #win particular medals (the IFSC awards a Gold, Silver, and Bronze medal for each discipline) in general.
  #For such an analysis, there would not be a need to convert the quality of one athletes performance in 
  #discipline A to another's in discipline B.

set.seed(1917, sample.kind = "Rounding")

#Reading in the data file from my GitHub (boulder_results.csv)
url_name <- "https://raw.githubusercontent.com/tchang343/IFSC/main/boulder_results.csv"
temp_table <- read.table(file = url_name, header = TRUE, sep = ",")

#NOTE: The data set used here originates from Kaggle:
  #URL:https://www.kaggle.com/datasets/brkurzawa/ifsc-sport-climbing-competition-results

#Checking the current dimensions of the table
dim(temp_table) #This should be 5535 rows and 13 columns

#Checking for NA values in the table
any(is.na(temp_table)) #There are currently NA values in the table.

#CLEANING THE DATA AND MAKING THE FINAL VERSION OF THE TABLE

#Getting rid of unneeded columns (Qualification.1, Qualification.2, and Category)
  #Category is being removed since all of the data is in the bouldering category.
temp_table_rev <- temp_table %>%
  select(Competition.Title, Competition.Date, FIRST, LAST, Nation, StartNr, Rank, 
         Qualification, Semifinal, Final)

#NOTE: Qualification.1 and Qualification.2 are empty for all rows. Since the data set repository on Kaggle,
  #from which the data used here is sourced, also includes data for lead/sport climbing, where the 
  #qualification round is composed of two routes instead of a single round, Qualification.1 and
  #Qualification.2 are essentially carry-overs from a different data set.

#Checking the dimensions of temp_table_rev
dim(temp_table_rev) #This is 5535 x 10.

#NOTES ON HOW TO READ THE CURRENT TABLE
  #StartNr is the order in which the climber first came out. The creator of the data set (Brett Kurzawa)
    #has warned that he is unsure of the accuracy of this information so it may be excluded later on.
  #Rank is a given climber's rank at the competition.
  #For Qualification, Semifinal, and Final, the formatting goes as follows:
    #The first number (before the T) is the number of climbs completed ("tops").
    #The second number (after T and before Z) is the number of zones reached.
    #The final number is read as follows: Example: 54 means 5 attempts to get the tops and 4 attempts to 
      #get the zones.

#Checking for any NA values and if there are any, finding which columns have them.
na_table <- data.frame(Column_name = "Competition.Title",
                       NAs = any(is.na(temp_table_rev$Competition.Title))) #Setting up a table to show which columns have NA values.
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Competition.Date",
                                 NAs = any(is.na(temp_table_rev$Competition.Date))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "FIRST",
                                 NAs = any(is.na(temp_table_rev$FIRST))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "LAST",
                                 NAs = any(is.na(temp_table_rev$LAST))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Nation",
                                 NAs = any(is.na(temp_table_rev$Nation))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "StartNr",
                                 NAs = any(is.na(temp_table_rev$StartNr))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Rank",
                                 NAs = any(is.na(temp_table_rev$Rank))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Qualification",
                                 NAs = any(is.na(temp_table_rev$Qualification))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Semifinal",
                                 NAs = any(is.na(temp_table_rev$Semifinal))))
na_table <- bind_rows(na_table,
                      data.frame(Column_name = "Final",
                                 NAs = any(is.na(temp_table_rev$Final))))
na_table #This shows that StartNr and Rank have missing values.

#Determining how many NAs there are in StartNr and Rank
sum(is.na(temp_table_rev$StartNr)) #There are 37 missing starting numbers.
sum(is.na(temp_table_rev$Rank)) #There is one missing rank.

#Locating the missing rank.
which(is.na(temp_table_rev$Rank)) #This is the 5535th item (so last row of the table).

#Since I have no reasonable way of determining what an appropriate replacement value would be for
  #the missing rank (the recording of the competition is no longer publicly available and the IFSC
  #website does not include this data in its current iteration), I will just get rid of the last row
  #in the table.
temp_table_rev <- temp_table_rev[-5535,]

#Just to confirm that this has worked as intended, I will check again for NAs in the Rank column and 
  #reconfirm the dimensions of the table.
dim(temp_table_rev) #This is 5534 x 10, as expected.
any(is.na(temp_table_rev$Rank)) #This is now FALSE so the Rank column is good to go.

#Now, I will locate the NA values in the StartNr column.
which(is.na(temp_table_rev$StartNr))

#The NA values in the StartNr are held between (inclusively) rows 1768-1785 and 4808-4825. So, let's take
  #a look at these rows
temp_table_rev[c(1768:1785),]
temp_table_rev[c(4808:4825),]

#In addition to observing that there are indeed NA values in the StartNr column, we can see that not all
  #competitions include a semifinals round (as indicated by the blank spaces). 

#As with the missing rank, I would need access to either the recordings of the competitions or the IFSC-held
  #data in order to replace the NA values with appropriate values. Since I do not have access to either
  #source of information, I will just exclude these rows. Since that will still leave me with over 5000 rows,
  #I imagine (hope) doing so will not have a large impact on the data.
temp_table_rev <- temp_table_rev[-which(is.na(temp_table_rev$StartNr)),]

#Now, I will check the StartNr column again for NA values and confirm the table's dimensions.
any(is.na(temp_table_rev$StartNr))
dim(temp_table_rev) #Should be 5498 x 10

#Before proceeding onto making the aforementioned new columns, I will check the whole table for NA values.
any(is.na(temp_table_rev))

#I will now handle the Qualification, Semifinals, and Finals columns and convert the data into a more 
  #usable format. 

#As a first step, I am going to replace all of the missing values with the following: 0T0z00. If a person
  #did not qualify for an advanced round or if they did not complete any tops or zones in a given round,
  #their score would be 0T0z00. Once this is converted over to the above mentioned format, this will be
  #recorded as a single 0.
temp_table_rev[temp_table_rev == ""] <- "0T0z00"

#Checking some of the previously blank spaces to confirm correct replacement.
temp_table_rev[c(4808:4817),]

#NOTE: Semifinals usually consists of approximately 40 people total (Women and men) from 
  #approximately 120-160 people in qualifications. This is further shrunk to approximately 
  #6 men and 6 women for finals. So, it is to be expected that most of the elements in the 
  #Semifinal and Final columns will be 0T0z00.

#The next task is to convert the data points that are currently formatted as 3T2z89 or similar into 
  #more usable formats, as described below:

#(1) Total_Tops: This will count how many boulders were completed, regardless of round.
#(2) Total_Zones: This will count how many zones were reached, regardless of round.
#(3) Total_Top_Attempts: This will record how many attempts were needed to achieve the tops.
#(4) Total_Zone_Attempts: This will record how many attempts were needed to reach the zones.

#I will also include the round specific data as separate columns (i.e. tops in semifinals and attempts
  #to get tops in semifinals will be two separate columns).

#To do this, I will use the stringr library, which is included in the tidyverse, to extract the 
  #relevant information for each of the above mentioned metrics.
#Since I will repeatedly reference the Qualification, Semifinal, and Final columns, I will begin by
#designating a separate variable for each.
quali <- temp_table_rev$Qualification 
semi <- temp_table_rev$Semifinal
fin <- temp_table_rev$Final

#STEP 1: Getting the total number of tops for each climber at each competition

#Separating out the number of tops for each climber in the qualification round
quali_tops <- sapply(quali, function(x){
  quali_top <- str_extract(x, pattern = "\\d")
  as.numeric(quali_top)
})

#Same idea but for semifinals
semi_tops <- sapply(semi, function(x){
  semi_top <- str_extract(x, "\\d")
  as.numeric(semi_top)
})

#Same idea but for finals
fin_tops <- sapply(fin, function(x){
  fin_top <- str_extract(x, "\\d")
  as.numeric(fin_top)
})

#Calculating the total number of tops for each climber at each competition.
total_tops <- quali_tops + semi_tops + fin_tops

#Adding the total_tops vector to the table as a new column
temp_table_rev <- temp_table_rev %>%
  mutate(Total_Tops = total_tops)

#Adding quali_tops, semi_tops, and fin_tops lists to the table as new columns.
temp_table_rev <- temp_table_rev %>%
  mutate(Qualification_Tops = quali_tops,
         Semifinal_Tops = semi_tops,
         Final_Tops = fin_tops)

#Confirming that the tops column was added correctly.
head(temp_table_rev)

#STEP 2: Getting the total number of zones for each climber at each competition

#Getting the number of zones in qualifications for each climber at each competition.
quali_zones <- sapply(quali, function(q){
  temp <- str_extract(q, "T.")
  quali_zone <- str_sub(temp, 2)
  as.numeric(quali_zone)
})

#Number of zones in semifinals for each climber at each competition.
semi_zones <- sapply(semi, function(q){
  temp <- str_extract(q, "T.")
  semi_zone <- str_sub(temp, 2)
  as.numeric(semi_zone)
})

#Number of zones in finals for each climber at each competition.
fin_zones <- sapply(fin, function(q){
  temp <- str_extract(q, "T.")
  fin_zone <- str_sub(temp, 2)
  as.numeric(fin_zone)
})

#Finding the total number of zones per competition for each climber
total_zones <- quali_zones + semi_zones + fin_zones

#Adding the total_zones vector to the table as a new column
temp_table_rev <- temp_table_rev %>%
  mutate(Total_Zones = total_zones)

#Adding quali_zones, semi_zones, and fin_zones as new columns in the table.
temp_table_rev <- temp_table_rev %>%
  mutate(Qualification_Zones = quali_zones,
         Semifinal_Zones = semi_zones,
         Final_Zones = fin_zones)

#Looking at the start of the table now.
head(temp_table_rev)

#STEPS 3 and 4: Getting the total number of attempts for tops and zones for each 
  #climber at each competition. 
#NOTE: I have grouped these steps together since I will start by isolating the final two to four
  #characters in each string (this varies depending on how many attempts were required).
#NOTE: It is impossible to receive credit for a top without getting credit for a zone. Thus, the number of 
  #zones will never be smaller than the number of tops, meaning if there are 3 digits following the "z" in
  #the score, it must be that the first digit is the number of attempts for the tops and the remaining two
  #digits are the attempts for the zones.

#Before separating out the attempts from the original strings, I will define a function that performs
  #different string wrangling processes depending on the number of characters in the substring taken from
  #each climber's performance record at each competition. 

#This version of the function is for attempts required to get the tops.
top_att_splitr <- function(v){
  if(nchar(v) == 2){
    top_att <- str_extract(v, "\\d")
  }
  else if(nchar(v) == 3){
    top_att <- str_extract(v, "\\d")
  }
  else if(nchar(v) == 4){
    top_att <- str_sub(v, 1, 2)
  }
  return(top_att)
}

#Number of attempts for tops in the qualification round for each climber at each competition.
quali_top_attempts <- sapply(quali, function(q){
  temp <- str_sub(q, 5)
  quali_top_attempt <- top_att_splitr(temp)
  as.numeric(quali_top_attempt)
})

#Number of attempts for tops in the semifinal round for each climber at each competition.
semi_top_attempts <- sapply(semi, function(q){
  temp <- str_sub(q, 5)
  semi_top_attempt <- top_att_splitr(temp)
  as.numeric(semi_top_attempt)
})

#Number of attempts for tops in the final round for each climber at each competition.
final_top_attempts <- sapply(fin, function(q){
  temp <- str_sub(q, 5)
  fin_top_attempt <- top_att_splitr(temp)
  as.numeric(fin_top_attempt)
})

#Determining the total number of attempts required for the tops for each climber at each competition.
total_top_attempts <- quali_top_attempts + semi_top_attempts + final_top_attempts

#Adding total_top_attempts to the table as a new column.
temp_table_rev <- temp_table_rev %>%
  mutate(Total_Attempts_to_Top = total_top_attempts)

#Adding quali_top_attempts, semi_top_attempts, and final_top_attempts as new columns.
temp_table_rev <- temp_table_rev %>%
  mutate(Qualification_Top_Attempts = quali_top_attempts,
         Semifinal_Top_Attempts = semi_top_attempts,
         Final_Top_Attempts = final_top_attempts)

#Taking a look at the start of the table.
head(temp_table_rev)

#Confirming that no NA values are present
any(is.na(temp_table_rev))

#I am now going to essentially the same process to get the attempts for the zones. 

#A new function must be defined (same idea as before but it must now address the zone attempts).
zone_att_splitr <- function(u){
  if(nchar(u) == 2){
    zone_att <- str_sub(u, 2)
  }
  else if(nchar(u) == 3){
    zone_att <- str_sub(u, 2)
  }
  else if(nchar(u) == 4){
    zone_att <- str_sub(u, 3)
  }
  return(zone_att)
}

#Number of attempts for zones in the qualification round for each climber at each competition.
quali_zone_attempts <- sapply(quali, function(q){
  temp <- str_sub(q, 5)
  quali_zone_attempt <- zone_att_splitr(temp)
  as.numeric(quali_zone_attempt)
})

#Number of attempts for zones in the semifinal round for each climber at each competition.
semi_zone_attempts <- sapply(semi, function(q){
  temp <- str_sub(q, 5)
  semi_zone_attempt <- zone_att_splitr(temp)
  as.numeric(semi_zone_attempt)
})

#Number of attempts for zones in the final round for each climber at each competition.
final_zone_attempts <- sapply(fin, function(q){
  temp <- str_sub(q, 5)
  final_zone_attempt <- zone_att_splitr(temp)
  as.numeric(final_zone_attempt)
})

#Determining the total number of attempts required for the zones for each climber at each competition.
total_zone_attempts <- quali_zone_attempts + semi_zone_attempts + final_zone_attempts

#Adding total_top_attempts to the table as a new column.
temp_table_rev <- temp_table_rev %>%
  mutate(Total_Attempts_to_Zone = total_zone_attempts)

#Adding quali_zone_attempts, semi_zone_attempts, and final_zone_attempts to the table as new columns.
temp_table_rev <- temp_table_rev %>%
  mutate(Qualification_Zones_Attempts = quali_zone_attempts,
         Semifinal_Zones_Attempts = semi_zone_attempts,
         Final_Zones_Attempts = final_zone_attempts)

#Since I will be grouping the data during the analysis step by climber name, I am going to make a new
  #column with the first and last names put together. First, however, I am going to change the last
  #names to only have a capital first letter (instead of every letter being capitalized).
temp_table_rev <- temp_table_rev %>%
  mutate(Last2 = str_to_title(LAST)) %>%
  mutate(Name = str_c(FIRST, Last2, sep = " "))

#Confirming that no NA values are present
any(is.na(temp_table_rev))

#We now have all of the relevant basic data to be used in a readable and usable form. The only remaining
  #step before moving onto data exploration is to remove unneeded columns.
bouldering <- temp_table_rev %>%
  select(Competition.Title, Competition.Date, Name, Nation, StartNr, Rank,
         Total_Tops, Total_Zones, Total_Attempts_to_Top, Total_Attempts_to_Zone,
         Qualification_Tops, Qualification_Zones, Qualification_Top_Attempts,
         Qualification_Zones_Attempts, Semifinal_Tops, Semifinal_Zones, 
         Semifinal_Top_Attempts, Semifinal_Zones_Attempts, Final_Tops,
         Final_Zones, Final_Top_Attempts, Final_Zones_Attempts) %>%
  rename(Competition = Competition.Title, Date = Competition.Date)

#I am now going to make a final column called winner_contender. This column will consist of only 0s and 1s.
  #If a climber made finals and completed at least one zone in finals (since having any tops necessitates
  #having at least one zone), I will assign them a 1. Otherwise, they get a 0.
win_con <- sapply(fin_zones, function(q){
  if(q >= 1){
    win_con = 1
  }
  else{
    win_con = 0
  }
  return(win_con)
})

#Adding winner_contender as a new column in the bouldering table.
bouldering <- bouldering %>%
  mutate(Winner_Contender = win_con)

#NOTE: I am ultimately going to predict whether a given climber would have a 0 or 1 in the Winner_Contender
  #column. 

#Taking a final look at the start of the table
dim(bouldering) #This should be 5498 x 23
head(bouldering)

#EXPLORING THE BOULDERING DATA 

#Looking at some of the basic statistics for Tops, Zones, and attempts
bouldering %>% summary()

#We can see that the majority of climbers do not complete most of the bouldering problems at any
  #given competition (mean is 2.34 and median is 1 out of a possible 12). Zone performance is nontrivially
  #higher (mean is 3.495 and median is 3 out of 12).

#It might be interesting to see how the ratio of successes:attempts varies between zones and tops.
top_att_avg <- mean(bouldering$Total_Attempts_to_Top) / mean(bouldering$Total_Tops)
zone_att_avg <- mean(bouldering$Total_Attempts_to_Zone) / mean(bouldering$Total_Zones)
quali_top_att_avg <- mean(bouldering$Qualification_Top_Attempts) / mean(bouldering$Qualification_Tops)
quali_zone_att_avg <- mean(bouldering$Qualification_Zones_Attempts) / mean(bouldering$Qualification_Zones)
semi_top_att_avg <- mean(bouldering$Semifinal_Top_Attempts) / mean(bouldering$Semifinal_Tops)
semi_zone_att_avg <- mean(bouldering$Semifinal_Zones_Attempts) / mean(bouldering$Semifinal_Zones)
fin_top_att_avg <- mean(bouldering$Final_Top_Attempts) / mean(bouldering$Final_Tops)
fin_zone_att_avg <- mean(bouldering$Final_Zones_Attempts) / mean(bouldering$Final_Zones)

#Making a table to show the comparison
att_avg_table <- data.frame(Category = "Overall Attempts to Top",
                            Ratio = top_att_avg)
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Overall Attempts to Zone",
                                      Ratio = zone_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Qualification Attempts to Top",
                                      Ratio = quali_top_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Qualification Attempts to Zone",
                                      Ratio = quali_zone_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Semifinal Attempts to Top",
                                      Ratio = semi_top_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Semifinal Attempts to Zone",
                                      Ratio = semi_zone_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Final Attempts to Top",
                                      Ratio = fin_top_att_avg))
att_avg_table <- bind_rows(att_avg_table,
                           data.frame(Category = "Final Attempts to Zone",
                                      Ratio = fin_zone_att_avg))
att_avg_table %>% knitr::kable()

#We can see from the table that the qualification round tends to be the easiest to get zones and tops in. 
  #During qualifications, the average number of attempts to get a top and a zone are 1.71 and 1.93, 
  #respectively. We can also see that semifinals is usually harder than finals, with average attempts
  #coming in at 2.34 for tops (vs. 2.26 in finals) and 2.54 for zones (vs. 2.27 in finals).

#Checking how many distinct countries, athletes, and competitions are in the table
bouldering %>% summarize(Number_of_Countries = n_distinct(Nation),
                         Number_of_Athletes = n_distinct(Name),
                         Number_of_Competitions = n_distinct(Competition))

#So there are 64 countries, 1518 athletes, and 22 competitions.

#Let's start with some visualizations.

#NOTE: I am making each plot a permanent object so that I can reference them later as needed (and in some
  #cases, display multiple plots side by side).

#Plot 1: Distribution of the number of total tops
plot1 <- bouldering %>%
  ggplot(aes(Total_Tops)) +
  geom_bar(color = "black", fill = "blue") +
  ggtitle("Distributions of Total Tops at a Single Competition") +
  xlab("Number of Tops")

plot1 

#We can see that a large number of athletes did not top any the bouldering problems. To get a 
  #better idea of the distribution, consider the following statistics:
#Proportion of climbers (where a climber is limited to a single competition, i.e. the same climber at
  #two different competitions is counted as two separate data points) who topped each possible number of
  #boulders (0 to 12).
bouldering %>%
  group_by(Total_Tops) %>%
  summarize(prop_top = n()/nrow(bouldering)) %>%
  arrange(desc(prop_top))

#So, 43.8% of climbers (as defined above) never top a boulder and only 0.4% of climbers topped all of
  #the boulders at a given competition. We also see a cluster of similar proportions for those with 1-4 tops
  #and for those with 7-9 tops. This is somewhat to be expected; those who make it through qualifications
  #had to top at least 1 boulder. From there, those who made finals would (generally) have had to top at 
  #least one more boulder and generally topped most, if not all, qualifcation boulders.

#Let's now look at the distribution of total zones.
plot2 <- bouldering %>%
  ggplot(aes(Total_Zones)) +
  geom_bar(color = "black", fill = "blue") +
  ggtitle("Distribution of Total Zones at a Single Competition") +
  xlab("Number of Zones")

plot2 

#Again, let's get more numeric data by computing the proportions for each possible number of zones.
bouldering %>% 
  group_by(Total_Zones) %>%
  summarize(prop_zone = n()/nrow(bouldering)) %>%
  arrange(desc(prop_zone))

#NOTE: While there are only 12 possible tops, there are 16 possible zones.
#As before, 0 is the most common number of zones at 37.1% but this is notably lower than the percentage
  #of climbers with 0 tops. This is to be expected since tops require zones but zones do not require tops.
  #Interestingly, 4 zones is the next most common number of zones at 9.06% and those with only a single
  #top are less common than those with 0, 4, 3, 2, 5, 6, 7, 8, 11, and 12 zones. This, alongside a 
  #plot that appears less consistently decreasing than the plot for tops, suggests that the number of zones
  #and the number of tops may have distinct effects on any predictions made based (at least in part) on them.

#Now, instead of looking at overall performances, let's see how the distributions of tops and zones change
  #between rounds (r_comp_plot is short for round comparison plot):
#Table used for the tops by round comparison chart.
tops_by_round <- data.frame(round = c(rep("Qualification", 5498),
                                      rep("Semifinals", 5498),
                                      rep("Finals", 5498)), #the 5498 is just the number of rows for each round.
                            values = c(quali_tops, semi_tops, fin_tops))

#Plotting the distribution of tops by round.
r_top_comp_plot <- tops_by_round %>%
  ggplot(aes(x = values, y = round, fill = round)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Distribution of Tops by Round")
r_top_comp_plot

#Now, I will do the same for zones.
zones_by_round <- data.frame(round = c(rep("Qualification", 5498),
                                       rep("Semifinals", 5498),
                                       rep("Finals", 5498)),
                             values = c(quali_zones, semi_zones, fin_zones))
r_zone_comp_plot <- zones_by_round %>%
  ggplot(aes(x = values, y = round, fill = round)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +
  theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  ggtitle("Distribution of Zones by Round")
r_zone_comp_plot

#Let's now look for potential country-related bias by seeing what the average number of tops are by country.
bouldering %>%
  group_by(Nation) %>%
  summarize(avg_tops = mean(Total_Tops)) %>%
  arrange(desc(avg_tops)) 

#We can see that French, Slovenian, and Austrian athletes are the only three groups who average 4 or more 
  #tops per competition. Notably, France, the country with the highest average tops, has a significant lead 
  #of approximately 0.83 over Slovenia, the second highest. 

#Let's see if a similar distribution holds for zones.
bouldering %>%
  group_by(Nation) %>%
  summarize(avg_zones = mean(Total_Zones)) %>%
  arrange(desc(avg_zones))
  
#As with tops, France, Slovenia, and Austria have the highest averages for zones, with France again holding
  #a large advantage over the others.

#My use of the mean for this initial analysis may not be giving the best picture of the actual qualities of
  #each country's climbers. Since not every climber is equally skilled, an especially strong or weak climber
  #can make a major impact on the country's overall average. Similarly, the number of athletes from each 
  #country is not equal, as seen by:
bouldering %>% 
  group_by(Nation) %>%
  summarize(number_of_athletes = n()) %>%
  arrange(desc(number_of_athletes))

#Instead of using the means, I will now consider the medians and compare them to the means.
mean_med_comp_table <- bouldering %>%
  group_by(Nation) %>%
  summarize(avg_tops = mean(Total_Tops),
            avg_zones = mean(Total_Zones),
            med_tops = median(Total_Tops),
            med_zones = median(Total_Zones)) %>%
  arrange(desc(avg_tops))

head(mean_med_comp_table)

#Let's plot the median tops and zones
plot3 <- mean_med_comp_table %>%
  ggplot(aes(med_tops)) +
  geom_bar(color = "black", fill = "red") +
  ggtitle("Distribution of Median Number of Tops by Country") +
  labs(x = "Median Number of Tops", y = "Number of Countries")

plot4 <- mean_med_comp_table %>%
  ggplot(aes(med_zones)) +
  geom_bar(color = "black", fill = "red") +
  ggtitle("Distribution of Median Number of Zones by Country") +
  labs(x = "Median Number of Zones", y = "Number of Countries")

#Let's look at the two plots thus far together.
grid.arrange(plot3, plot4, ncol = 2, nrow = 1)

#Since the raw means and medians for tops and zones tell somewhat varying stories, perhaps a better metric
  #would be to see how the efficiency (tops/attempts and zones/attempts) compare by country.
  
#Setting up a new table for efficiency rates
efficiency_table <- bouldering %>%
  group_by(Nation) %>%
  summarize(top_eff = mean(Total_Tops) / mean(Total_Attempts_to_Top),
            zone_eff = mean(Total_Zones) / mean(Total_Attempts_to_Zone))
#There are NaN where no one topped or got a zone since that causes a 0/0 issue. So, I'll replace the NaN
  #values with a 0.
efficiency_table$top_eff[which(is.nan(efficiency_table$top_eff))] <- 0
efficiency_table$zone_eff[which(is.nan(efficiency_table$zone_eff))] <- 0

#Checking that there are no more NaN values and showing the start of the table in descending order.
any(is.na(efficiency_table))

#Sorting the efficiency table in terms of highest to lowest efficiency at getting tops.
efficiency_table <- efficiency_table %>%
  arrange(desc(top_eff))

head(efficiency_table)

#This shows that North Macedonia (MKD), Taiwan (TPE), Nepal (NEP), and Macao (MAC) all are more efficient
  #than France, Norway, Austria, and other countries that placed much higher in terms of mean and median
  #numbers of tops and zones. This is likely due to the current efficiency_table not considering how many
  #tops or zones a country typically receives. To see the whole distributions, consider the following plots:
plot5 <- efficiency_table %>%
  ggplot(aes(Nation, top_eff, label = Nation)) +
  geom_point(size = 2, color = "red") +
  ggtitle("Top Efficiency") +
  ylab("Efficiency Rate") +
  geom_text_repel(max.overlaps = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot6 <- efficiency_table %>%
  ggplot(aes(Nation, zone_eff, label = Nation)) +
  geom_point(size = 2, color = "blue") +
  ggtitle("Zone Efficiency") +
  ylab("Efficiency Rate") +
  geom_text_repel(max.overlaps = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#I'll now show the tables side by side
grid.arrange(plot5, plot6, ncol = 2, nrow = 1)

#With a few notable exceptions (Brazil, Ecuador, Guatemala, Pakistan, Uzbekistan, Mexico, Peru, 
  #South Africa, Estonia, and North Macedonia), the majority of countries have a top efficiency rate 
  #between approximately 0.3 and 0.65, and a zone efficiency rate between approx. 0.3 and 0.6.

#Not too much else is likely to be learned from comparing attempts to tops and zones. So, I will move
  #on to exploring whether a competitor's starting number affected the numbers of tops and zones
  #they earned.
plot7 <- bouldering %>%
  group_by(StartNr) %>%
  mutate(avg_tops = mean(Total_Tops)) %>%
  ggplot(aes(StartNr, avg_tops)) +
  geom_point(size = 2, color = "green") +
  ggtitle("Starting Number vs. Tops") +
  labs(x = "Starting Number", y ="Average Number of Tops")

#Let's now see if the same holds true for zones.
plot8 <- bouldering %>%
  group_by(StartNr) %>%
  mutate(avg_zones = mean(Total_Zones)) %>%
  ggplot(aes(StartNr, avg_zones)) +
  geom_point(size = 2, color = "red") +
  ggtitle("Starting Number vs. Zones") +
  labs(x = "Starting Number", y = "Average Number of Zones")

#Showing plots 7 and 8 side by side
grid.arrange(plot7, plot8, ncol = 2, nrow = 1)

#Though there is some variation in the distributions (and as a result, their respective plots), the 
  #overall shapes of the distributions are very similar. The first competitors seems to largely do poorer
  #in terms of both tops and zones than those who climbed later. That said, the vast majority of climbers
  #did not exceed 6 tops or 7 zones (I am rounding 7.5 down to 7 since you cannot get a half point for a
  #zone). Notably, those who went at the very end seem also do less well than those in the middle.

#Before moving on to building the first machine learning model for predicting tops, I will take a look at
  #the relationships between when a competition happened and tops/zones, as well as between particular climbers and tops.

#Individual Climbers vs. median Tops and Zones
indiv_meds <- bouldering %>%
  group_by(Name) %>%
  summarize(med_tops = median(Total_Tops),
            med_zones = median(Total_Zones)) %>%
  arrange(Name)

#Computing summary statistics about the above table.
summary(indiv_meds)

#Adding a numerical ordering to the climbers in the indiv_avgs table
indiv_meds <- indiv_meds %>%
  mutate(number = c(1:nrow(indiv_meds)))

#Plotting the two tables' data side by side (tops first, zones second).
plot9 <- indiv_meds %>%
  arrange(desc(med_tops)) %>%
  ggplot(aes(number, med_tops)) +
  geom_line(color = "green") +
  ggtitle("Distribution of Median Number of Tops by Individual Climbers") +
  labs(x = "Climber Number", y = "Median Number of Tops")

plot10 <- indiv_meds %>%
  arrange(desc(med_zones)) %>%
  ggplot(aes(number, med_zones)) +
  geom_line(color = "red") +
  ggtitle("Distribution of Median Number of Zones by Individual Climbers") +
  labs(x = "Climber Number", y = "Median Number of Zones")

grid.arrange(plot9, plot10, ncol = 2, nrow = 1)

#Finally, I will look at what effect the timing of a given competition had on the overall performance
  #of the climbers.
comp_spec_table <- bouldering %>%
  group_by(Competition) %>%
  summarize(avg_tops = mean(Total_Tops),
            avg_zones = mean(Total_Zones),
            med_tops = median(Total_Tops),
            med_zones = median(Total_Zones))

#Visually comparing average tops, median tops, average zones, and median zones by competition
plot11 <- comp_spec_table %>%
  ggplot(aes(group = Competition)) +
  geom_point(aes(Competition, avg_tops), color = "green") +
  geom_point(aes(Competition, med_tops), color = "red") +
  geom_point(aes(Competition, avg_zones), color = "blue") +
  geom_point(aes(Competition, med_zones), color = "black") +
  ylim(0, 14) +
  labs(x = "Competitions", y = "Number of Tops/Zones") +
  ggtitle("Average vs. Median Tops/Zones by Competition") +
  geom_label(aes(x = 18, y = 12, label = "Average Tops"), color = "green") +
  geom_label(aes(x = 18, y = 11, label = "Median Tops"), color = "red") +
  geom_label(aes(x = 18, y = 10, label = "Average Zones"), color = "blue") +
  geom_label(aes(x = 18, y = 9, label = "Median Zones"), color = "black") +
  theme(axis.text.x=element_blank())

plot11

#This clearly shows that the performance of the athletes is not even close to being uniform across
  #competitions. The first half of the tournaments all have notably higher median and mean numbers of
  #both tops and zones than any of the latter competitions. This might be explained by some of the 
  #competitions being on the junior level and others being on the senior level.
#NOTE: For some of the competitions, the median tops might look like it is missing. It is not; it's just
  #that the median was zero and is therefore obscured by the median zones point (because it's black).

#We are now prepared to start building models.


#BUILDING THE MODELS

#Step 1: Overview

  #I am going to make a single training set and a single testing set, with 80%  of the data 
  #going to the train set and 20% to the test set. Since there are approximately 4400 rows of data, this
  #will leave me with approximately 1100 rows of data to use as a test. 

  #The metric of success used here will be accuracy. Since tops are the most important factor to actually
  #winning a competition, being able to predict the number of tops a given climber will receive at a given
  #competition is essential to predicting winners at a competition. 

  #I will not try to predict zones based on tops, however, since, while zones are a requirement for tops,
  #the reverse is not true. In reality, being able to predict zones in this way would require knowing 
  #information which is not available at any time where a prediction about zones would be helpful. 

#Step 1.5: Modifying the bouldering table

  #Since the only factors I have considered are which competition a given row is in relation to, the 
  #nationality of the climber, starting number, tops, zones, and attempts, I will be removing the 
  #following columns: Name, Rank, Date (because date and competition are effectively the same). 

bouldering <- bouldering %>%
  select(Winner_Contender, StartNr, Total_Tops, Total_Zones, Total_Attempts_to_Top, 
         Total_Attempts_to_Zone, Qualification_Tops, Qualification_Zones, Qualification_Top_Attempts,
         Qualification_Zones_Attempts, Semifinal_Tops, Semifinal_Zones, Semifinal_Top_Attempts,
         Semifinal_Zones_Attempts, Final_Tops, Final_Zones, Final_Top_Attempts, Final_Zones_Attempts)

#NOTE: I have removed the Competition and Nation columns from bouldering. This is because there appeared
  #to be very little impact on the number of tops, zones, attempts, or efficiency in response to either 
  #potential factor. Also, the small number of competitions (22) against the much larger number of climber
  #data rows (5000+) would likely cause overly broad groupings to be made. A similar reasoning applies to
  #nations (where there are only 64).

#Before Making any actual models, I am going to change the Winner_Contender column into factors.
bouldering$Winner_Contender <- as.factor(bouldering$Winner_Contender)

#Taking a look at the new table and performing quality checks
dim(bouldering) #Should be 5498 x 20
any(is.na(bouldering))
head(bouldering)

#Step 2: Making the train and test sets
index <- createDataPartition(bouldering$Winner_Contender, times = 1, p = 0.8, list = FALSE)
boulder_train <- bouldering %>% slice(index)
boulder_test <- bouldering %>% slice(-index)

#Step 3: Quality checking the new sets (confirming dimensions and no NA values)
dim(boulder_train) #should be 4399 x 20
dim(boulder_test) #should be 1099 x 20
any(is.na(boulder_train))
any(is.na(boulder_test)) 

#NOTE: There are 19 potential predictors for Winner_Contender.

#Step 4: Making the models (This is a classification model)

#I will starting with KNN and random forest models. If appropriate (meaning no model scores particularly
  #well in regard to accuracy, specificity, or sensitivity), I will consider logistic regression and/or
  #an emsemble.

#RECALL THE GOAL: Predict whether a given climber has a 1 or 0 in the Winner_Contender column.

#Model 1: K-Nearest Neighbors

#Making the model
knn_fit <- train(Winner_Contender ~ ., method = "knn", data = boulder_train)

#Making the predictions
y_hat_knn <- predict(knn_fit, boulder_test, type = "raw")

#Making the confusion matrix.
knn_cf_mat <- confusionMatrix(y_hat_knn, boulder_test$Winner_Contender) 

#NOTE: cf_mat is short for confusion matrix.

#NOTE: I will be making a table throughout this process that will record the accuracy, sensitivity, and
#specificity of each model I make.
model_comparison <- data.frame(Model = "K Nearest Neighbors (KNN)",
                               Accuracy = knn_cf_mat$overall["Accuracy"],
                               Sensitivity = knn_cf_mat$byClass["Sensitivity"],
                               Specificity = knn_cf_mat$byClass["Specificity"])

#NOTE: Since the processes for making the models are highly similar to one another, I am not going to 
  #repeat the names of each step for each model. If there are any significant deviations in the process
  #from one model to another, I will note them.

#Model 2: Random Forests
rf_fit <- train(Winner_Contender ~ ., method = "rf", data = boulder_train)
y_hat_rf <- predict(rf_fit, boulder_test, type = "raw")
rf_cf_mat <- confusionMatrix(y_hat_rf, boulder_test$Winner_Contender)

#Adding model 2 to the table.
model_comparison <- bind_rows(model_comparison,
                              data.frame(Model = "Random Forest (rf)",
                                         Accuracy = rf_cf_mat$overall["Accuracy"],
                                         Sensitivity = rf_cf_mat$byClass["Sensitivity"],
                                         Specificity = rf_cf_mat$byClass["Specificity"]))

#It appears that the random forest model has made a perfect prediction and has maximum sensitivity and
  #specificity. This seems unusually (and perhaps unrealistically) high so I am going to see what logistic
  #regression yields.

#Model 3: Logistic Regression
glm_fit <- train(Winner_Contender ~ ., method = "glm", data = boulder_train)
y_hat_glm <- predict(glm_fit, boulder_test, type = "raw")
glm_cf_mat <- confusionMatrix(y_hat_glm, boulder_test$Winner_Contender)

#Adding it to the table
model_comparison <- bind_rows(model_comparison,
                              data.frame(Model = "Logistic Regression (glm)",
                                         Accuracy = glm_cf_mat$overall["Accuracy"],
                                         Sensitivity = glm_cf_mat$byClass["Sensitivity"],
                                         Specificity = glm_cf_mat$byClass["Specificity"]))

#This has again yielded what appears to be a perfect prediction. This, however, generated a lot of
  #warnings (50+) when the model and predictions were being made. So, I would favor random forests over
  #logistic regression.

#Let's look at the importance of the features used in each prediction.
varImp(knn_fit)
varImp(rf_fit)
varImp(glm_fit)

#Notably, for both KNN and random forests, Final_Zones_Attempts and Final_Zones were the most important
  #features. For logistic regression, however, Total_Zones and Qualification_Zones were the most important
  #features. 

#Taking a look at the final model_comparison table
model_comparison %>% knitr::kable(caption = "Model Comparison Table")

#Since the random forests model already achieved sufficiently high metrics of success, there is no need to
  #make an ensemble. Thus, while KNN yielded a very accurate model (approximately 98% accurate), random 
  #forests had a higher accuracy (and no significant errors, unlike logistic regression).

#FINAL PART

#NOTE: If you actually wanted to predict whether a given climber is likely to be a contender to win prior 
  #to the finals, you would need to do something akin to the following instead.
bouldering2 <- bouldering %>%
  select(Winner_Contender, StartNr, Qualification_Tops, Qualification_Zones, 
         Qualification_Top_Attempts, Qualification_Zones_Attempts, Semifinal_Tops, 
         Semifinal_Zones, Semifinal_Top_Attempts, Semifinal_Zones_Attempts) 

#Note that all features about total attempts, tops, or zones have been removed. All features
  #involving the finals have been removed as well.

index <- createDataPartition(bouldering2$Winner_Contender, times = 1, p = 0.8, list = FALSE)
boulder_train2 <- bouldering2 %>% slice(index)
boulder_test2 <- bouldering2 %>% slice(-index)

knn_fit2 <- train(Winner_Contender ~ ., method = "knn", data = boulder_train2)
y_hat_knn2 <- predict(knn_fit2, boulder_test2, type = "raw")
knn_cf_mat2 <- confusionMatrix(y_hat_knn2, boulder_test2$Winner_Contender) 

rf_fit2 <- train(Winner_Contender ~ ., method = "rf", data = boulder_train2)
y_hat_rf2 <- predict(rf_fit2, boulder_test2, type = "raw")
rf_cf_mat2 <- confusionMatrix(y_hat_rf2, boulder_test2$Winner_Contender)

glm_fit2 <- train(Winner_Contender ~ ., method = "glm", data = boulder_train2)
y_hat_glm2 <- predict(glm_fit2, boulder_test2, type = "raw")
glm_cf_mat2 <- confusionMatrix(y_hat_glm2, boulder_test2$Winner_Contender)

model_comparison2 <- data.frame(Model = "K Nearest Neighbors (KNN)",
                               Accuracy = knn_cf_mat2$overall["Accuracy"],
                               Sensitivity = knn_cf_mat2$byClass["Sensitivity"],
                               Specificity = knn_cf_mat2$byClass["Specificity"])
model_comparison2 <- bind_rows(model_comparison2,
                              data.frame(Model = "Random Forest (rf)",
                                         Accuracy = rf_cf_mat2$overall["Accuracy"],
                                         Sensitivity = rf_cf_mat2$byClass["Sensitivity"],
                                         Specificity = rf_cf_mat2$byClass["Specificity"]))
model_comparison2 <- bind_rows(model_comparison2,
                              data.frame(Model = "Logistic Regression (glm)",
                                         Accuracy = glm_cf_mat2$overall["Accuracy"],
                                         Sensitivity = glm_cf_mat2$byClass["Sensitivity"],
                                         Specificity = glm_cf_mat2$byClass["Specificity"]))


varImp(knn_fit2) #Semifinal_Tops and Semifinal_Zones are now the most important features.
varImp(rf_fit2) #Semifinal_Tops and Semifinal_Zones are now the most important features.
varImp(glm_fit2) #Semifinal_Tops and Semifinal_Zones are now the most important features.

model_comparison2 %>% knitr::kable(caption = "Model Comparison Table 2") 

#NOTE: The exact metrics listed below may be slightly different due to the partitioning of the bouldering
  #table being potentially different each time it is done but the values should be similar in all instances.
#Using this newly reduced group of features, KNN is actually the least accurate model (88.5%) and the
  #least specific (36.5%). 
#Logistic regression is more accurate (90.5%) but also has low specificity (52.6%).
#Random Forests is certainly the best option, with 95.8% accuracy, 98.5% sensitivity, and 79.5% specificity.

#So, I am only going to proceed with the random forest model. To possibly raise the accuracy, I will now
  #manually adjust the tuning parameters.
nodesize <- seq(1, 51, 5)
accuracies <- sapply(nodesize, function(q){
  train(Winner_Contender ~ ., method = "rf", data = boulder_train2,
        tuneGrid = data.frame(mtry = 9),
        nodesize = q)$results$Accuracy
})
qplot(nodesize, accuracies)

rf_fit3 <- randomForest(Winner_Contender ~ ., data = boulder_train2,
                        nodesize = nodesize[which.max(accuracies)])
y_hat_rf3 <- predict(rf_fit3, boulder_test2)
rf_cf_mat3 <- confusionMatrix(y_hat_rf3, boulder_test2$Winner_Contender)

rf_cf_mat3 #this shows 94.1% accuracy, 98% sensitivity, and 70.5% specificity.

varImp(rf_fit3)

#Since manually adjusting the parameters actually worsened the measures of success, the best (and final) 
  #model will be the non-manually adjusted Random Forest model.
final_model_comparison <- bind_rows(model_comparison2,
                                    data.frame(Model = "Random Forest [manually adjusted]",
                                               Accuracy = rf_cf_mat3$overall["Accuracy"],
                                               Sensitivity = rf_cf_mat3$byClass["Sensitivity"],
                                               Specificity = rf_cf_mat3$byClass["Specificity"]))

final_model_comparison %>% knitr::kable(caption = "Final Model Comparison",
                                        row_spec = 2, color = "black", background = "yellow")