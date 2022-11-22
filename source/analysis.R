library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(usmap)

# The functions might be useful for A4
source("https://raw.githubusercontent.com/info201b-au2022/a4-SirBlank/main/source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
incarceration_data <- get_data(num_records = -1)

# This function gets total jail population based on gender
get_jail_gender_growth <- function() {
  df <- incarceration_data %>%
    group_by(state, county_name, year) %>%
    summarize(change_per_year_female = female_jail_pop - lag(female_jail_pop, na.rm = TRUE, default = 0),
              change_per_year_male = male_jail_pop - lag(male_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year) %>%
    summarize(female = sum(change_per_year_female, na.rm = TRUE),
              male = sum(change_per_year_male, na.rm = TRUE))
  df <- data.frame(df[1], stack(df[2:3]))
  colnames(df)[3] <- "Gender"
  return(df)
}

# This function gets the percent change from 2008 and 2018 in jail population based on gender
get_percent_change_gender_jail_pop <- function() {
  df <- incarceration_data %>%
    group_by(state, county_name, year) %>%
    summarize(change_per_year_female = female_jail_pop - lag(female_jail_pop, na.rm = TRUE, default = 0),
              change_per_year_male = male_jail_pop - lag(male_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year, state) %>%
    summarize(female = sum(change_per_year_female, na.rm = TRUE),
              male = sum(change_per_year_male, na.rm = TRUE))
  
  df <- df[(df$year == 2018 | df$year == 2008), ]
  
  df <- df %>%
    group_by(state) %>%
    mutate(percent_change_female = (female / lag(female) - 1) * 100,
           percent_change_male = (male / lag(male) - 1) * 100)
  
  df <- df[!(df$year == 2008), ]
  
  return(df)
}

# Values for summary
female_jail_pop_2008 <- get_jail_gender_growth()[39, 2]
female_jail_pop_2008
female_jail_pop_2018 <- get_jail_gender_growth()[49, 2]
female_jail_pop_2018

male_jail_pop_2008 <- get_jail_gender_growth()[88, 2]
male_jail_pop_2008
male_jail_pop_2018 <- get_jail_gender_growth()[98, 2]
male_jail_pop_2018

female_percent_change <- (female_jail_pop_2018 - female_jail_pop_2008) / female_jail_pop_2008 * 100
female_percent_change <- round(female_percent_change, 3)
female_percent_change

male_percent_change <- (male_jail_pop_2018 - male_jail_pop_2008) / male_jail_pop_2008 * 100
male_percent_change <- round(male_percent_change, 3)
male_percent_change

male_percent_change_pos <- abs(male_percent_change)

state_highest_female_change_row <- get_percent_change_gender_jail_pop()[which.max(get_percent_change_gender_jail_pop()$percent_change_female), ]
state_highest_female_change_row 

state_highest_female_change <- state_highest_female_change_row %>%
  pull(state)
state_highest_female_change

state_highest_female_change_num <- state_highest_female_change_row %>%
  pull(percent_change_female)
state_highest_female_change_num <- round(state_highest_female_change_num, 3)
state_highest_female_change_num

state_highest_female_change_num_male <- state_highest_female_change_row %>%
  pull(percent_change_male)
state_highest_female_change_num_male <- round(state_highest_female_change_num_male, 3)
state_highest_female_change_num_male

summary_info <- list(female_percent_change,
                     male_percent_change,
                     state_highest_female_change,
                     state_highest_female_change_num,
                     state_highest_female_change_num_male
)
summary_info



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function gets the total jail population from 1970 to 2018
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  df <- incarceration_data %>%
  group_by(year, state, county_name) %>%
  summarize(change_per_year = total_jail_pop - lag(total_jail_pop, na.rm = TRUE, default = 0)) %>%
  group_by(year) %>%
  summarize(sum = sum(change_per_year, na.rm = TRUE))
  return(df)   
}
#View(get_year_jail_pop())

# This function plots the total jail population from 1970 to 2018
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  p <- ggplot(data = get_year_jail_pop(), aes(x = year, y = sum)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = label_comma()) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") + 
    xlab("Year") +
    ylab("Total Jail Population") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018). This chart shows the growth of the U.S. jail population from \n 1970 to 2018.")
  
  return(p)   
} 

plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function gets the total jail population of the selected states
get_prison_pop_by_state <- function(states) {
  selected_states <- states
  df <- incarceration_data %>%
    group_by(year, state, county_name) %>%
    summarize(change_per_year = total_jail_pop - lag(total_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year, state) %>%
    summarize(sum = sum(change_per_year, na.rm = TRUE))
  df <- subset(df, state %in% selected_states)
  colnames(df)[2] <- "State"
  return(df) 
}


prison_pop_ca_tx_ny_il <- get_prison_pop_by_state(c("CA", "TX", "NY", "IL"))

# This function plots the total jail population of the selected states.
plot_prison_pop_by_state <- function() {
  p <- ggplot(data = prison_pop_ca_tx_ny_il, aes(x = year, y = sum, group = State, color = State)) +
    geom_line() +
    scale_y_continuous(labels = label_comma()) +
    ggtitle("Increase of Jail Population in California, Texas, New York, and Illinois") + 
    labs(subtitle = "(1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "Figure 2. Increase of Jail Population in California, Texas, New York, and Illinois (1970-2018). This chart shows the growth of the U.S. states California, Texas, New York, and Illinois's jail population from 1970 to 2018.") +
    labs(fill = "U.S. State") 
    
  return(p)
}
plot_prison_pop_by_state()

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# women vs men jail population
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function gets total jail population based on gender
get_jail_gender_growth <- function() {
  df <- incarceration_data %>%
    group_by(state, county_name, year) %>%
    summarize(change_per_year_female = female_jail_pop - lag(female_jail_pop, na.rm = TRUE, default = 0),
              change_per_year_male = male_jail_pop - lag(male_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year) %>%
    summarize(female = sum(change_per_year_female, na.rm = TRUE),
              male = sum(change_per_year_male, na.rm = TRUE))
  df <- data.frame(df[1], stack(df[2:3]))
  colnames(df)[3] <- "Gender"
  return(df)
}

get_jail_gender_growth()
#View(get_jail_gender_growth())

# This function plots the total jail population based on gender
plot_jail_gender_growth <- function() {
  p <- ggplot(data = get_jail_gender_growth(), aes(x = year, y = values, group = Gender, color = Gender)) +
    geom_line() +
    scale_y_continuous(labels = label_comma()) +
    ggtitle("Increase of Male v.s. Female Jail Population in U.S. (1970-2018)") + 
    xlab("Year") +
    ylab("Total Jail Population") +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(caption = "Figure 3. Increase of Male v.s. Female Jail Population in U.S. (1970-2018). This chart shows the growth of male and \n female jail population from 1970 to 2018.") +
    labs(fill = "U.S. State") 
  return(p)
}

plot_jail_gender_growth()


 ## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# A chloropleth map that turns purple if a state has higher rates of male jail population and turns yellow if a state has higher female jail population.
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function gets the percent change from 2008 and 2018 in jail population based on gender
get_percent_change_gender_jail_pop <- function() {
  df <- incarceration_data %>%
    group_by(state, county_name, year) %>%
    summarize(change_per_year_female = female_jail_pop - lag(female_jail_pop, na.rm = TRUE, default = 0),
              change_per_year_male = male_jail_pop - lag(male_jail_pop, na.rm = TRUE, default = 0)) %>%
    group_by(year, state) %>%
    summarize(female = sum(change_per_year_female, na.rm = TRUE),
              male = sum(change_per_year_male, na.rm = TRUE))
  
  df <- df[(df$year == 2018 | df$year == 2008), ]
  
  df <- df %>%
    group_by(state) %>%
    mutate(percent_change_female = (female / lag(female) - 1) * 100,
           percent_change_male = (male / lag(male) - 1) * 100)
  
  df <- df[!(df$year == 2008), ]
  
  return(df)
}

#View(get_percent_change_gender_jail_pop())

# This function plots the percent change for female population 
plot_female_percent_change <- function() {
  p <- plot_usmap(data = get_percent_change_gender_jail_pop(), values = "percent_change_female", color = "white") + 
    scale_fill_continuous(
      low = "green", high = "red", name = "Average Percentage Change in Female Jail Population per Year (%)", label = scales::comma, limits = c(-50, 70)
    ) + 
    theme(legend.position = "right") +
    theme(plot.caption = element_text(hjust = 0)) +
    ggtitle("Average Percentage Change per Year in Female Jail Population in U.S. (2008-2018)") +
    labs(caption = "Figure 4. Average Percentage Change in Female Population per Year in Male Jail Population in U.S. (2008-2018). This map shows a visual encoding of average percentage change in female population per year in each state.")
  return(p)
}
plot_female_percent_change()

# This function plots the percent change for male population
plot_male_percent_change <- function() {
  p <- plot_usmap(data = get_percent_change_gender_jail_pop(), values = "percent_change_male", color = "white") + 
    scale_fill_continuous(
      low = "green", high = "red", name = "Average Percentage Change in Male Jail Population per Year (%)", label = scales::comma, limits = c(-50, 70)
    ) + 
    theme(legend.position = "right") +
    theme(plot.caption = element_text(hjust = 0)) +
    ggtitle("Average Percentage Change per Year in Male Jail Population in U.S. (2008-2018)") +
    labs(caption = "Figure 5. Average Percentage Change in Male Population per Year in Male Jail Population in U.S. (2008-2018). This map shows a visual encoding of average percentage change in male population per year in each state.")
  return(p)
}
plot_male_percent_change()



## Load data frame ---- 


