
#Data cleaned using Excel

#Load in proper packages


#Making "Here" Package work
setwd("data")

#Load in the correct data
library(readxl)
nlsy_short <- read_excel(here::here("data", "cleaned_internet_midwest_data.xlsx"))


# Split by space and unnest

library(tidyverse)
nlsy <- nlsy_short %>%
	mutate(midwest_states = str_split(midwest_states, " ")) %>%
	unnest(midwest_states)

#Create a {gtsummary} table of descriptive statistics about your data (1 pt)
library(gtsummary)
tbl_summary(
	nlsy,
	by = midwest_identity,
	include = c(location, midwest_states,
							gender, age, income, education),
	type = list(midwest_states ~ "categorical"),
	label = list(
		midwest_states ~ "Midwest states picked",
		gender ~ "Gender",
		age ~ "Age Group",
		income ~ "Household Income",
		education ~ "Education Level",
		location ~ "Census Region"
	),
	missing_text = "Missing") %>%
	modify_spanning_header(all_stat_cols() ~ "**Identify as a Midwesterner**")


#Fit a regression and present well-formatted results from the regression (1 pt)

#Needed to recode Male and Female as 0 and 1 (Male=0, Female=1)
nlsy <- nlsy %>%
	mutate(gender_bin = ifelse(gender == "Female", 1, 0))


tbl_uvregression(
	nlsy,
	y = gender_bin,
	include = c(
		gender_bin, age, income, education
	),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE
)


#Create a figure (1 pt)






