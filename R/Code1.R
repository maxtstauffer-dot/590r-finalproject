
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




