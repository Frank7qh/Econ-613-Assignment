library(tidyverse)
library(xtable)

# Exercise 1
# 1.1-1.5
read_csv("./data/dathh2007.csv") %>% summarize(count = n())
read_csv("./data/dathh2005.csv") %>% filter(mstatus == "Couple, with Kids") %>%
  summarize(count = n())
read_csv("./data/datind2008.csv") %>% summarize(count = n())
read_csv("./data/datind2016.csv") %>% filter(age >= 25, age <= 35) %>%
  summarize(count = n())
datind2009 <- read_csv("./data/datind2009.csv")
cross_table <- table(datind2009$gender, datind2009$profession)
cross_table
print(xtable(cross_table, type = "latex"),file = "cross_table.tex")
rm(list = ls(all.names = TRUE))

# 1.6
abs_diff <- function(x,y) { # used for computing gini coefficient below
  return(abs(x-y))
}

mysummary <- function(var){ # used for distribution of wage
  ls <- list()
  ls$mean <- mean(var)
  ls$sd <- sd(var)
  ls$inter_decile_ratio <- as.numeric(quantile(var, 0.9) / quantile(var, 0.1))
  # compute gini coefficient
  nom_gini <- sum(outer(var, var, abs_diff))
  denom_gini <- 2 * (length(var)^2) * mean(var)
  ls$gini <- nom_gini / denom_gini
  return(ls)
}

datind2005_emp <- read_csv("./data/datind2005.csv") %>%
  filter(empstat == "Employed")
mysummary(na.omit(datind2005_emp$wage))
datind2019_emp <- read_csv("./data/datind2019.csv") %>%
  filter(empstat == "Employed")
mysummary(na.omit(datind2019_emp$wage))
rm(list = ls(all.names = TRUE))

# 1.7
read_csv("./data/datind2010.csv") %>% ggplot() + 
  geom_histogram(mapping = aes(age), binwidth = 1)

read_csv("./data/datind2010.csv") %>% ggplot() + 
  geom_histogram(mapping = aes(x = age), binwidth = 3) +
  facet_wrap(~ gender, ncol = 2)

# 1.8
dathh2011 <- read_csv("./data/dathh2011.csv", 
                      col_types = cols(.default = "c"), col_select = !c(1))
datind2011 <- read_csv("./data/datind2011.csv", 
                       col_types = cols(.default = "c"), col_select = !c(1))
common_names <- intersect(colnames(dathh2011), colnames(datind2011))
full_join(dathh2011, datind2011, by = common_names) %>%
  filter(location == "Paris") %>% summarise(count = n())
rm(list = ls(all.names = TRUE))



# Exercise 2
# import and append data
dathh <- list.files("./data", pattern = "dathh*") %>%
                      map(~ paste("./data/", .x, sep = "")) %>%
  map(read_csv, col_types = cols(.default = "c"), col_select = !c(1)) %>%
  bind_rows

datind <- list.files("./data", pattern = "datind*") %>%
                      map(~ paste("./data/", .x, sep = "")) %>%
  map(read_csv, col_types = cols(.default = "c"), col_select = !c(1)) %>%
  bind_rows

# common names
common_names <- intersect(colnames(dathh), colnames(datind))
common_names

# merge
data <- full_join(dathh, datind, by = common_names)

# duplicates
nrow(data) - nrow(distinct(data, idind, year)) # only 32 duplicates
data <- distinct(data, idind, year, .keep_all = TRUE)

# transform string into factors
to_factor_var <- c("mstatus","move", "location","empstat", "profession", "gender")
data <- data %>% mutate(across(all_of(to_factor_var), ~ factor(.)))
rm(to_factor_var)

# tranform string into numeric
to_numeric_var <- c("year", "datent", "myear", "respondent", "age", "wage")
data <- data %>% mutate(across(all_of(to_numeric_var), ~ as.integer(.))) 
# it turns out that all are integer
rm(to_numeric_var)

# 2.5
# Number of households in which there are more than four family members each year
result <- data %>% group_by(year, idmen) %>% summarise(hh_member = n()) %>% 
  filter(hh_member > 4) %>% summarise(count = n())
print(xtable(t(result), type = "latex"), file = "./output/2.5.tex")
  
# Highly unbalanced panel! No way to track households across years
# The following plots the number of years each household appears in the data
data %>% group_by(idmen, year) %>% summarise(hh_member = n()) %>% 
  summarise(n_year = n()) %>% ggplot() + geom_bar(mapping = aes(n_year))

# 2.6
# Number of households in which at least one member is unemployed each year
result <- data %>% group_by(year, idmen) %>% 
  summarise(has_unemp = any(empstat == "Unemployed")) %>% 
  filter(has_unemp) %>% summarise(num_with_unemp = n())
print(xtable(t(result), type = "latex"), file = "./output/2.6.tex")

# 2.7
# Number of households in which at least two members are of the same profession each year
result <- data %>% group_by(year, idmen) %>% 
  summarise(same_prof = any(duplicated(profession))) %>% 
  filter(same_prof) %>% summarise(num_same_prof = n())
print(xtable(t(result), type = "latex"), file = "./output/2.7.tex")

# 2.8
# Number of individuals in the panel that are from household-Couple with kids each year
result <- data %>% group_by(year) %>% filter(mstatus == "Couple, with Kids") %>%
  summarise(count = n())
print(xtable(t(result), type = "latex"), file = "./output/2.8.tex")

# 2.9
# Number of individuals in the panel that are from Paris each year
result <- data %>% group_by(year) %>% filter(location == "Paris") %>%
  summarise(count = n())
print(xtable(t(result), type = "latex"), file = "./output/2.9.tex")

# 2.10
# Idmen with most household members each year
idmens <- data %>% group_by(year, idmen) %>% summarise(hh_member = n()) %>%
  filter(min_rank(desc(hh_member)) == 1)
print(xtable(select(idmens, year, idmen), type = "latex"), file = "./output/2.10.tex")

# Idmen with most household members ever
data %>% group_by(idmen, year) %>% summarise(hh_member = n()) %>%
  summarise(max_hh_member = max(hh_member)) %>%
  filter(min_rank(desc(max_hh_member)) == 1)

# 2.11
# Number of households present in 2010 and 2011
data %>% group_by(idmen, year) %>% filter(year == 2010 | year == 2011) %>%
  summarise() %>% summarise(n_year = n()) %>% 
  filter(n_year == 2) %>% summarise(count = n())

rm(idmens, result)
# Exercise 3

# 3.1
# two definitions of time spent in the survey:
# time_spent: exit_year - entry_year + 1
# n_year: number of years actually surveyed 
# (remove years between entry_year and exit_year, but not surveyed)

data <- data %>% group_by(idmen) %>% 
  mutate(entry_year = min(year), exit_year = max(year)) 
# entry_year, exit_year recorded for further use

data %>% group_by(idmen) %>% 
  summarise(time_spent = mean(exit_year - entry_year + 1)) %>%
  ggplot() + geom_bar(mapping = aes(time_spent))

data %>% group_by(idmen, year) %>% summarise() %>%
  summarise(n_year = n()) %>% ggplot() + geom_bar(mapping = aes(n_year))

# 3.2
# just moved into current dwelling: year == datent
data <- data %>% mutate(just_entered = (year == datent))
head <- data %>% group_by(idmen, year) %>% 
  summarise(just_entered = first(just_entered)) %>% head(10)
print(xtable(head, type = "latex"), file = "./output/3.2.tex")
rm(head)

data %>% group_by(year) %>% 
  summarise(share_just_entered = mean(as.integer(just_entered),
                                      na.rm = TRUE)) %>%
  ggplot + geom_line(mapping = aes(year, share_just_entered))

# 3.3
# move == TRUE cannot determine migration at the surveyed year;
# it must be coupled with the condition that the individual was surveyed last year
data <- data %>% group_by(idind) %>% mutate(recent_move = 
  case_when(move == 1 ~ FALSE, move == 2 & (year-1 == lag(year)) ~ TRUE,
            TRUE ~ NA)) %>% mutate(just_migrated = if_else(year <= 2014, 
          myear == year, recent_move))

head <- data %>% group_by(idmen, year) %>% 
  summarise(just_migrated = first(just_migrated)) %>% head(10)
print(xtable(head, type = "latex"), file = "./output/3.3.tex")

data %>% group_by(year) %>% 
  summarise(share_just_migrated = mean(just_migrated, na.rm = TRUE)) %>%
  ggplot + geom_line(mapping = aes(year, share_just_migrated))

# 3.4
data %>% group_by(year) %>% 
  summarise(share_just_moved = mean(just_entered, na.rm = TRUE),
            share_just_migrated = mean(just_migrated, na.rm = TRUE)) %>%
  pivot_longer(c(share_just_moved, share_just_migrated), 
               names_to = "measurement_type", values_to = "move_share") %>%
  ggplot + geom_line(mapping = aes(year, move_share, color = measurement_type))

# 3.5
# Here I use datent to identify if a household has migrated
# This computes the proportion of migrating households with such situations,
# and one household can be counted twice if it migrated twice
data <- data %>% group_by(idind) %>% mutate(last_profession = lag(profession),
  last_empstat = lag(empstat))
data %>% filter(just_entered) %>% group_by(idmen, year) %>%
  summarise(change_prof_emp = any((profession != 
  last_profession) | (empstat != last_empstat))) %>%
  ungroup %>% summarise(proportion_change = mean(change_prof_emp, 
                                                 na.rm = TRUE))

# If one insists on the number of households who had such situations
# at least once:
data %>% filter(just_entered) %>% group_by(idmen) %>% 
  summarise(change_prof_emp = any((profession != 
  last_profession) | (empstat != last_empstat))) %>%
  filter(change_prof_emp == TRUE) %>% summarise(count = n())
 
# Exercise 4
# Entry & exit is inaccurate, as in some years between the entry / exit years
# the individual was not surveyed.

# Here I just record the years surveyed for each individual in a list.
attrition <- data %>% group_by(idind) %>% mutate(ls_year = list(unique(year))) %>%
  ungroup %>% mutate(attrition = !map2_lgl(year+1, ls_year, is.element)) %>%
  filter(year < max(year)) %>% group_by(year) %>% 
  summarise(attrition_percent = mean(attrition) * 100)

print(xtable(t(attrition), type = "latex"), file = "./output/attrition.tex")
rm(attrition)

