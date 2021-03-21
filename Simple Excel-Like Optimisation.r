library(tidyverse, warn.conflicts = FALSE)

sensitivity <- tibble(
worker_salary = 40000,
misc_per_pig = 1000,
pigs = seq(from = 1, to = 100, by = 1),
total_indirect_costs = worker_salary + (misc_per_pig * pigs),
direct_costs_per_pig = 4859.166667,
total_direct_costs = direct_costs_per_pig * pigs,
total_costs = total_direct_costs + total_indirect_costs,
revenue_per_pig = 9000,
total_revenue = revenue_per_pig * pigs,
profit = total_revenue - total_costs
)

head(sensitivity)

# Number of pigs required to break even in the 4th month
sensitivity %>% 
filter(profit==min(abs(profit)))

crossing(
    months = c(5,6,7),
    pigs = seq(from = 1, to = 200, by = 1)) %>% 
head()

# For each month, try the same number of pigs

sensitivity_exp <- tibble(
#Get all possible combination of month*pigs values
crossing(months = c(4,5,6),
         pigs = seq(from = 1, to = 200, by = 1)),
worker_salary = 10000,
total_salaries = worker_salary * months,
misc_per_pig = 1000,
total_indirect_costs = total_salaries + (misc_per_pig * pigs),
direct_costs_per_pig = case_when(months == 4 ~ 4859.166667,
                                 months == 5 ~ 7064.166667,
                                 TRUE ~ 9269.166667),
total_direct_costs = direct_costs_per_pig * pigs,
total_costs = total_direct_costs + total_indirect_costs,
revenue_per_pig = case_when(months == 4 ~ 9000,
                            months == 5 ~ 13910,
                            TRUE ~ 17640),
total_revenue = revenue_per_pig * pigs,
profit = total_revenue - total_costs
)

sensitivity_exp %>% 
filter(pigs==12)

#Number of pigs to break even in each month
sensitivity_exp %>% 
group_by(months) %>% 
arrange(abs(profit), `.by_group` = TRUE) %>% 
slice(1)


