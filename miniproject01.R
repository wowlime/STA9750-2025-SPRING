library(readr)
library(dplyr)
library(stringr)
library(data.table)
library(scales)
df <- read_csv("data/mp01/nyc_payroll_export.csv")
glimpse(df)
df <- df |>
  mutate(
    `agency_name` = str_to_title(`agency_name`),
    `last_name` = str_to_title(`last_name`),
    `first_name` = str_to_title(`first_name`),
    `work_location_borough` = str_to_title(`work_location_borough`),
    `title_description` = str_to_title(`title_description`),
    `leave_status` = str_to_title(`leave_status_as_of_june_30`)
  )
df_mayor <- df |> 
  filter(first_name == 'Eric', last_name == 'Adams', mid_init == 'L') |>
  select(fiscal_year, title_description, agency_name, base_salary) |>
  rename('Fiscal Year' = fiscal_year,
         'Position' = title_description,
         'Agency' = agency_name,
         'Total Salary' = base_salary) |>
  group_by(`Fiscal Year`) |>  
  summarize(
    `Total Salary` = sum(`Total Salary`),
    `Position` = paste(unique(`Position`), collapse = " / "),  
    `Agency` = paste(unique(`Agency`), collapse = " / ")  
  ) |>  
  arrange(`Fiscal Year`)  
df_mayor

tbl_txt <- "
Fiscal Year, Total Salary, Position, Agency
2014, 160000, Borough President, Borough President-Brooklyn
2015, 160000, Borough President, Borough President-Brooklyn
2017, 179200, Borough President, Borough President-Brooklyn
2018, 179200, Borough President, Borough President-Brooklyn
2019, 179200, Borough President, Borough President-Brooklyn
2020, 179200, Borough President, Borough President-Brooklyn
2021, 179200, Borough President, Borough President-Brooklyn
2022, 437950, Borough President / Mayor, Borough President-Brooklyn / Office Of The Mayor
2023, 258750, Mayor, Office Of The Mayor
2024, 258750, Mayor, Office Of The Mayor"

read_csv(tbl_txt) |> 
  mutate(`Total Salary` = dollar(`Total Salary`)) |>
  data.table(options=list(searching=FALSE, 
                         paging=FALSE,
                         info=FALSE))
df |> 
  mutate(Total_Compensation = case_when(
    pay_basis == "per Annum" ~ base_salary,  
    pay_basis == "per Hour" ~ (regular_hours * (base_salary / 2080)) +  
      (ot_hours * (base_salary / 2000) * 1.5) + total_other_pay,
    pay_basis == "per Day" ~ base_salary * (regular_hours / 7.5),  
    TRUE ~ NA_real_  
  ))
#Q.1 Which job title has the highest base rate of pay?
df |> 
  mutate(base_rate = case_when(
    pay_basis == "per Annum" ~ base_salary / 2000, 
    pay_basis == "per Hour" ~ base_salary, 
    TRUE ~ NA_real_
  )) |> 
  arrange(desc(base_rate)) |> 
  select(title_description, base_rate) |> 
  slice(1)
#Q.2 Which individual & in what year had the single highest city total payroll (regular + overtime)?
df |> 
  mutate(total_pay = regular_gross_paid + total_ot_paid) |> 
  arrange(desc(total_pay)) |> 
  select(fiscal_year, first_name, last_name, total_pay) |> 
  slice(1)
#Q.3 Which individual worked the most overtime hours in this dataset?
df |> 
  arrange(desc(ot_hours)) |> 
  select(first_name, last_name, ot_hours) |> 
  slice(1)
#Q.4 Which agency has the highest average total annual payroll (base + overtime pay per employee)?
df |> 
  group_by(agency_name) |> 
  summarize(avg_total_pay = mean(base_salary + total_ot_paid, na.rm = TRUE)) |> 
  arrange(desc(avg_total_pay)) |> 
  slice(1)
#Q.5 Which agency has the most employees on payroll in each year?
df |> 
group_by(fiscal_year, agency_name) |> 
  summarize(employee_count = n()) |> 
  arrange(fiscal_year, desc(employee_count)) |> 
  slice(1)
#Q.6 Which agency has the highest overtime usage (compared to regular hours)?
df |> 
group_by(agency_name) |> 
  summarize(overtime_ratio = sum(ot_hours, na.rm = TRUE) / sum(regular_hours, na.rm = TRUE)) |> 
  arrange(desc(overtime_ratio)) |> 
  slice(1)
#Q.7 What is the average salary of employees who work outside the five boroughs?
df |> 
  filter(!work_location_borough %in% c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")) |> 
  summarize(avg_salary = mean(base_salary, na.rm = TRUE))
#Q. 8 How much has the city’s aggregate payroll grown over the past 10 years?
payroll_growth <- df |> 
  group_by(fiscal_year) |> 
  summarize(total_payroll = sum(base_salary + total_ot_paid, na.rm = TRUE)) |> 
  arrange(fiscal_year) |> 
  mutate(percentage_increase = (total_payroll - lag(total_payroll)) / lag(total_payroll) * 100)
#Policy analysis
#Policy 1 Capping Salaries at Mayoral Level
mayor_salaries <- df |> 
  filter(title_description == "Mayor") |> 
  group_by(fiscal_year) |> 
  summarize(base_salary_mayor = max(base_salary, na.rm = TRUE))
mayor_salaries
df_updated <- df |> 
  left_join(mayor_salaries, by = "fiscal_year") |> 
  mutate(above_mayor_salary = base_salary > base_salary_mayor)
employees_above_mayor <- df_updated |> 
  filter(above_mayor_salary) |> 
  select(fiscal_year, agency_name, title_description, base_salary, base_salary_mayor)
print(employees_above_mayor, n = 50)
employees_above_mayor <- employees_above_mayor |>  
  mutate(savings = base_salary - base_salary_mayor)  
total_savings <- sum(employees_above_mayor$savings, na.rm = TRUE)
total_savings
impacted_agencies <- employees_above_mayor |>  
  count(agency_name, sort = TRUE) 
impacted_titles <- employees_above_mayor |>  
  count(title_description, sort = TRUE)  
impacted_agencies
impacted_titles
total_people_above_mayor <- nrow(employees_above_mayor)
total_people_above_mayor
#Policy 2
overtime_analysis <- df |> 
  group_by(agency_name, title_description) |> 
  summarize(
    total_overtime_hours = sum(ot_hours, na.rm = TRUE),
    total_overtime_pay = sum(total_ot_paid, na.rm = TRUE),
    average_hourly_rate = mean(base_salary / 2080, na.rm = TRUE)
  ) |> 
  mutate(
    required_fte = ceiling(total_overtime_hours / 2080)
  )
overtime_analysis <- overtime_analysis |> 
  mutate(
    overtime_cost = total_overtime_hours * average_hourly_rate * 1.5, 
    regular_cost = total_overtime_hours * average_hourly_rate,         
    potential_savings = overtime_cost - regular_cost                 
  )
overtime_analysis
overtime_fte_analysis <- df |> 
  group_by(agency_name, title_description) |> 
  summarize(
    total_overtime_hours = sum(ot_hours, na.rm = TRUE)
  ) |> 
  mutate(
    required_fte = ceiling(total_overtime_hours / 2080)  
  ) |> 
  arrange(desc(total_overtime_hours))
overtime_fte_analysis
agency_savings <- overtime_analysis |> 
  group_by(agency_name) |> 
  summarize(
    total_potential_savings = sum(potential_savings, na.rm = TRUE),
    total_required_fte = sum(required_fte, na.rm = TRUE)
  ) |> 
  arrange(desc(total_potential_savings))
agency_savings
#policy 3
nyc_boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx")
nyc_data <- df |> 
  filter(work_location_borough %in% nyc_boroughs)
borough_salary_nyc <- nyc_data |> 
  group_by(work_location_borough) |> 
  summarize(avg_base_salary = mean(base_salary, na.rm = TRUE), .groups = 'drop')
print(paste("Average Base Salary for NYC Boroughs:", borough_salary_nyc))
lowest_cost_borough <- borough_salary_nyc |> 
  filter(avg_base_salary == min(avg_base_salary)) |> 
  pull(work_location_borough)
print(paste("Lowest-Cost Borough:", lowest_cost_borough))
adjusted_salaries <- nyc_data |> 
  left_join(borough_salary_nyc, by = "work_location_borough") |> 
  mutate(
    adjusted_salary = ifelse(
      work_location_borough != lowest_cost_borough,
      base_salary * 0.9,  
      base_salary
    ),
    salary_reduction = pmax(0, base_salary - adjusted_salary)
  )
total_savings <- sum(adjusted_salaries$salary_reduction, na.rm = TRUE)
print(paste("Total Potential Savings by Adjusting Salaries:", dollar(total_savings)))
savings_by_borough <- adjusted_salaries |> 
  group_by(work_location_borough) |> 
  summarize(total_savings = sum(salary_reduction, na.rm = TRUE)) |> 
  arrange(desc(total_savings))
print("Potential Savings by Borough:")
print(savings_by_borough)

library(ggplot2)
library(scales)
ggplot(payroll_growth, aes(x = fiscal_year, y = percentage_increase)) +
  geom_line(color = "steelblue", linewidth = 1) +  
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Year-over-Year Percentage Increase in Aggregate Payroll",
       x = "Fiscal Year",
       y = "Percentage Increase (%)") +
  theme_minimal()