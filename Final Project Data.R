library(tidyverse)
library(fuzzyjoin)
ai <- read_csv("ai_threat_index.csv")
bls <- read_csv("bls_projections.csv")
bls_clean <- bls |> 
  select(-`Occupation Code`, -`Median Annual Wage 2023`,`Education Code`, `Workex Code`)
ai_clean <- ai |> 
  rename(job_title = `Job titles`) |> 
  mutate(job_title = str_to_lower(str_trim(job_title)))

bls_clean <- bls_clean |> 
  rename(job_title = `Occupation Title`) |> 
  mutate(job_title = str_to_lower(str_trim(job_title)))
combined_data <- stringdist_inner_join(
  ai_clean, bls_clean,
  by = "job_title",
  max_dist = 0.4,         
  method = "jw"       
)
combined_data <- combined_data |> 
  select(-job_title.y)
combined_data <- combined_data |> 
  rename(job_title = job_title.x)
glimpse(combined_data)
view(combined_data)