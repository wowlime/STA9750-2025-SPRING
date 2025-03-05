if(!file.exists("births.csv")){
  download.file("https://raw.githubusercontent.com/michaelweylandt/STA9750/main/births.csv", 
                destfile="births.csv")
}
library(readr)
library(dplyr)

births <- read_csv("births.csv")
glimpse(births)
births |> filter(day==1, month==1, year==1984)
births |> filter(year==1984) |> summarize(sum(births))
births |> group_by(year) |> summarize(n_births = sum(births))
births |> 
  group_by(year) |>
  summarize(n_births = sum(births)) |>
  mutate(increase_births = n_births - lag(n_births))
births |> 
  group_by(month) |>
  summarize(avg_births = mean(births)) |>
  slice_max(avg_births)
