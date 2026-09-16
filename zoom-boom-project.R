###############################
##Name: Hue Khoi Ly
##Project: The Zoom Boom: How Remote Work Rewired Where Americans Live
##Part 1: Remote Work
###############################
library(tidyverse)
library(jsonlite)
library(tidycensus)
library(ggplot2)

#pull data from the JSON link
x <- fromJSON("https://wfhresearch.com/datafiles/WFHtimeseries_monthly.json", simplifyVector = FALSE)

#choose main series in the list
main_series <- x$series[[which(
  sapply(x$series, \(s) s$source_id) == "wfhcovid_matquestion"
)]]

#change the list to data frame
remote_tbl <- as.data.frame(do.call(rbind, main_series$observations))
names(remote_tbl) <- c("date", "value")

#change the type of the column value
rm_USA <- remote_tbl |>
  mutate(
    date = as.Date(unlist(date)),
    value = as.numeric(unlist(value))
  )


#create plot for question 1
baseline <- rm_USA |>
  filter(date == as.Date("2019-12-01")) |>
  pull(value)

rm_USA |> 
  ggplot(aes(date,value))+
  geom_line()+
  geom_hline(yintercept = baseline, linetype = "dashed", color = "red") +
  labs(
    title = "SWAA work from home rate",
    x = "Date",
    y = "Percent of full paid working days"
  )

## Pull the data from census
##census_api_key("API key",install = "TRUE") before get_acs()
rm.pc_24<- get_acs(
  geography = "state",
  variables = "DP03_0024PE",
  survey = "acs1",
  year = 2024,
  geometry = TRUE
)

target_states <- c("California", "Nevada", "Arizona", "Texas", "Idaho","Colorado", "Utah", "Florida")
rm.pc_24.tg <- rm.pc_24 |> 
  filter(NAME %in% target_states)

## chart for question 2:
rm.pc_24.tg |> ggplot(aes(x=reorder(NAME,estimate),y= estimate))+
  geom_col(fill = '#00BF63')+
  coord_flip() +
  labs(
    title = "Share of workers working from home",
    x = "",
    y = "Percent"
  )

##question 3:
##pull data
years <- c(2019, 2021, 2022, 2023, 2024)
remote_by_year <- lapply(years, function(y) {
  get_acs(
    geography = "state",
    variables = "DP03_0024PE",
    year = y,
    survey = "acs1"
  ) |>
    mutate(year = y)
}) |>
  bind_rows()

## wider the table and filter the table
remote_growth <- remote_by_year |>
  filter(NAME %in% target_states) |>
  select(NAME, year, estimate) |>
  pivot_wider(names_from = year, values_from = estimate) |>
  mutate(change_2019_2024 = `2024` - `2019`)

## create graph for q3:
ggplot(remote_growth, aes(x = reorder(NAME, change_2019_2024), y = change_2019_2024)) +
  geom_segment(aes(xend = NAME, y = 0, yend = change_2019_2024), color = "gray60") +
  geom_point(size = 3, color = "steelblue") +
  coord_flip() +
  labs(
    title = "Growth in remote-work share, 2019 to 2024",
    x = "",
    y = "Percentage-point change"
  ) +
  theme_minimal()

## question 4:
ca_remote <- lapply(years, function(y) {
  get_acs(
    geography = "state",
    variables = "DP03_0024PE",
    year = y,
    survey = "acs1"
  ) |>
    filter(NAME == "California") |>
    mutate(year = y)
}) |>
  bind_rows()

ca_compare <- ca_remote |>
  transmute(
    year,
    remote = estimate,
    onsite = 100 - estimate
  )
## longer the data table
ca_long <- ca_compare |>
  pivot_longer(
    cols = c(remote, onsite),
    names_to = "work_type",
    values_to = "percent"
  )
## draw the graph
ggplot(ca_long, aes(x = factor(year), y = percent, fill = work_type)) +
  geom_col() +
  labs(
    title = "Remote vs onsite work in California",
    x = "Year",
    y = "Percent of workers",
    fill = "Work type"
  ) +
  theme_minimal()

#question 5:
#longer the table
remote_long <- remote_growth |>
  select(NAME, `2019`, `2021`, `2022`, `2023`, `2024`) |>
  pivot_longer(
    cols = c(`2019`, `2021`, `2022`, `2023`, `2024`),
    names_to = "year",
    values_to = "remote_share"
  ) |>
  mutate(year = as.numeric(year))

ggplot(remote_long, aes(x = year, y = remote_share, color = NAME, group = NAME)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Remote-work share by year",
    x = "Year",
    y = "Percent working from home",
    color = "State"
  ) +
  theme_minimal()
#Question 6:
pop_2019 <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1"
) |>
  select(GEOID, NAME, pop_2019 = estimate)

pop_2024 <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2024,
  survey = "acs1"
) |>
  select(GEOID,NAME , pop_2024 = estimate)

pop_change <- pop_2019 |>
  left_join(pop_2024, by = c("GEOID", "NAME")) |>
  mutate(
    pop_growth_pct = (pop_2024 - pop_2019) / pop_2019 * 100
  )

state_compare <- remote_growth |>
  left_join(pop_change, by = "NAME")

library(ggrepel)

ggplot(state_compare, aes(x = change_2019_2024, y = pop_growth_pct)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  geom_text_repel(
    data = subset(state_compare, NAME %in% target_states),
    aes(label = NAME),
    size = 4
  ) +
  labs(
    title = "Remote-work growth and population growth by state",
    x = "Change in remote-work share, 2019 to 2024",
    y = "Population growth (%), 2019 to 2024"
  ) +
  theme_minimal()