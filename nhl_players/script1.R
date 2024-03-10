library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2024-01-09")
canada_births_1991_2022 <- tuesdata$canada_births_1991_2022
nhl_player_births <- tuesdata$nhl_player_births
nhl_rosters <- tuesdata$nhl_rosters
nhl_teams <- tuesdata$nhl_teams
#
#
group_year <- factor(canada_births_1991_2022$year)
births_year <- tapply(
  canada_births_1991_2022$births,
  group_year,
  sum
)
# check if tapply working properly
sum(canada_births_1991_2022$births)
sum(births_year)
#
group_month <- factor(canada_births_1991_2022$month)
births_month <- tapply(
  canada_births_1991_2022$births, 
  group_month,
  sum
)
sum(births_month)
#
plot(births_year, x = names(births_year))
plot(births_month, x = names(births_month))
# z jakich krajów pochodzą gracze nhl?
playersByCountry <- nhl_player_births %>%
  group_by(birth_country) %>%
  summarize(num = n()) %>%
  mutate(perc = round(num / sum(num) * 100, 2))
# jak rozkladaja sie urodzenia graczy
playersByYear <- nhl_player_births %>%
  group_by(birth_year) %>%
  summarize(num = n()) %>%
  mutate(perc = round(num / sum(num) * 100, 2))
# tylko kanadyjscy gracze
can_players <- nhl_player_births |>
  filter(birth_country == "CAN")
# tylko kanadyjscy gracze urodzeni w przedziale z df canada_births
can_players_year <- can_players |>
  filter(birth_year >= 1991 & birth_year <= 2022)
# rozkład kanadyjskich graczy z podzialem na rok urodzenia
can_players_births_year <- can_players_year |> 
  group_by(birth_year) |> 
  summarize(n())
# create missing birth_date column, based od bith_date (date format)
nhl_rosters <- nhl_rosters |> 
  mutate(birth_year = format(nhl_rosters$birth_date, "%Y"))
# na jakich pozycjach grają can gracze urodzeni 1991-2022
can_players_year_positions <- nhl_rosters |> 
  filter(birth_country == "CAN") |> 
  filter(birth_year >= 1991 & birth_year <= 2022) |> 
  group_by(position_type) |> 
  summarize(num = n())
# barplot pozycji can gracze urodzeni 1991-2022
barplot(
  can_players_year_positions$num, 
  names.arg = can_players_year_positions$position_type,
)
# is there a higher likelihood for ALL players to be born 
# in the first quarter of the year. (ALL YEARS)
nhl_rosters_edit <- nhl_rosters |> 
  mutate(
    birth_year = format(nhl_rosters$birth_date, "%Y"),
    birth_month = as.numeric(format(nhl_rosters$birth_date, "%m"))
  )
# check if class is ok (it was not w/o as.numeric())
class(nhl_rosters_edit$birth_month)
# make quarters: (probably not to tidy)
quarter1 = c(1:3)
quarter2 = c(4:6)
quarter3 = c(7:9)
quarter4 = c(10:12)
#
nhl_rosters_edit_quarters <- nhl_rosters_edit |>
  mutate(
    quarter = case_when(
      birth_month %in% quarter1 ~ 1,
      birth_month %in% quarter2 ~ 2,
      birth_month %in% quarter3 ~ 3,
      birth_month %in% quarter4 ~ 4
    )
  ) 
quarter_factor <- factor(nhl_rosters_edit_quarters$quarter)
quarter_res <- table(quarter_factor)
barplot(quarter_res)
# ANS: sort of, ale moze statystyke jeszcze by jakąś dziabnął
#
# is there a higher likelihood for 1991-2002 born CANADIAN players to be born 
# in the first quarter of the year.
can_players_year_quarters <- can_players_year |>
  mutate(
    quarter = case_when(
      birth_month %in% quarter1 ~ 1,
      birth_month %in% quarter2 ~ 2,
      birth_month %in% quarter3 ~ 3,
      birth_month %in% quarter4 ~ 4
    )
  ) 
quarter_factor_can <- factor(can_players_year_quarters$quarter)
quarter_can_res <- table(quarter_factor_can)
barplot(quarter_can_res)
# ANS: tyle samo lub podobnie co w całej skali wszystkich graczy
#
# Maybe more people in Canada in general are born earlier in the year?
canada_births_1991_2022
canada_births_1991_2022_quarters <- canada_births_1991_2022 |>
  mutate(
    quarter = case_when(
      month %in% quarter1 ~ 1,
      month %in% quarter2 ~ 2,
      month %in% quarter3 ~ 3,
      month %in% quarter4 ~ 4
    )
  ) 
quarter_factor_all_can <- factor(canada_births_1991_2022_quarters$quarter)
table(quarter_factor_all_can)
tapply(
  canada_births_1991_2022_quarters,
  quarter_factor_all_can,
  sum
)
# ANS: NO!
#
# refactoring; a function for quarters
add_quarter_col <- function (df_input, df_col) {
  df_input |> 
    mutate(
      quarter = case_when(
        df_col %in% quarter1 ~ 1,
        df_col %in% quarter2 ~ 2,
        df_col %in% quarter3 ~ 3,
        df_col %in% quarter4 ~ 4
      )
    ) 
}
add_quarter_col(canada_births_1991_2022, canada_births_1991_2022$month)

