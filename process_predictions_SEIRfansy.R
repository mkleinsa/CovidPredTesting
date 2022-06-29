suppressWarnings({
  library(plotly)
  library(tidyverse)
})

data_link = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/seir/prediction_tt_latest.txt"

tmp = read_tsv(data_link, col_types = cols()) %>%
  select(state, date, section, pred, value = mean) %>%
  pivot_wider(names_from = "section", values_from = "value",
              id_cols = c("date", "pred", "state"))

tmp = tmp %>%
  rename(case_daily_reported = positive_daily_reported) %>%
  mutate(death_daily_unreported = death_unreported - lag(death_unreported),
         case_daily_unreported = unreported_daily) %>%
  select(state, date, pred, case_daily_reported, death_daily_reported) %>%
  filter(pred == 1) %>%
  filter(date <= min(date) + 30) %>%
  pivot_longer(cols = c(case_daily_reported, death_daily_reported))

tmp =
  tmp %>%
  mutate(text = paste0(format(round(value, digits = 0), big.mark=","), " daily ",
                       str_extract(name, "[^_]+"), "s"))

s = "India"
p.title = paste0("Projected daily number of new cases and deaths\nin ", s)
cases.xaxis = "Date"
cases.yaxis = "Daily counts"

cases = plot_ly(
  tmp %>% filter(name == "case_daily_reported"), x = ~ date, y = ~ value,
  text = ~ text, hoverinfo = "text", type = "bar", hoverlabel = list(align = "left"),
  showlegend = F, marker = list(color = "#FF9933")) %>%
  layout(annotations = list(text = "SEIR daily new cases", xref = "paper",
                            yref = "paper", xanchor = "left", x = 0, y = 1.1,
                            showarrow = F, font = list(size = 22)),
         xaxis = list(title = "Date"), yaxis = list(title = "Daily counts"))

deaths = plot_ly(
  tmp %>% filter(name == "death_daily_reported"),
  x = ~ date, y = ~ value, text = ~ text, hoverinfo = "text", type = "bar",
  hoverlabel = list(align = "left"), showlegend = F, marker = list(color = "#138808")) %>%
    layout(annotations = list(text = "SEIR daily new deaths", xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22)), xaxis = list(title = "Date"),
           yaxis = list(title = "Daily counts"))

subplot(cases, deaths, titleX = T, titleY = T, margin = .08, nrows = 2,
        shareX = F) %>% plotly::config(toImageButtonOptions = list(width = NULL,
                                                                   height = NULL))

data = tmp %>%
  select(name, date, value, text) %>%
  pivot_wider(names_from = c("name"), values_from = c("value", "text")) %>%
  rename(daily_cases = value_case_daily_reported,
         daily_deaths = value_death_daily_reported,
         daily_cases_text = text_case_daily_reported,
         daily_deaths_text = text_death_daily_reported) %>%
  mutate(place = "India")

data = data %>%
  mutate(daily_deaths = round(daily_deaths),
         daily_cases = round(daily_cases))

write_csv(data, "../CovidPredTesting/SEIR_predictions.csv")
