library(tidyverse)
library(readxl)
corp_tax  <- read_excel("corp_tax.xlsx")

corp_tax %>%
  slice_sample(n = 10)

corp_tax  %>%
  filter(company == "Amazon.com")  %>%
  select(tax_rate)

corp_tax  %>%
  filter(company == "Facebook")  %>%
  select(tax_rate)

corp_tax  %>%
  slice_max(profit, n=3)

corp_tax  %>%
  arrange(desc(profit))

corp_tax  %>%
  group_by(industry)  %>%
  summarize(Profit = sum(profit),
            Tax = sum(tax))  %>%
  mutate(Rate = Tax / Profit * 100)  %>%
  arrange(Rate)


#################################
#  Combine Tables
#################################


library(tidyverse)


x  <- tribble(
  ~ticker, ~name, ~revenue,
  "TPR", "Tapestry", 5880,
  "ALL", "Allstate", 39815,
  "FFIV", "F5 Networks Inc", 361
)

x

y  <- tribble(
  ~ticker, ~name, ~cor,
  "TPR", "Tapestry", 2031,
  "ALL", "Allstate", 25466,
  "DLTR", "Dollar Tree", 15876
)

y

x  %>% bind_cols(y)

x  %>% bind_rows(y)

x  %>% inner_join(y)

x  %>% full_join(y)

x  %>% left_join(y)

x  %>% right_join(y)

x  %>%  semi_join(y)

x  %>% anti_join(y)


#################################
#  echarts-4r-ex-1
#################################

spend_time  %>%
  group_by(year)  %>%
  e_charts(x = activity, timeline = TRUE)  %>%
  e_timeline_opts(autoPlay = TRUE)  %>%
  e_bar(serie = avg_hours)  %>%
  e_title(text = 'Avge')  %>%
  e_legend(show = FALSE)


#################################
#  echarts-4r-ex-2
#################################
spend_time  %>%
  mutate(year = paste(year, "12","31", sep = "-"))  %>%
  mutate(year = lubridate::ymd(year))  %>%
  group_by(activity)  %>%
  e_charts(year)  %>%
  e_line(avg_hours)  %>%
  e_tooltip()  %>%
  e_title(text = 'Average hours Americans spend per day on each activity')  %>%
  e_legend(top = 50) #move legend down from top




spend_time  %>%
  mutate(year = paste(year, "12","31", sep = "-"))  %>%
  mutate(year = lubridate::ymd(year))  %>%
  group_by(activity)  %>%
  e_charts(x = year)  %>%
  e_line(serie = avg_hours)  %>%
  e_tooltip()
