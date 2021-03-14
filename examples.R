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
