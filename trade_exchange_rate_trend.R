setwd("D:/pkic/forecasting/exchange_rate/data")

# Upload data -------------------------------------------------------------

library(xlsx)
library(tidyverse)
library(lubridate)
library(zoo)


# Exchange Rate ----------------------------------------------------------

##1 Sheet
exchange_rate <- read.xlsx("D:/pkic/forecasting/exchange_rate/data/IBF_Arch.xlsx",
                           sheetName =  "Monthly Avg. Ex. Rates in PKR",
                           startRow = 9) 

colnames(exchange_rate)[1:2] <- c("month", "year")

exchange_rate <- exchange_rate %>% unite(
  col = "date_dirty", c("month", "year"), sep = ", "
) %>% mutate(
  date_dirty = as.Date(parse_date_time(date_dirty, "%b, %Y"))
) %>% select(
  date_dirty, U.S..Dollar
) %>% na.omit(.) %>% 
  rename(PKR_USD = U.S..Dollar)

exchange_rate <- exchange_rate %>% ungroup()  %>% mutate(
  date_yearmon = as.yearmon(date_dirty)
)


# External Trade ----------------------------------------------------------

setwd("D:/pkic/forecasting/exchange_rate/data")

# Upload data -------------------------------------------------------------

library(xlsx)
library(tidyverse)


# External Trade ----------------------------------------------------------

external_trade <- read.xlsx("D:/pkic/forecasting/exchange_rate/data/exp_import_BOP_Arch.xlsx",
                            sheetIndex   = 1, startRow = 8) %>% setNames(
                              make.names(names(.), unique = T)
                            ) %>% select(
                              matches("Period|BOP|Balance")
                            ) %>% mutate(
                              Period = as.numeric(as.character(Period)),
                              Period = janitor::excel_numeric_to_date(Period)
                            ) %>% na.omit(.)


# Clean Column Names and Make them into numeric ---------------------------

external_trade <- external_trade %>% 
  rename(period  = Period,
         exports = Exports..BOP.,
         imports = Imports..BOP.,
         balance_trade = Balance.of.Trade) %>% 
  mutate_at(vars(-period), ~as.numeric(as.character(.)))



# PLOT THE TREND ----------------------------------------------------------


shade = data.frame(x1=c("2018-01-20"),
                   x2=c("2020-01-15"),
                   y1=c(-Inf), y2=c(Inf))

shade$x1 <- decimal_date(as.Date(shade$x1, "%Y-%m-%d"))
shade$x2 <- decimal_date(as.Date(shade$x2, "%Y-%m-%d"))

external_trade %>% 
  filter(period > "2005-01-01") %>% 
  mutate_at(vars(-one_of("period")), ~as.numeric(as.character(.))) %>% 
  rename(date_dirty = period) %>% 
  mutate(date_yearmon = as.yearmon(date_dirty)) %>%
  left_join(., exchange_rate, by = "date_yearmon") %>%
  select(-date_yearmon,-date_dirty.x) %>%
  rename(date_dirty = date_dirty.y) %>% 
  pivot_longer(-date_dirty, names_to = "header",
               values_to = "values") %>%
  mutate(header = case_when(
    header == "balance_trade" ~ "Trade Balance(USD Million)",
    header == "exports" ~ "Export(USD Million)",
    header == "imports" ~ "Imports(USD Million)",
    TRUE ~ as.character(header))) %>% 
  group_by(header) %>% 
  mutate(values = rollapply(values, 6, mean,align = "right", fill = NA)) %>% 
  mutate(date_dirty = decimal_date(date_dirty)) %>%
  ungroup() %>% 
  mutate_at(vars(header), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot() + 
  geom_line(aes(x=date_dirty, y=values, color = header), size = 1)+
  geom_rect(data=shade, 
            mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),
            color='gray', alpha=0.4)+
  theme_bw()+
  facet_grid(header~., scales = "free_y",
             labeller=label_wrap_gen(width=10))+
  ylab("") + xlab("")+
  theme(legend.title = element_blank())+
  theme(strip.text.y = element_text(size = 7))+
  ggtitle("Exchange Rate & External Trade")

