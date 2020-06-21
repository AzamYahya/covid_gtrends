require("quantmod")
if(!require(gtrendsR)){
  install.packages("gtrendsR")
  library(gtrendsR)
}

sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo",
           from = as.Date("2019-06-23"), to = as.Date("2020-06-12"))


GSPC <- sp500$GSPC
##into dataframe

SnP500_df <- data.frame(date = index(GSPC$GSPC.Close),
                        coredata(GSPC$GSPC.Close)) %>% 
                        rename(snp500_close = GSPC.Close)

##Get Google trend data

trend_data <- gtrends(c("Corona", "Covid"),
                      time = "today 12-m", gprop = "web")

trend_data_temp <- trend_data$interest_over_time

trend_data_clean <- trend_data_temp %>% filter(
  geo == "world") %>% mutate(
    hits = ifelse(hits == "<1", 0, hits),
    hits = as.numeric(as.character(hits)), 
    date = as.Date(date, format = "%Y-%m-%d") - 3,
    )

trend_data_clean %>% 
  left_join(., SnP500_df, by = "date") %>% 
  select(date, hits, snp500_close) %>% 
  rename('Google Trend Index' = hits,
         "S&P500" = snp500_close) %>% 
  filter(date > "2020-01-01") %>% 
  pivot_longer(-date, 
               names_to = "header",
               values_to = "values") %>%
  ggplot(aes(x = date, y = values))+
  geom_line(aes(color = header), size = 1) +
  theme_bw()+
  facet_grid(header~., scales = "free_y")+
  xlab("") + ylab("") +
  theme(legend.title=element_blank())
  
