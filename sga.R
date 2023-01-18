library(prophet)
library(tidyverse)

raw_view <- read.csv("pageviews-20150701-20230112.csv")

#turn to log form
raw_view <- within(raw_view, y <- log(sga))

#start with the first view
df <- subset(raw_view, y > 0)
df <- rename(df, ds = Date)

#simple prophet model
simple <- prophet(df)

#predict 1 yr number of view
future <- make_future_dataframe(simple, periods = 365)
simple_forecast <- predict(simple, future)

plot(simple, simple_forecast)

#check trend, yearly seasonality, weekly seasonality
prophet_plot_components(simple, simple_forecast)

#alternatively, we may think that SGA has been being well-known and the daily
#view will not keep increasing if he cannot break into semi-final, which is likely
#to happen in the period we predict(1 yr), therefore we can use a logistic growth
#model instead the default linear model in prophet

#set a cap
df$cap <- 15

#predict with a logistic growth model
logistic_m <- prophet(df, growth = "logistic")

logistic_future <- make_future_dataframe(logistic_m, periods = 365)
logistic_future$cap <- 15
logistic_forecast <- predict(logistic_m, logistic_future)
plot(logistic_m, logistic_forecast)

prophet_plot_components(logistic_m, logistic_forecast)

#now we create a customized holiday schedule for SGA, covering his playoffs games,
#draft day, trade, all-rookie third team award, and 2018-19 all star weekend. Other factors like highlighting
#perforemance are not covered, since they have a low effect on view of wikipedia page

playoffs <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2019-04-13','2019-04-15','2019-04-18','2019-04-21',
                 '2019-04-24','2019-04-26','2020-08-18','2020-08-20',
                 '2020-08-22','2020-08-24','2020-08-29','2020-08-31',
                 '2020-09-02')),
  lower_window = 0,
  upper_window = 1
)

draftday <- data_frame(
  holiday = 'drafyday',
  ds = as.Date(c('2018-06-21')),
  lower_window = 0,
  upper_window = 2
)

OKCtrade <- data_frame(
  holiday = 'OKCtrade',
  ds = as.Date(c('2019-07-10')),
  lower_window = 0,
  upper_window = 2
)

allrookie <- data_frame(
  holiday = 'allrookie',
  ds = as.Date(c('2018-05-21')),
  lower_window = 0,
  upper_window = 1
)

allstar <- data_frame(
  holiday = 'allstar',
  ds = as.Date(c('2019-02-17')),
  lower_window = -2,
  upper_window = 0
)

holidays <- bind_rows(playoffs, draftday, OKCtrade, allrookie, allstar)

simple_h <- prophet(df, holidays = holidays)
forecast_h <- predict(simple_h, future)

plot(simple_h, forecast_h)

prophet_plot_components(simple_h, forecast_h)

#We can also add national holidays by country, for example
#simple_h <- add_country_holidays(simple, country_name = 'US')
#simple_h <- fit.prophet(simple_h, df)

#However that's not a good idea here since people won't search SGA's wiki for
#holiday reason. While someone may argue that Chrismas games are a factor,
#we decide not to include it here.

#It is reasonable that the number of views is affected by NBA seasons, views are
#higher during NBA seasons than off-seasons, therefore we'll adjust this factor

is_nba_season <- function(ds){
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  return(month > 9 | month < 4)
}
df$on_season <- is_nba_season(df$ds)
df$off_season <- !is_nba_season(df$ds)

#We need to disable weekly seasonality to implement 2 different weekly seasonality
model1 <- prophet(holidays = holidays, weekly.seasonality = F)
model1 <- add_seasonality(model1, name = 'weekly_on_season', period = 7, fourier.order = 3, condition.name = 'on_season')
model1 <- add_seasonality(model1, name = 'weekly_off_season', period = 7, fourier.order = 3, condition.name = 'off_season')
model1 <- fit.prophet(model1, df)

future1 <- make_future_dataframe(model1, periods = 365)
future1$on_season <- is_nba_season(future1$ds)
future1$off_season <- !is_nba_season(future$ds)
forecast1 <- predict(model1, future1)

plot(model1, forecast1)
prophet_plot_components(model1, forecast1)

#results show that during the NBA season, the number of views has a peak on Saturday.
#The interpretation is that people watch NBA games on Saturday more.
#However, we find that the off-season weekly seasonality is a little weird, so as
#yearly seasonality. The reason is that we missed the effect of the pandemic.
#And for NBA, the effect lasted even longer, because the league adjusted their
#schedules for the 2018-19 and 2019-20 seasons.

#we can also add uncertainty in seasonality, though this takes extra time
model_mcmc <- prophet(df, mcmc.samples = 300)
future_mcmc <- make_future_dataframe(model_mcmc, periods = 365)
forecast_mcmc <- predict(model_mcmc, future_mcmc)

plot(model_mcmc, forecast_mcmc)
prophet_plot_components(model_mcmc, forecast_mcmc, weekly seasonality)

#dealing with outliers
outliers <- (as.Date(df$ds) > as.Date('2018-03-09')
             & as.Date(df$ds) < as.Date('2018-03-24'))

df$y[outliers] = NA

model2 <- prophet(holidays = holidays, weekly.seasonality = F)
model2 <- add_seasonality(model2, name = 'weekly_on_season', period = 7, fourier.order = 3, condition.name = 'on_season')
model2 <- add_seasonality(model2, name = 'weekly_off_season', period = 7, fourier.order = 3, condition.name = 'off_season')
model2 <- fit.prophet(model2, df)

future2 <- make_future_dataframe(model2, periods = 365)
future2$on_season <- is_nba_season(future2$ds)
future2$off_season <- !is_nba_season(future2$ds)
forecast2 <- predict(model2, future2)

plot(model2, forecast2)
prophet_plot_components(model2, forecast2)

#dealing with the pandemic
lockdowns <- data_frame(
  holiday = 'lockdowns',
  ds = as.Date(c('2020-03-11')),
  lower_window = 0,
  upper_window = 139
)

restart <- data_frame(
  holiday = 'restart',
  ds = as.Date(c('2020-07-30')),
  lower_window = 0,
  upper_window = 16
)

offseason19 <- data_frame(
  holiday = 'offseason19',
  ds = as.Date(c('2020-08-17')),
  lower_window = 0,
  upper_window = 125
)


holidays <- bind_rows(playoffs, draftday, OKCtrade, allrookie, allstar,
                      lockdowns, restart, offseason19)

model3 <- prophet(holidays = holidays, weekly.seasonality = F)
model3 <- add_seasonality(model3, name = 'weekly_on_season', period = 7, fourier.order = 3, condition.name = 'on_season')
model3 <- add_seasonality(model3, name = 'weekly_off_season', period = 7, fourier.order = 3, condition.name = 'off_season')
model3 <- fit.prophet(model3, df)

future3 <- make_future_dataframe(model3, periods = 365)
future3$on_season <- is_nba_season(future3$ds)
future3$off_season <- !is_nba_season(future3$ds)
forecast3 <- predict(model3, future3)

plot(model3, forecast3)
prophet_plot_components(model3, forecast3)

#cross-validation test
df.cv <- cross_validation(model3, horizon = 365, units = 'days')

df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mape')

#save prophet model
saveRDS(model3, file = 'model_SGA.RDS')
