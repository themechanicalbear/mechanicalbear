---
title: Directional Trading and Price Extremes
author: Jason Taylor
date: '2018-07-14'
draft: TRUE
slug: directional-trading-and-price-extremes
summary: How do naked short options perform after large price movements and at price extremes?
categories:
  - Tastytrade
tags: []
header:
  caption: ''
  image: ''
---

### Market Measures - July 11, 2018

How do naked short options perform after large price movements and at price extremes?

*Let’s compare selling naked options into multiple days of trading in one direction to a large weekly move.*  
*The tastytrade Research Department used 45 DTE options to see if the following setup worked from 2005 to now.*  
*If the market went lower, we sold a 30 delta put, and if it went higher, we sold a 30 delta call.*  
*We find that that selling puts into weakness (either measured by days or % moves) did yield a substantial profit.*  
*Selling calls into strength yielded decent profits when waiting for large percentage moves up over number of up days.*  

Here I will recreate this study and extend it to include more underlyings again with emphasis on learning new coding practices. As always recreating the Market Measure study will be done utilizing the tastytrade package on  [github](https://github.com/themechanicalbear/tastytrade).

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tastytrade", "dplyr", "ggplot2", "here", "purrr", "tidyr",
                    "data.table")
  lapply(library_list, require, character.only = TRUE)})))

stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE", "EEM",
                "MA", "FB", "FXI", "SLV", "EWZ", "FXE", "TBT", "IBM")
tar_dte <- 45
tar_delta_put <- -.30
tar_delta_call <- .30
down_3_days_results <- data.frame()
all_closes <- data.frame()
all_loss_table <- data.frame()
all_results <- data.frame()
```

#### Study function


```r
# Consecutive down days (1, 2, 3) and all days
# Weekly move greater than (1, 2, 3 %)

# Calculate win rate as percentage
# Calculate Avg. P/L in $

# 30 delta calls in up moves and 30 delta puts in down moves

# Symbol (SPY)
```

#### Data


```r
stock <- "SPY"
options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
  dplyr::mutate(mid = (bid + ask) / 2)

each_day <- options %>%
  dplyr::distinct(quotedate, close)
```

#### Find consecutive down days


```r
down_days <- options %>%
  dplyr::distinct(quotedate, close) %>%
  dplyr::arrange(quotedate) %>%
  dplyr::mutate(down_one = ifelse(close < lag(close, 1), 1, 0),
                down_two = ifelse(close < lag(close, 2), 1, 0),
                down_three = ifelse(close < lag(close, 3), 1, 0))

down_one <- dplyr::filter(down_days, down_one == 1)
down_two <- dplyr::filter(down_days, down_one == 1, down_two == 1)
down_three <- dplyr::filter(down_days, down_one == 1, down_two == 1, down_three == 1)
```

#### Find consecutive up days


```r
up_days <- options %>%
  dplyr::distinct(quotedate, close) %>%
  dplyr::arrange(quotedate) %>%
  dplyr::mutate(up_one = ifelse(close > lag(close, 1), 1, 0),
                up_two = ifelse(close > lag(close, 2), 1, 0),
                up_three = ifelse(close > lag(close, 3), 1, 0))

up_one <- dplyr::filter(up_days, up_one == 1)
up_two <- dplyr::filter(up_days, up_one == 1, up_two == 1)
up_three <- dplyr::filter(up_days, up_one == 1, up_two == 1, up_three == 1)
```

#### Find weekly percentage changes

```r
weekly_change <- options %>%
  dplyr::distinct(quotedate, close) %>%
  dplyr::arrange(quotedate) %>%
  dplyr::mutate(year = lubridate::year(quotedate),
                week = lubridate::week(quotedate)) %>%
  dplyr::group_by(year, week) %>%
  dplyr::mutate(rank = rank(quotedate)) %>%
  dplyr::filter(rank == min(rank) | rank == max(rank)) %>%
  dplyr::mutate(delta = close - lag(close, 1)) %>%
  dplyr::mutate(delta = ifelse(is.na(delta), max(delta, na.rm = TRUE), delta)) %>%
  dplyr::filter(rank == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(return = (delta / close) * 100)%>% 
  dplyr::filter(!return == "-Inf")

wk_dwn_one_pct <-  dplyr::filter(weekly_change, return <= -1)
wk_dwn_two_pct <-  dplyr::filter(weekly_change, return <= -2)
wk_dwn_three_pct <-  dplyr::filter(weekly_change, return <= -3)

wk_up_one_pct <-  dplyr::filter(weekly_change, return >= 1)
wk_up_two_pct <-  dplyr::filter(weekly_change, return >= 2)
wk_up_three_pct <-  dplyr::filter(weekly_change, return >= 3)
```

#### Sell puts consecutive down days


```r
down_3_days <- options %>%
  dplyr::filter(quotedate %in% down_three$quotedate) %>%
  dplyr::mutate(m_dte = abs(dte - tar_dte))

short_put_opens_down_3_days <- tastytrade::open_short_put(down_3_days, "SPY",
                                                          tar_delta_put)

down_2_days <- options %>%
  dplyr::filter(quotedate %in% down_two$quotedate) %>%
  dplyr::mutate(m_dte = abs(dte - tar_dte))

short_put_opens_down_2_days <- tastytrade::open_short_put(down_2_days, "SPY",
                                                          tar_delta_put)

down_1_days <- options %>%
  dplyr::filter(quotedate %in% down_one$quotedate) %>%
  dplyr::mutate(m_dte = abs(dte - tar_dte))

short_put_opens_down_1_days <- tastytrade::open_short_put(down_1_days, "SPY",
                                                          tar_delta_put)
```

#### Sell puts on all days


```r
all_days <- options %>%
  dplyr::mutate(m_dte = abs(dte - tar_dte))

short_put_opens_all_days <- tastytrade::open_short_put(all_days, "SPY",
                                                       tar_delta_put)
```

#### Close put function


```r
possible_closes <- function(date, exp, p_strike, credit) {
  closes <- options %>%
    dplyr::filter(expiration == exp, quotedate == exp, 
                  strike == p_strike & type == "put") %>%
    dplyr::group_by(quotedate) %>%
    dplyr::mutate(open_date = as.Date(date, origin = "1970-01-01"),
                  open_credit = credit,
                  debit = sum(mid),
                  profit = open_credit - debit) %>%
    dplyr::ungroup() %>%
    dplyr::select(symbol, quotedate, expiration, open_date, 
                  open_credit, debit, profit) %>%
    dplyr::distinct()
}
```

#### Close puts consecutive down days


```r
down_3_days_results <- purrr::pmap_dfr(list(short_put_opens_down_3_days$quotedate,
                                            short_put_opens_down_3_days$expiration,
                                            short_put_opens_down_3_days$strike_put,
                                            short_put_opens_down_3_days$mid_put), 
                                       possible_closes) %>%
  dplyr::right_join(down_three, by = c("open_date" = "quotedate")) %>%
  dplyr::filter(complete.cases(.))

down_2_days_results <- purrr::pmap_dfr(list(short_put_opens_down_2_days$quotedate,
                                            short_put_opens_down_2_days$expiration,
                                            short_put_opens_down_2_days$strike_put,
                                            short_put_opens_down_2_days$mid_put), 
                                       possible_closes) %>%
  dplyr::right_join(down_two, by = c("open_date" = "quotedate")) %>%
  dplyr::filter(complete.cases(.))

down_1_days_results <- purrr::pmap_dfr(list(short_put_opens_down_1_days$quotedate,
                                            short_put_opens_down_1_days$expiration,
                                            short_put_opens_down_1_days$strike_put,
                                            short_put_opens_down_1_days$mid_put), 
                                       possible_closes) %>%
  dplyr::right_join(down_one, by = c("open_date" = "quotedate")) %>%
  dplyr::filter(complete.cases(.))

all_days_results <- purrr::pmap_dfr(list(short_put_opens_all_days$quotedate,
                                         short_put_opens_all_days$expiration,
                                         short_put_opens_all_days$strike_put,
                                         short_put_opens_all_days$mid_put), 
                                    possible_closes) %>%
  dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
  dplyr::filter(complete.cases(.))
```

#### Results of the down days


```r
# Win rates and average P/L per trade
mean_pl_3 <- mean(down_3_days_results$profit)
win_rate_3 <- nrow(dplyr::filter(down_3_days_results, profit > 0)) / 
  nrow(down_3_days_results)
mean_pl_2 <- mean(down_2_days_results$profit)
win_rate_2 <- nrow(dplyr::filter(down_2_days_results, profit > 0)) / 
  nrow(down_2_days_results)
mean_pl_1 <- mean(down_1_days_results$profit)
win_rate_1 <- nrow(dplyr::filter(down_1_days_results, profit > 0)) / 
  nrow(down_1_days_results)
mean_pl_all <- mean(all_days_results$profit)
win_rate_all <- nrow(dplyr::filter(all_days_results, profit > 0)) / 
  nrow(all_days_results)

down_days_results <- data.frame(metric = c("Win Rate", "Avg. P/L"),
                                all_days = c(win_rate_all, mean_pl_all),
                                one_day = c(win_rate_1, mean_pl_1),
                                two_days = c(win_rate_2, mean_pl_2),
                                three_days = c(win_rate_3, mean_pl_3))
print(down_days_results, digits = 2)
```

```
##     metric all_days one_day two_days three_days
## 1 Win Rate     0.91     0.9     0.91        0.9
## 2 Avg. P/L     1.42     1.4     1.50        1.6
```




