---
title: Directional Trading and Price Extremes
author: Jason Taylor
date: '2018-07-11'
draft: FALSE
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

How do naked short options perform after large price movements or price extremes?

The tastytrade Research Department used 45 DTE options in SPY to see if the following setup worked from 2005 to now.
I will use 14 symbols that I have the most data for from Jan 2012 - March 2018.
If the market went lower, sell a 30 delta put, and if it went higher, sell a 30 delta call. 

#### Tastytrade results:  
We find that that selling puts into weakness (either measured by days or % moves) did yield a substantial profit. Selling calls into strength yielded decent profits when waiting for large percentage moves up over number of up days.  

#### My Results:
I found that there was variation in results between symbols and less success in selling calls than puts. One possible reason for the difference in results in SPY alone is the date range as the period 2012-18 is decidedly bullish and calls did not perform well here. Several symbols lost money in all slices. See results tables at the end of this post. More analysis can be done here, and I may come back to this one.

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
```


```r
study <- function(stock) {
  options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
    dplyr::mutate(mid = (bid + ask) / 2,
                  m_dte = abs(dte - tar_dte))
  expirations <- dplyr::filter(options, quotedate == expiration)
  
  each_day <- dplyr::distinct(options, quotedate, close)
  
  consecutive_days <- options %>%
    dplyr::distinct(quotedate, close) %>%
    dplyr::arrange(quotedate) %>%
    dplyr::mutate(down_one = ifelse(close < lag(close, 1), 1, 0),
                  down_two = ifelse(close < lag(close, 2), 1, 0),
                  down_three = ifelse(close < lag(close, 3), 1, 0),
                  up_one = ifelse(close > lag(close, 1), 1, 0),
                  up_two = ifelse(close > lag(close, 2), 1, 0),
                  up_three = ifelse(close > lag(close, 3), 1, 0))
  
  down_one <- dplyr::filter(consecutive_days, down_one == 1)
  down_two <- dplyr::filter(consecutive_days, down_one == 1, down_two == 1)
  down_three <- dplyr::filter(consecutive_days, down_one == 1, down_two == 1,
                              down_three == 1)
  
  up_one <- dplyr::filter(consecutive_days, up_one == 1)
  up_two <- dplyr::filter(consecutive_days, up_one == 1, up_two == 1)
  up_three <- dplyr::filter(consecutive_days, up_one == 1, up_two == 1, up_three == 1)
  
  weekly_change <- options %>%
    dplyr::distinct(quotedate, close) %>%
    dplyr::arrange(quotedate) %>%
    dplyr::mutate(year = lubridate::year(quotedate),
                  week = lubridate::week(quotedate)) %>%
    dplyr::group_by(year, week) %>%
    dplyr::mutate(rank = rank(quotedate)) %>%
    dplyr::filter(rank == min(rank) | rank == max(rank)) %>%
    dplyr::mutate(delta = close - lag(close, 1)) %>%
    dplyr::mutate(delta = ifelse(is.na(delta), max(delta, na.rm = TRUE),
                                 delta)) %>%
    dplyr::filter(rank == 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(return = (delta / close) * 100) %>% 
    dplyr::filter(!return == "-Inf")
  
  wk_dwn_one <-  dplyr::filter(weekly_change, return <= -1)
  wk_dwn_two <-  dplyr::filter(weekly_change, return <= -2)
  wk_dwn_three <-  dplyr::filter(weekly_change, return <= -3)
  
  wk_up_one <-  dplyr::filter(weekly_change, return >= 1)
  wk_up_two <-  dplyr::filter(weekly_change, return >= 2)
  wk_up_three <-  dplyr::filter(weekly_change, return >= 3)
  
  opened_puts <- tastytrade::open_short(options, tar_delta_put, "put")
  opened_calls <- tastytrade::open_short(options, tar_delta_call, "call")
  
  # opened_puts <- tastytrade::open_short_put(options, "SPY", tar_delta_put)
  # opened_calls <- tastytrade::open_short_call(options, "SPY", tar_delta_call)
  
  closed_puts <- purrr::pmap_dfr(list(list(expirations),
                                      opened_puts$quotedate,
                                      opened_puts$expiration,
                                      opened_puts$strike,
                                      opened_puts$mid), 
                                 tastytrade::close_short_put_exp) %>%
    dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
    dplyr::filter(complete.cases(.))
  
  closed_calls <- purrr::pmap_dfr(list(list(expirations),
                                       opened_calls$quotedate,
                                       opened_calls$expiration,
                                       opened_calls$strike,
                                       opened_calls$mid), 
                                  tastytrade::close_short_call_exp) %>%
    dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
    dplyr::filter(complete.cases(.))
  
  down_3d <- dplyr::filter(closed_puts, open_date %in% down_three$quotedate)
  down_2d <- dplyr::filter(closed_puts, open_date %in% down_two$quotedate)
  down_1d <- dplyr::filter(closed_puts, open_date %in% down_one$quotedate)
  
  down_3p <- dplyr::filter(closed_puts, open_date %in% wk_dwn_three$quotedate)
  down_2p <- dplyr::filter(closed_puts, open_date %in% wk_dwn_two$quotedate)
  down_1p <- dplyr::filter(closed_puts, open_date %in% wk_dwn_one$quotedate)
  
  up_3d <- dplyr::filter(closed_calls, open_date %in% up_three$quotedate)
  up_2d <- dplyr::filter(closed_calls, open_date %in% up_two$quotedate)
  up_1d <- dplyr::filter(closed_calls, open_date %in% up_one$quotedate)
  up_3p <- dplyr::filter(closed_calls, open_date %in% wk_up_three$quotedate)
  up_2p <- dplyr::filter(closed_calls, open_date %in% wk_up_two$quotedate)
  up_1p <- dplyr::filter(closed_calls, open_date %in% wk_up_one$quotedate)
  
  # Win rates and average P/L per trade
  mean_d3d <- mean(down_3d$profit)
  mean_d2d <- mean(down_2d$profit)
  mean_d1d <- mean(down_1d$profit)
  mean_d3p <- mean(down_3p$profit)
  mean_d2p <- mean(down_2p$profit)
  mean_d1p <- mean(down_1p$profit)
  mean_u3d <- mean(up_3d$profit)
  mean_u2d <- mean(up_2d$profit)
  mean_u1d <- mean(up_1d$profit)
  mean_u3p <- mean(up_3p$profit)
  mean_u2p <- mean(up_2p$profit)
  mean_u1p <- mean(up_1p$profit)
  mean_dad <- mean(closed_puts$profit)
  mean_uad <- mean(closed_calls$profit)
  
  win_d3d <- nrow(dplyr::filter(down_3d, profit > 0)) / nrow(down_3d)
  win_d2d <- nrow(dplyr::filter(down_2d, profit > 0)) / nrow(down_2d)
  win_d1d <- nrow(dplyr::filter(down_1d, profit > 0)) / nrow(down_1d)
  win_d3p <- nrow(dplyr::filter(down_3p, profit > 0)) / nrow(down_3p)
  win_d2p <- nrow(dplyr::filter(down_2p, profit > 0)) / nrow(down_2p)
  win_d1p <- nrow(dplyr::filter(down_1p, profit > 0)) / nrow(down_1p)
  win_u3d <- nrow(dplyr::filter(up_3d, profit > 0)) / nrow(up_3d)
  win_u2d <- nrow(dplyr::filter(up_2d, profit > 0)) / nrow(up_2d)
  win_u1d <- nrow(dplyr::filter(up_1d, profit > 0)) / nrow(up_1d)
  win_u3p <- nrow(dplyr::filter(up_3p, profit > 0)) / nrow(up_3p)
  win_u2p <- nrow(dplyr::filter(up_2p, profit > 0)) / nrow(up_2p)
  win_u1p <- nrow(dplyr::filter(up_1p, profit > 0)) / nrow(up_1p)
  win_dad <- nrow(dplyr::filter(closed_puts, profit > 0)) / nrow(closed_puts)
  win_uad <- nrow(dplyr::filter(closed_calls, profit > 0)) / nrow(closed_calls)
  
  data.frame(symbol = stock,
             direction = c("down", "down", "up", "up"),
             metric = c("Win Rate", "Avg. P/L"),
             all_days = c(win_dad, mean_dad, win_uad, mean_uad),
             one_day = c(win_d1d, mean_d1d, win_u1d, mean_u1d),
             two_days = c(win_d2d, mean_d2d, win_u2d, mean_u2d),
             three_days = c(win_d3d, mean_d3d, win_u3d, mean_u3d),
             one_percent = c(win_d1p, mean_d1p, win_u1p, mean_u1p),
             two_percent = c(win_d2p, mean_d2p, win_u2p, mean_u2p),
             three_percent = c(win_d3p, mean_d3p, win_u3p, mean_u3p))
}
```


```r
all_results <- purrr::map_dfr(stock_list, study)
```


```r
win_rate_puts <- dplyr::filter(all_results, metric == "Win Rate", 
                               direction == "down") %>%
  dplyr::select(-c(direction, metric)) %>%
  dplyr::arrange(desc(all_days))
knitr::kable(win_rate_puts, digits = 2, format = "html", 
             caption = "Win rates for selling puts into down moves")
```

<table>
<caption>Table 1 Win rates for selling puts into down moves</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:right;"> all_days </th>
   <th style="text-align:right;"> one_day </th>
   <th style="text-align:right;"> two_days </th>
   <th style="text-align:right;"> three_days </th>
   <th style="text-align:right;"> one_percent </th>
   <th style="text-align:right;"> two_percent </th>
   <th style="text-align:right;"> three_percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MA </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.84 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.86 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 0.89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.81 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:right;"> 0.81 </td>
   <td style="text-align:right;"> 0.81 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.81 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.59 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 1.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 0.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.59 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.54 </td>
  </tr>
</tbody>
</table>


```r
win_rate_calls <- dplyr::filter(all_results, metric == "Win Rate", 
                                direction == "up") %>%
  dplyr::select(-c(direction, metric)) %>%
  dplyr::arrange(desc(all_days))
knitr::kable(win_rate_calls, digits = 2, format = "html", 
             caption = "Win rates for selling calls into up moves")
```

<table>
<caption>Table 2 Win rates for selling calls into up moves</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:right;"> all_days </th>
   <th style="text-align:right;"> one_day </th>
   <th style="text-align:right;"> two_days </th>
   <th style="text-align:right;"> three_days </th>
   <th style="text-align:right;"> one_percent </th>
   <th style="text-align:right;"> two_percent </th>
   <th style="text-align:right;"> three_percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:right;"> 0.86 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.42 </td>
   <td style="text-align:right;"> 0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.77 </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.68 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.74 </td>
   <td style="text-align:right;"> 0.66 </td>
   <td style="text-align:right;"> 0.55 </td>
   <td style="text-align:right;"> 0.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.55 </td>
   <td style="text-align:right;"> 0.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.42 </td>
   <td style="text-align:right;"> 0.41 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MA </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.62 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.60 </td>
   <td style="text-align:right;"> 0.59 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> 0.32 </td>
   <td style="text-align:right;"> 0.31 </td>
  </tr>
</tbody>
</table>


```r
mean_pl_puts <- dplyr::filter(all_results, metric == "Avg. P/L", 
                               direction == "down") %>%
  dplyr::select(-c(direction, metric)) %>%
  dplyr::arrange(desc(all_days))
knitr::kable(mean_pl_puts, digits = 2, format = "html", 
             caption = "Mean P/L for selling puts into down moves")
```

<table>
<caption>Table 3 Mean P/L for selling puts into down moves</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:right;"> all_days </th>
   <th style="text-align:right;"> one_day </th>
   <th style="text-align:right;"> two_days </th>
   <th style="text-align:right;"> three_days </th>
   <th style="text-align:right;"> one_percent </th>
   <th style="text-align:right;"> two_percent </th>
   <th style="text-align:right;"> three_percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> MA </td>
   <td style="text-align:right;"> 3.19 </td>
   <td style="text-align:right;"> 3.19 </td>
   <td style="text-align:right;"> 3.36 </td>
   <td style="text-align:right;"> 3.48 </td>
   <td style="text-align:right;"> 3.55 </td>
   <td style="text-align:right;"> 3.40 </td>
   <td style="text-align:right;"> 2.81 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:right;"> 1.42 </td>
   <td style="text-align:right;"> 1.43 </td>
   <td style="text-align:right;"> 1.50 </td>
   <td style="text-align:right;"> 1.55 </td>
   <td style="text-align:right;"> 1.42 </td>
   <td style="text-align:right;"> 1.84 </td>
   <td style="text-align:right;"> 1.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:right;"> 1.34 </td>
   <td style="text-align:right;"> 1.32 </td>
   <td style="text-align:right;"> 1.36 </td>
   <td style="text-align:right;"> 1.35 </td>
   <td style="text-align:right;"> 0.84 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:right;"> 1.13 </td>
   <td style="text-align:right;"> 1.18 </td>
   <td style="text-align:right;"> 1.27 </td>
   <td style="text-align:right;"> 1.32 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 1.46 </td>
   <td style="text-align:right;"> 1.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:right;"> 0.97 </td>
   <td style="text-align:right;"> 0.97 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.64 </td>
   <td style="text-align:right;"> 0.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:right;"> 0.79 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.83 </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> -0.22 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> -1.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.47 </td>
   <td style="text-align:right;"> -0.74 </td>
   <td style="text-align:right;"> -1.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> -0.43 </td>
   <td style="text-align:right;"> -0.72 </td>
   <td style="text-align:right;"> -0.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> -0.16 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.16 </td>
   <td style="text-align:right;"> -0.31 </td>
   <td style="text-align:right;"> -0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> -0.53 </td>
   <td style="text-align:right;"> -0.58 </td>
   <td style="text-align:right;"> 0.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:right;"> -0.04 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;"> -0.04 </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;"> -0.36 </td>
   <td style="text-align:right;"> -0.53 </td>
   <td style="text-align:right;"> -0.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> -0.04 </td>
   <td style="text-align:right;"> -0.03 </td>
   <td style="text-align:right;"> -0.03 </td>
   <td style="text-align:right;"> -0.24 </td>
   <td style="text-align:right;"> -0.29 </td>
   <td style="text-align:right;"> -0.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> -0.08 </td>
   <td style="text-align:right;"> -0.11 </td>
   <td style="text-align:right;"> -0.12 </td>
   <td style="text-align:right;"> -0.69 </td>
   <td style="text-align:right;"> -0.64 </td>
   <td style="text-align:right;"> -0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:right;"> -0.11 </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> -0.90 </td>
   <td style="text-align:right;"> -1.58 </td>
   <td style="text-align:right;"> -2.18 </td>
  </tr>
</tbody>
</table>


```r
mean_pl_calls <- dplyr::filter(all_results, metric == "Avg. P/L", 
                                direction == "up") %>%
  dplyr::select(-c(direction, metric)) %>%
  dplyr::arrange(desc(all_days))
knitr::kable(mean_pl_calls, digits = 2, format = "html", 
             caption = "Mean P/L for selling calls into up moves")
```

<table>
<caption>Table 4 Mean P/L for selling calls into up moves</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:right;"> all_days </th>
   <th style="text-align:right;"> one_day </th>
   <th style="text-align:right;"> two_days </th>
   <th style="text-align:right;"> three_days </th>
   <th style="text-align:right;"> one_percent </th>
   <th style="text-align:right;"> two_percent </th>
   <th style="text-align:right;"> three_percent </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> -0.49 </td>
   <td style="text-align:right;"> -0.26 </td>
   <td style="text-align:right;"> -0.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 0.58 </td>
   <td style="text-align:right;"> -0.80 </td>
   <td style="text-align:right;"> -1.53 </td>
   <td style="text-align:right;"> -2.71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> -0.18 </td>
   <td style="text-align:right;"> -0.62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> -0.31 </td>
   <td style="text-align:right;"> -0.80 </td>
   <td style="text-align:right;"> -0.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> -0.24 </td>
   <td style="text-align:right;"> -0.43 </td>
   <td style="text-align:right;"> -0.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.08 </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> -0.13 </td>
   <td style="text-align:right;"> -0.28 </td>
   <td style="text-align:right;"> -0.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.51 </td>
   <td style="text-align:right;"> -0.55 </td>
   <td style="text-align:right;"> -0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:right;"> -0.02 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> -0.57 </td>
   <td style="text-align:right;"> -0.70 </td>
   <td style="text-align:right;"> -0.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> -0.03 </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> -0.24 </td>
   <td style="text-align:right;"> -0.47 </td>
   <td style="text-align:right;"> -0.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:right;"> -0.09 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> -0.96 </td>
   <td style="text-align:right;"> -0.90 </td>
   <td style="text-align:right;"> -0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:right;"> -0.15 </td>
   <td style="text-align:right;"> -0.05 </td>
   <td style="text-align:right;"> -0.06 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> -0.54 </td>
   <td style="text-align:right;"> -1.08 </td>
   <td style="text-align:right;"> -1.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:right;"> -0.50 </td>
   <td style="text-align:right;"> -0.48 </td>
   <td style="text-align:right;"> -0.47 </td>
   <td style="text-align:right;"> -0.48 </td>
   <td style="text-align:right;"> -1.36 </td>
   <td style="text-align:right;"> -1.84 </td>
   <td style="text-align:right;"> -2.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:right;"> -0.57 </td>
   <td style="text-align:right;"> -0.42 </td>
   <td style="text-align:right;"> -0.34 </td>
   <td style="text-align:right;"> -0.28 </td>
   <td style="text-align:right;"> -1.45 </td>
   <td style="text-align:right;"> -3.39 </td>
   <td style="text-align:right;"> -6.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:right;"> -0.77 </td>
   <td style="text-align:right;"> -0.71 </td>
   <td style="text-align:right;"> -0.75 </td>
   <td style="text-align:right;"> -0.69 </td>
   <td style="text-align:right;"> -1.92 </td>
   <td style="text-align:right;"> -2.87 </td>
   <td style="text-align:right;"> -4.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MA </td>
   <td style="text-align:right;"> -3.32 </td>
   <td style="text-align:right;"> -3.26 </td>
   <td style="text-align:right;"> -3.31 </td>
   <td style="text-align:right;"> -2.94 </td>
   <td style="text-align:right;"> -5.94 </td>
   <td style="text-align:right;"> -7.68 </td>
   <td style="text-align:right;"> -9.40 </td>
  </tr>
</tbody>
</table>




