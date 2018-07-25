---
title: Short Put Enhancement
author: Jason Taylor
date: '2018-07-13'
slug: short-put-enhancement
summary: The effect of adding a short call to an ATM short put. How did this perform?
draft: TRUE
categories:
  - Tastytrade
tags:
  - Short Put
header:
  caption: ''
  image: ''
---

### Market Measures - July 13, 2018

#### Tastytrade Compared:

Allocating 30% short ATM puts, managed at 50%  
Allocating 30% of short 50P/16C strangles, managed at 50%  

#### Tastytrade results:

We find that adding a short call to our portfolio of short puts did not add to or hinder performance, but adding the call did reduce our portfolio volatility by 0.4% per month.  

#### My findings:  

The results table shows that there is little variance of win rate between strangles and puts. A more consistent result is that the mean profit per trade is better with the strangle trades with the only exception being TBT. When combining these metrics this can lead to greater total profits.

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tidyverse", "tastytrade", "here", "DT", "htmlwidgets")
  lapply(library_list, require, character.only = TRUE)})))
stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE", "EEM", 
                "FB", "FXI", "SLV", "EWZ", "FXE", "TBT", "IBM")
tar_dte <- 45
tar_delta_put <- -0.5
tar_delta_call <- .16
```

#### Study function


```r
study <- function(stock, tar_dte, tar_delta_put, tar_delta_call) {
  options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte)) %>%
    dplyr::mutate(mid = (bid + ask) / 2)
  
  monthly <- readRDS(paste0(here::here(), "/data/monthly.RDS"))
  
  options_monthly <- options %>%
    dplyr::filter(quotedate %in% monthly$date)
  
  each_day <- dplyr::distinct(options, quotedate, close)
  
  opened_puts <- tastytrade::open_short(options_monthly, tar_delta_put, "put")
  opened_calls <- tastytrade::open_short(options_monthly, tar_delta_call, "call")
  
  closed_puts <- purrr::pmap_dfr(list(list(options), opened_puts$quotedate,
                                      opened_puts$expiration, opened_puts$strike,
                                      opened_puts$mid, list("put")),
                                 tastytrade::close_short_daily) %>%
    dplyr::rename(put_open_credit = open_credit,
                  put_close_debit = debit,
                  put_profit = profit)
  
  closed_calls <- purrr::pmap_dfr(list(list(options), opened_calls$quotedate,
                                       opened_calls$expiration, 
                                       opened_calls$strike,
                                       opened_calls$mid, list("call")),
                                  tastytrade::close_short_daily) %>%
    dplyr::rename(call_open_credit = open_credit,
                  call_close_debit = debit,
                  call_profit = profit)
  
  strangle_closes <- dplyr::left_join(closed_puts, closed_calls, 
                                      by = c("symbol", "quotedate", 
                                             "expiration", "open_date")) %>%
    dplyr::mutate(strang_open_credit = put_open_credit + call_open_credit,
                  strang_close_debit = put_close_debit + call_close_debit,
                  strang_profit = put_profit + call_profit) %>%
    dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::rename(open_stock_price = close) %>%
    dplyr::mutate(open_margin = open_stock_price * 20,
                  contracts = 300000 / open_margin,
                  tot_profit = strang_profit * contracts * 100) %>%
    dplyr::filter(quotedate == expiration | 
                    (strang_close_debit <= (strang_open_credit / 2))) %>%
    dplyr::group_by(open_date, expiration) %>%
    dplyr::filter(quotedate == min(quotedate)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(open_date) %>%
    dplyr::mutate(cum_profit = cumsum(tot_profit))
  
  put_only_closes <- closed_puts %>%
    dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::rename(open_stock_price = close) %>%
    dplyr::mutate(open_margin = open_stock_price * 20,
                  contracts = 300000 / open_margin,
                  tot_profit = put_profit * contracts * 100) %>%
    dplyr::filter(quotedate == expiration | 
                    (put_close_debit <= (put_open_credit / 2))) %>%
    dplyr::group_by(open_date, expiration) %>%
    dplyr::filter(quotedate == min(quotedate)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(open_date) %>%
    dplyr::mutate(cum_profit = cumsum(tot_profit))
  
  strang_results <- metrics(strangle_closes, "strangle")
  put_results <- metrics(put_only_closes, "puts only")
  
  dplyr::bind_rows(strang_results, put_results)
}
```
#### Calculate metrics 

We are looking for % winners, average profit, and total profit for strangles and put only setups  


```r
metrics <- function(df, type) {
  df %>%
    dplyr::mutate(win = ifelse(tot_profit > 0, 1, 0),
                  total_profit = scales::dollar(sum(tot_profit)),
                  mean_profit = scales::dollar(mean(tot_profit))) %>%
    dplyr::group_by(symbol, mean_profit, total_profit, win) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(win_rate = scales::percent(n / sum(n))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(win == 1) %>%
    dplyr::mutate(study = type) %>%
    dplyr::select(symbol, study, win_rate, mean_profit, total_profit)
}
```
#### Run the study  

```r
results <- purrr::pmap_dfr(list(stock_list, tar_dte, tar_delta_put,
                                tar_delta_call), study) %>%
  dplyr::mutate_if(is.numeric, funs(round(., 2)))
```
#### Results  

```r
 MM71318 <- results %>% 
   DT::datatable(., width = 700)

f <- "../../static/post/short-put-enhancement/MM71318.html"
htmlwidgets::saveWidget(MM71318, file.path(normalizePath(dirname(f)),
                                           basename(f)), selfcontained = TRUE)
```

<iframe seamless src="MM71318.html" width="100%" height="500"></iframe>






