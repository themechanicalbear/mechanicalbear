---
title: ROC Comparisons
author: Jason Taylor
date: '2018-07-19'
slug: roc-comparisons
draft: TRUE
categories:
  - Tastytrade
tags:
  - Strangles
  - Iron Condors
description: ''
topics: []
---

### Market Measures - July 19, 2018

#### Tastytrade Compared:

Iron condors have a higher return on capital(ROC) than a strangle, so does this mean the iron condor will outperform?

#### Study:
SPY 2005-2017, 45 DTE
One contract 16 delta Strangle
One contract 25 delta Iron Condor with $20 wings
All trades managed at 50%  

#### Tastytrade results:  

We find that iron condors do not outperform the strangle even though the iron condor has higher ROC. This is because the strangle requires much more capital than the iron condor, thereby dragging on ROC. However, the strangle has a net higher P/L on average, which is the driver of long term performance. ROC alone should not be used to gauge long term performance because ROC has two independent components which should be analyzed separately: PnL and Buying Power Requirement. High ROC alone does not necessarily dictate higher performance Daily PnL is a better way to gauge long-term performance than simply ROC.

#### My findings:  

I found that iron condors performed better with many of the stocks in terms of ROC, daily PnL, and total profit. The timeframe of my study was different 2012-18, rather than 2005-17. Another note here is that the margin used for each trade is not consistent as the 20 wide spread doesn't really make sense for SLV for example. Standardizing the margin used would be a possible enhancement here although a simple takeaway is that these strategies generally perform well.

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tidyverse", "tastytrade", "here", "DT", "htmlwidgets")
  lapply(library_list, require, character.only = TRUE)})))
stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE", "EEM",
                "FXI", "SLV", "EWZ", "FXE", "TBT", "IBM", "FB")
```

#### Strangle study function


```r
strangle_study <- function(stock, tar_dte, tar_delta_put, tar_delta_call) {
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
  
  dplyr::left_join(closed_puts, closed_calls, 
                   by = c("symbol", "quotedate", 
                          "expiration", "open_date")) %>%
    dplyr::mutate(strang_open_credit = put_open_credit + call_open_credit,
                  strang_close_debit = put_close_debit + call_close_debit,
                  strang_profit = put_profit + call_profit) %>%
    dplyr::right_join(each_day, by = c("open_date" = "quotedate")) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::rename(open_stock_price = close) %>%
    dplyr::mutate(open_margin = open_stock_price * 20,
                  #contracts = 300000 / open_margin,
                  contracts = 1,
                  tot_profit = strang_profit * contracts * 100) %>%
    dplyr::filter(quotedate == expiration | 
                    (strang_close_debit <= (strang_open_credit / 2))) %>%
    dplyr::group_by(open_date, expiration) %>%
    dplyr::filter(quotedate == min(quotedate)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(open_date) %>%
    dplyr::mutate(cum_profit = cumsum(tot_profit)) %>%
    dplyr::mutate(days_held = quotedate - open_date)
}
```

#### Iron Condor study function


```r
iron_condor_study <- function(stock, tar_dte, tar_delta_put, tar_delta_call) {
  options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte)) %>%
    dplyr::mutate(mid = (bid + ask) / 2)
  
  monthly <- readRDS(paste0(here::here(), "/data/monthly.RDS"))
  
  options_monthly <- options %>%
    dplyr::filter(quotedate %in% monthly$date)
  
  short_puts <- tastytrade::open_short(options_monthly, tar_delta_put, "put")
  short_calls <- tastytrade::open_short(options_monthly, tar_delta_call, "call")
  
  open_long_put <- function(qdt, exp, typ, s_strike) {
    options_monthly %>%
      dplyr::filter(quotedate == qdt,
                    expiration == exp,
                    type == typ,
                    strike < s_strike) %>%
      dplyr::mutate(diff_strike = s_strike - strike,
                    max_strike_diff = max(diff_strike)) %>%
      
      dplyr::filter(diff_strike >= 20 | diff_strike == max_strike_diff) %>%
      dplyr::filter(diff_strike == min(diff_strike)) %>%
      dplyr::select(quotedate, expiration, strike, delta, dte, mid)
  }
  
  open_long_call <- function(qdt, exp, typ, s_strike) {
    options_monthly %>%
      dplyr::filter(quotedate == qdt,
                    expiration == exp,
                    type == typ,
                    strike > s_strike) %>%
      dplyr::mutate(diff_strike = strike - s_strike,
                    max_strike_diff = max(diff_strike)) %>%
      
      dplyr::filter(diff_strike >= 20 | diff_strike == max_strike_diff) %>%
      dplyr::filter(diff_strike == min(diff_strike)) %>%
      dplyr::select(quotedate, expiration, strike, delta, dte, mid)
  }
  
  long_puts <- purrr::pmap_dfr(list(short_puts$quotedate,
                                    short_puts$expiration,
                                    short_puts$type,
                                    short_puts$strike),
                               open_long_put)
  
  long_calls <- purrr::pmap_dfr(list(short_calls$quotedate,
                                     short_calls$expiration,
                                     short_calls$type,
                                     short_calls$strike),
                                open_long_call)
  
  put_side <- dplyr::left_join(short_puts, long_puts, 
                               by = c("quotedate", "expiration", "dte")) %>%
    dplyr::rename(sp_strike = strike.x,
                  lp_strike = strike.y,
                  sp_delta = delta.x,
                  lp_delta = delta.y,
                  sp_credit = mid.x,
                  lp_debit = mid.y)
  
  call_side <- dplyr::left_join(short_calls, long_calls, 
                                by = c("quotedate", "expiration", "dte")) %>%
    dplyr::rename(sc_strike = strike.x,
                  lc_strike = strike.y,
                  sc_delta = delta.x,
                  lc_delta = delta.y,
                  sc_credit = mid.x,
                  lc_debit = mid.y)
  
  iron_condors <- dplyr::left_join(put_side, call_side,
                                   by = c("quotedate", "expiration", "dte")) %>%
    dplyr::mutate(credit = sp_credit + sc_credit + lp_debit + lc_debit)
  
  
  # Close iron condors at expiration or 50% profit
  close_ic <- function(qdt, exp, sps, lps, scs, lcs, crdt) {
    options %>%
      dplyr::select(symbol,quotedate, type, expiration, strike, mid) %>%
      dplyr::filter(quotedate > qdt,
                    expiration == exp) %>%
      dplyr::filter((type == "put" & strike == sps) |
                      (type == "put" & strike == lps) |
                      (type == "call" & strike == scs) |
                      (type == "call" & (strike == lcs))) %>%
      dplyr::mutate(mid = ifelse(type == "put" & strike == lps, -mid,
                                 ifelse(type == "call" & strike == lcs, -mid, mid)),
                    open_date = as.Date(qdt, origin = "1970-01-01")) %>%
      dplyr::group_by(quotedate, expiration) %>%
      dplyr::mutate(debit = sum(mid)) %>%
      dplyr::ungroup() %>%
      #dplyr::filter(quotedate == expiration | debit <= (crdt / 2)) %>%
      dplyr::filter(quotedate == max(quotedate) | debit <= (crdt / 2)) %>%
      dplyr::filter(quotedate == min(quotedate)) %>%
      dplyr::distinct(symbol, quotedate, open_date, expiration, debit) %>%
      dplyr::rename(close_date = quotedate)
  }
  
  ic_closes <- purrr::pmap_dfr(list(iron_condors$quotedate,
                                    iron_condors$expiration,
                                    iron_condors$sp_strike,
                                    iron_condors$lp_strike,
                                    iron_condors$sc_strike,
                                    iron_condors$lc_strike,
                                    iron_condors$credit),
                               close_ic)
  
  dplyr::left_join(iron_condors, ic_closes, by = c("quotedate" = "open_date",
                                                   "expiration")) %>%
    dplyr::mutate(profit = (credit - debit) * 100) %>%
    dplyr::arrange(quotedate) %>%
    dplyr::mutate(cum_profit = cumsum(profit)) %>%
    dplyr::mutate(put_wide = sp_strike - lp_strike,
                  call_wide = lc_strike - sc_strike) %>%
    dplyr::mutate(wide = ifelse(put_wide > call_wide, put_wide, call_wide)) %>%
    dplyr::mutate(margin = (wide - credit) * 100) %>%
    dplyr::mutate(days_held = close_date - quotedate) %>%
    dplyr::filter(complete.cases(.))
}
```

#### Run both studies for all stocks


```r
ic_results <- purrr::pmap_dfr(list(stock_list, tar_dte = 45, 
                                   tar_delta_put = -0.25, tar_delta_call = 0.25),
                              iron_condor_study)
st_results <- purrr::pmap_dfr(list(stock_list, tar_dte = 45,
                                   tar_delta_put = -0.16, tar_delta_call = 0.16),
                              strangle_study)
```

#### Metrics  


```r
metrics_st <- st_results %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(roc = tot_profit / open_margin,
                st_roc = scales::percent(mean(roc)),
                st_margin = scales::dollar(mean(open_margin)),
                st_profit = round(sum(tot_profit), digits = 2),
                total_days_held = sum(as.numeric(days_held)),
                st_daily = scales::dollar(st_profit / total_days_held)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(symbol, st_roc, st_daily, st_margin, st_profit) 

metrics_ic <- ic_results %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(roc = profit / margin,
                ic_roc = scales::percent(mean(roc)),
                ic_margin = scales::dollar(mean(margin)),
                ic_profit = round(sum(profit), digits = 2), 
                total_days_held = sum(as.numeric(days_held)),
                ic_daily = scales::dollar(ic_profit / total_days_held)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(symbol, ic_roc, ic_daily, ic_margin, ic_profit)

results <- dplyr::left_join(metrics_st, metrics_ic, by = "symbol") %>%
  dplyr::select(symbol, st_roc, ic_roc, st_daily, ic_daily,
                st_margin, ic_margin, st_profit, ic_profit) %>%
  dplyr::arrange(symbol)
```

#### Results 


```r
 MM71918 <- results %>% 
   DT::datatable(., width = 1200)

f <- "../../static/post/roc-comparisons/MM71918.html"
htmlwidgets::saveWidget(MM71918, file.path(normalizePath(dirname(f)),
                                           basename(f)), selfcontained = TRUE)
```

<iframe seamless src="MM71918.html" width="100%" height="1000"></iframe>
