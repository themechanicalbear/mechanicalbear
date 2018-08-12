---
title: Put Richness
author: Jason Taylor
date: '2018-08-07'
draft: TRUE
slug: put-richness
categories:
  - Tastytrade
tags:
  - Strangles
description: ''
topics: []
---

### Market Measures - August 07, 2018

On average, puts trade at a higher premium to calls at the same delta. Is a higher put/call price ratio indicative of better POP or average P/L?  
<!--more-->
#### Study:  
* SPY, 2005-2017, 45 DTE
* Sold 16 delta strangles
  + Compared:  
    - Selling strangles when the price ratio was above average (> 1.8)
    - Selling strangles when the price ratio was below average (< 1.8)
    - All trades held to expiration

#### Tastytrade results:  
A higher put/call price ratio leads to double the P/L in the long run while containing half the risk.

This ratio is mostly a measure of IV at the 1SD level. Using this ratio in conjunction with IVR, i.e., looking for high IVR and a high put/call price ratio would be the ideal scenario for selling strangles.  

#### Adjustments to the study:  
* Including stocks in addition to SPY, (IWM, GLD, QQQ, DIA, TLT, XLE, EEM, EWZ, FXE) 
* The date range for my dataset is Jan 2012 - Mar 2018
* Modifying the put-call price ratio from the dataset average (1.8) in the example study to the running one year mean. The mean of the entire dataset is unknown at the time of trade entry and can not be used as criteria.
* Calculate the ratio normalizing by delta so that although 18 delta is the closest to 16 the price is not a direct comparison to the 16 delta on the other side. 
  + As an example call price of $.10 for 10 delta and put price of $.40 for 16 delta has a p/c ratio of 2.5 rather than  4
  + This estimate helps to normalize by delta but does not take into account how volatility changes as we move away from ATM, but this should get us close
* Including a standard deviation of the ratio as an additional entry condition input  
* Split results by above running mean as well as above the running upper bound of the sd band

#### My Results:  
When choosing entry criteria of 45 DTE, 16 delta strangles, and closing at expiration waiting to enter until the pc ratio exceeds the trailing one yr mean or upper bound of SD did not have consistent results. SPY did show signs of benefiting from this strategy, but FXE, for example, did not. Depending on your objectives, if you are trading SPY only or want something consistent across more underlyings, these results may be useful or not in your trading. Either way, this exercise expanded the development to a point where entering and exiting strangles and calculating metrics has become easier. This result will allow me to continue writing posts and building this toolset into a robust backtesting system. Please read on for code snippets and summary plots.

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tidyverse", "tastytrade", "here", "htmlwidgets")
  lapply(library_list, require, character.only = TRUE)})))
args <- expand.grid(symbol = c("SPY", "IWM", "GLD", "QQQ", "DIA",
                               "TLT","XLE", "EEM", "EWZ", "FXE"),
                    tar_dte = 45,
                    tar_delta_put = -0.16,
                    tar_delta_call = 0.16,
                    stringsAsFactors = FALSE)
```

#### Summary of study function

* Our function will connect us to the dataset in redshift
* Open short puts and calls every day given the target DTE, and Deltas
* Collect a subset of options data that could be possible expiration exits
* Close all trades at expiration and join together results


```r
study <- function(stock, tar_dte, tar_delta_put, tar_delta_call) {
  rs_conn <- tastytrade::redshift_connect("TASTYTRADE")
  options <- rs_conn %>%
    dplyr::tbl(stock) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte))
  
  opened_puts <- tastytrade::open_short(options, tar_delta_put, "put")
  opened_calls <- tastytrade::open_short(options, tar_delta_call, "call")
  
  sub_options <- options %>%
    dplyr::filter(quotedate == expiration,
                  strike %in% 
                    c(opened_puts$put_strike, opened_calls$call_strike)) %>%
    dplyr::collect() %>%
    dplyr::mutate(quotedate = as.Date(quotedate, format = "%Y-%m-%d"),
                  expiration = as.Date(expiration, format = "%Y-%m-%d"))
  
  closed_puts <- 
    purrr::pmap_dfr(list(list(sub_options), opened_puts$quotedate,
                         opened_puts$expiration, list("put"), 
                         opened_puts$put_strike, 
                         opened_puts$put_open_credit,
                         opened_puts$delta_strike,
                         opened_puts$close_price),
                    tastytrade::close_short_exp)
  
  closed_calls <- 
    purrr::pmap_dfr(list(list(sub_options), opened_calls$quotedate,
                         opened_calls$expiration, list("call"), 
                         opened_calls$call_strike, 
                         opened_calls$call_open_credit,
                         opened_calls$delta_strike,
                         opened_calls$close_price),
                    tastytrade::close_short_exp)
  
  dplyr::left_join(closed_puts, closed_calls, 
                   by = c("symbol", "quotedate", "expiration", "open_date",
                          "open_stock_price")) 
}
```

#### Run study  

Using the purrr library to map the arguments list to our study function results in a dataframe of strangles to review


```r
results <- purrr::pmap_dfr(list(args$symbol, args$tar_dte, args$tar_delta_put,
                                args$tar_delta_call), study) 

dplyr::glimpse(results)
```

```
## Observations: 15,173
## Variables: 13
## $ symbol           <chr> "SPY", "SPY", "SPY", "SPY", "SPY", "SPY", "SP...
## $ quotedate        <date> 2012-02-17, 2012-02-17, 2012-02-17, 2012-02-...
## $ expiration       <date> 2012-02-17, 2012-02-17, 2012-02-17, 2012-02-...
## $ open_date        <date> 2012-01-03, 2012-01-04, 2012-01-05, 2012-01-...
## $ open_stock_price <dbl> 127.495, 127.700, 128.000, 128.020, 129.130, ...
## $ put_delta        <dbl> -0.1565, -0.1631, -0.1651, -0.1528, -0.1657, ...
## $ put_credit       <dbl> 1.070, 1.065, 1.020, 0.875, 0.920, 0.890, 0.8...
## $ put_debit        <dbl> 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.0...
## $ put_profit       <dbl> 1.065, 1.060, 1.015, 0.870, 0.915, 0.885, 0.8...
## $ call_delta       <dbl> 0.1470, 0.1746, 0.1759, 0.1527, 0.1504, 0.156...
## $ call_credit      <dbl> 0.555, 0.660, 0.640, 0.490, 0.470, 0.495, 0.5...
## $ call_debit       <dbl> 0.455, 1.475, 1.475, 1.475, 0.455, 0.455, 0.4...
## $ call_profit      <dbl> 0.09999999, -0.81500000, -0.83500004, -0.9850...
```

#### Calculate metrics  

Using the results at expiration from the strangle trades we now can calculate additional metrics to be reviewed.

* Put/Call ratio based on delta
* Profit
* Return on Capital
* Margin at the open based on 20% of the stock price to approximate
  + *this is an area to explore for a new function in the tastytrade package*
* 1 Year running average of the Mean and Standard Deviation of pc ratio
* Percentage win rates for each split


```r
result_metrics <- results %>%
  dplyr::filter(put_credit > 0, call_credit > 0) %>%
  dplyr::mutate(margin = 20 * open_stock_price,
                pc_ratio = abs((put_credit / put_delta) / 
                                 (call_credit / call_delta)),
                profit = (put_profit + call_profit) * 100,
                roc = profit / margin,
                profitable = ifelse(profit > 0, 1, 0)) %>%
  dplyr::group_by(symbol) %>%
  dplyr::arrange(open_date) %>%
  dplyr::mutate(symbol_count = n(),
                mean_profit_all = mean(profit),
                mean_roc_all = mean(roc),
                win_rate_all = sum(profitable) / symbol_count,
                sd_profit_all = sd(profit),
                run_sd = TTR::runSD(pc_ratio, n = 252),
                run_mean = TTR::runMean(pc_ratio, n = 252),
                lower = run_mean - run_sd,
                upper = run_mean + run_sd,
                above_upper = ifelse(pc_ratio > upper, 1, 0),
                above_run_mean = ifelse(pc_ratio > run_mean, 1, 0)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(symbol, above_upper, above_run_mean) %>%
  dplyr::mutate(mean_profit = mean(profit),
                mean_roc = mean(roc),
                count = n(),
                sd_profit = sd(profit),
                win_rate = sum(profitable) / count) %>%
  dplyr::ungroup() %>%
  dplyr::select(symbol, open_date, pc_ratio, profitable, lower,
                upper, above_upper, mean_profit_all, mean_profit, win_rate,
                win_rate_all, above_run_mean, sd_profit, sd_profit_all, run_sd,
                run_mean, mean_roc, mean_roc_all)

dplyr::glimpse(result_metrics)
```

```
## Observations: 14,838
## Variables: 18
## $ symbol          <chr> "SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE...
## $ open_date       <date> 2012-01-03, 2012-01-03, 2012-01-03, 2012-01-0...
## $ pc_ratio        <dbl> 1.810897, 1.773478, 1.143696, 1.834532, 1.9315...
## $ profitable      <dbl> 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1...
## $ lower           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ upper           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ above_upper     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ mean_profit_all <dbl> 70.638907, 55.717083, 32.953249, 24.935122, 28...
## $ mean_profit     <dbl> 92.665339, 55.575697, 116.930279, -3.916335, 8...
## $ win_rate        <dbl> 0.8486056, 0.8047809, 0.9003984, 0.6414343, 0....
## $ win_rate_all    <dbl> 0.8001332, 0.8372721, 0.8136605, 0.7459239, 0....
## $ above_run_mean  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ sd_profit       <dbl> 97.66448, 93.93827, 158.40609, 104.01994, 78.0...
## $ sd_profit_all   <dbl> 179.16656, 169.40416, 237.67250, 129.24087, 21...
## $ run_sd          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ run_mean        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ mean_roc        <dbl> 0.033499808, 0.034810827, 0.035774345, -0.0037...
## $ mean_roc_all    <dbl> 0.019152915, 0.024684420, 0.012831337, 0.01058...
```

#### Plot the standard deviation of pc ratio bands  

Using the upper band of the standard deviation for pc ratio as entry criteria gives a higher entry requirement and is more selective. This plot shows that band for the SPY, and later we will see how trading only those entries performs against all days above the mean value. At this point, we have quite a few rows with NA values due to the trailing one yr mean and sd. These will subsequently be removed because trades would not have been opened without this data. This ribbon plot shows this missing data in the first year on the left.


```r
spy <- dplyr::filter(result_metrics, symbol == "SPY")
ggplot(data = spy, aes(x = open_date, y = pc_ratio)) +
  geom_point(size = 0.35, aes(colour = factor(profitable))) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = open_date, 
                  fill = "sd band"), alpha = 0.2) +
  scale_fill_manual("",values = "grey12") + 
  theme_minimal() +
  ggtitle("1 Yr Standard Deviation PC Ratio Ribbon")
```

<img src="/post/2018-08-07-put-richness_files/figure-html/sd ribbon plot-1.png" width="672" />

#### Metrics calculations  

Using the results to build a table showing distinct values to compare the splits.


```r
metrics <- result_metrics %>%
  dplyr::select(symbol, above_upper, above_run_mean,
                mean_profit_all, mean_profit, win_rate, win_rate_all,
                sd_profit, sd_profit_all, mean_roc, mean_roc_all) %>%
  dplyr::distinct() %>%
  tidyr::replace_na(list(above_upper = 2, above_run_mean = 2)) %>%
  dplyr::mutate(mean_profit_above_upper = 
                  ifelse(above_upper == 1, mean_profit, NA),
                mean_profit_above_run_mean = 
                  ifelse(above_upper == 0 & above_run_mean == 1, mean_profit, NA),
                mean_roc_above_upper = 
                  ifelse(above_upper == 1, mean_roc, NA),
                mean_roc_above_run_mean = 
                  ifelse(above_upper == 0 & above_run_mean == 1, mean_roc, NA),
                win_rate_above_upper = 
                  ifelse(above_upper == 1, win_rate, NA),
                win_rate_above_run_mean = 
                  ifelse(above_upper == 0 & above_run_mean == 1, win_rate, NA),
                sd_profit_above_upper = 
                  ifelse(above_upper == 1, sd_profit, NA),
                sd_profit_above_run_mean = 
                  ifelse(above_upper == 0 & above_run_mean == 1, sd_profit, NA)) %>%
  dplyr::select(symbol, 
                mean_profit_all, mean_profit_above_upper, 
                mean_profit_above_run_mean,
                mean_roc_all, mean_roc_above_upper, mean_roc_above_run_mean, 
                win_rate_all, win_rate_above_upper, 
                win_rate_above_run_mean,
                sd_profit_all, sd_profit_above_upper, 
                sd_profit_above_run_mean) %>%
  dplyr::group_by(symbol) %>% 
  dplyr::summarise_all(funs(mean(., na.rm = TRUE)))

formatted_metrics <- metrics %>%
  dplyr::mutate(mean_profit_all = scales::dollar(mean_profit_all), 
                mean_profit_above_upper = scales::dollar(mean_profit_above_upper), 
                mean_profit_above_run_mean = scales::dollar(mean_profit_above_run_mean),
                mean_roc_all = scales::percent(mean_roc_all), 
                mean_roc_above_upper = scales::percent(mean_roc_above_upper), 
                mean_roc_above_run_mean = scales::percent(mean_roc_above_run_mean),
                win_rate_all = scales::percent(win_rate_all), 
                win_rate_above_upper = scales::percent(win_rate_above_upper), 
                win_rate_above_run_mean = scales::percent(win_rate_above_run_mean),
                sd_profit_all = scales::dollar(sd_profit_all), 
                sd_profit_above_upper = scales::dollar(sd_profit_above_upper), 
                sd_profit_above_run_mean = scales::dollar(sd_profit_above_run_mean))
```

#### Mean PnL 


```r
dplyr::select(formatted_metrics, 
              c(symbol, mean_profit_all, mean_profit_above_upper, 
                mean_profit_above_run_mean)) %>%
  knitr::kable(., digits = 2, format = "html", 
               caption = "Mean PnL for selling strangles based on Put-Call Ratio",
               col.names = c("SYM", "ALL", "ABOVE UPPER", "ABOVE RUN MEAN"),
               escape = FALSE, 
               align = c('l', 'r', 'r', 'r')) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  kableExtra::add_header_above(., c(" ", "MEAN PnL" = 3)) %>%
  kableExtra::column_spec(., 1:4, width = "0.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Mean PnL for selling strangles based on Put-Call Ratio</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">MEAN PnL</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> ALL </th>
   <th style="text-align:right;"> ABOVE UPPER </th>
   <th style="text-align:right;"> ABOVE RUN MEAN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 0.5in; "> DIA </td>
   <td style="text-align:right;width: 0.5in; "> $28.02 </td>
   <td style="text-align:right;width: 0.5in; "> $49.93 </td>
   <td style="text-align:right;width: 0.5in; "> $30.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EEM </td>
   <td style="text-align:right;width: 0.5in; "> $21.64 </td>
   <td style="text-align:right;width: 0.5in; "> $31.07 </td>
   <td style="text-align:right;width: 0.5in; "> $18.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EWZ </td>
   <td style="text-align:right;width: 0.5in; "> $-2.07 </td>
   <td style="text-align:right;width: 0.5in; "> $7.29 </td>
   <td style="text-align:right;width: 0.5in; "> $-9.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> FXE </td>
   <td style="text-align:right;width: 0.5in; "> $21.06 </td>
   <td style="text-align:right;width: 0.5in; "> $-2.44 </td>
   <td style="text-align:right;width: 0.5in; "> $23.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> GLD </td>
   <td style="text-align:right;width: 0.5in; "> $32.95 </td>
   <td style="text-align:right;width: 0.5in; "> $-41.70 </td>
   <td style="text-align:right;width: 0.5in; "> $35.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> IWM </td>
   <td style="text-align:right;width: 0.5in; "> $55.72 </td>
   <td style="text-align:right;width: 0.5in; "> $63.18 </td>
   <td style="text-align:right;width: 0.5in; "> $52.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> QQQ </td>
   <td style="text-align:right;width: 0.5in; "> $24.94 </td>
   <td style="text-align:right;width: 0.5in; "> $57.02 </td>
   <td style="text-align:right;width: 0.5in; "> $36.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> SPY </td>
   <td style="text-align:right;width: 0.5in; "> $70.64 </td>
   <td style="text-align:right;width: 0.5in; "> $82.51 </td>
   <td style="text-align:right;width: 0.5in; "> $55.26 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> TLT </td>
   <td style="text-align:right;width: 0.5in; "> $22.20 </td>
   <td style="text-align:right;width: 0.5in; "> $-5.61 </td>
   <td style="text-align:right;width: 0.5in; "> $17.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> XLE </td>
   <td style="text-align:right;width: 0.5in; "> $30.17 </td>
   <td style="text-align:right;width: 0.5in; "> $40.44 </td>
   <td style="text-align:right;width: 0.5in; "> $28.89 </td>
  </tr>
</tbody>
</table>

#### Win Rates 


```r
dplyr::select(formatted_metrics, c(symbol, win_rate_all, win_rate_above_upper,
                                   win_rate_above_run_mean)) %>%
  knitr::kable(., digits = 2, format = "html", 
               caption = "Win Rates for selling strangles based on Put-Call Ratio",
               col.names = c("SYM", "ALL", "ABOVE UPPER", "ABOVE RUN MEAN"),
               escape = FALSE, 
               align = c('l', 'r', 'r', 'r')) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  kableExtra::add_header_above(., c(" ", "Win Rate" = 3)) %>%
  kableExtra::column_spec(., 1:4, width = "0.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2 Win Rates for selling strangles based on Put-Call Ratio</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Win Rate</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> ALL </th>
   <th style="text-align:right;"> ABOVE UPPER </th>
   <th style="text-align:right;"> ABOVE RUN MEAN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 0.5in; "> DIA </td>
   <td style="text-align:right;width: 0.5in; "> 77.3% </td>
   <td style="text-align:right;width: 0.5in; "> 84.1% </td>
   <td style="text-align:right;width: 0.5in; "> 77.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EEM </td>
   <td style="text-align:right;width: 0.5in; "> 84.8% </td>
   <td style="text-align:right;width: 0.5in; "> 90.1% </td>
   <td style="text-align:right;width: 0.5in; "> 83.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EWZ </td>
   <td style="text-align:right;width: 0.5in; "> 74.7% </td>
   <td style="text-align:right;width: 0.5in; "> 73.8% </td>
   <td style="text-align:right;width: 0.5in; "> 70.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> FXE </td>
   <td style="text-align:right;width: 0.5in; "> 84.0% </td>
   <td style="text-align:right;width: 0.5in; "> 69.4% </td>
   <td style="text-align:right;width: 0.5in; "> 87.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> GLD </td>
   <td style="text-align:right;width: 0.5in; "> 81.4% </td>
   <td style="text-align:right;width: 0.5in; "> 75.1% </td>
   <td style="text-align:right;width: 0.5in; "> 82.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> IWM </td>
   <td style="text-align:right;width: 0.5in; "> 83.7% </td>
   <td style="text-align:right;width: 0.5in; "> 84.7% </td>
   <td style="text-align:right;width: 0.5in; "> 82.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> QQQ </td>
   <td style="text-align:right;width: 0.5in; "> 74.6% </td>
   <td style="text-align:right;width: 0.5in; "> 88.7% </td>
   <td style="text-align:right;width: 0.5in; "> 77.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> SPY </td>
   <td style="text-align:right;width: 0.5in; "> 80.0% </td>
   <td style="text-align:right;width: 0.5in; "> 81.9% </td>
   <td style="text-align:right;width: 0.5in; "> 77.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> TLT </td>
   <td style="text-align:right;width: 0.5in; "> 81.8% </td>
   <td style="text-align:right;width: 0.5in; "> 83.1% </td>
   <td style="text-align:right;width: 0.5in; "> 84.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> XLE </td>
   <td style="text-align:right;width: 0.5in; "> 82.5% </td>
   <td style="text-align:right;width: 0.5in; "> 83.2% </td>
   <td style="text-align:right;width: 0.5in; "> 82.3% </td>
  </tr>
</tbody>
</table>

#### Standard Deviation PnL 


```r
dplyr::select(formatted_metrics, 
              c(symbol, sd_profit_all, sd_profit_above_upper,
                sd_profit_above_run_mean)) %>%
  knitr::kable(., digits = 2, format = "html", 
               caption = "Standard Deviation PnL for selling strangles based on Put-Call Ratio",
               col.names = c("SYM", "ALL", "ABOVE UPPER", "ABOVE RUN MEAN"),
               escape = FALSE, 
               align = c('l', 'r', 'r', 'r')) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  kableExtra::add_header_above(., c(" ", "SD PnL" = 3)) %>%
  kableExtra::column_spec(., 1:4, width = "0.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 3 Standard Deviation PnL for selling strangles based on Put-Call Ratio</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">SD PnL</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> ALL </th>
   <th style="text-align:right;"> ABOVE UPPER </th>
   <th style="text-align:right;"> ABOVE RUN MEAN </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 0.5in; "> DIA </td>
   <td style="text-align:right;width: 0.5in; "> $213.17 </td>
   <td style="text-align:right;width: 0.5in; "> $187.13 </td>
   <td style="text-align:right;width: 0.5in; "> $221.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EEM </td>
   <td style="text-align:right;width: 0.5in; "> $63.05 </td>
   <td style="text-align:right;width: 0.5in; "> $37.21 </td>
   <td style="text-align:right;width: 0.5in; "> $61.05 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> EWZ </td>
   <td style="text-align:right;width: 0.5in; "> $146.12 </td>
   <td style="text-align:right;width: 0.5in; "> $116.54 </td>
   <td style="text-align:right;width: 0.5in; "> $122.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> FXE </td>
   <td style="text-align:right;width: 0.5in; "> $104.56 </td>
   <td style="text-align:right;width: 0.5in; "> $119.56 </td>
   <td style="text-align:right;width: 0.5in; "> $103.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> GLD </td>
   <td style="text-align:right;width: 0.5in; "> $237.67 </td>
   <td style="text-align:right;width: 0.5in; "> $359.85 </td>
   <td style="text-align:right;width: 0.5in; "> $268.32 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> IWM </td>
   <td style="text-align:right;width: 0.5in; "> $169.40 </td>
   <td style="text-align:right;width: 0.5in; "> $168.14 </td>
   <td style="text-align:right;width: 0.5in; "> $167.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> QQQ </td>
   <td style="text-align:right;width: 0.5in; "> $129.24 </td>
   <td style="text-align:right;width: 0.5in; "> $128.18 </td>
   <td style="text-align:right;width: 0.5in; "> $127.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> SPY </td>
   <td style="text-align:right;width: 0.5in; "> $179.17 </td>
   <td style="text-align:right;width: 0.5in; "> $172.64 </td>
   <td style="text-align:right;width: 0.5in; "> $211.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> TLT </td>
   <td style="text-align:right;width: 0.5in; "> $171.89 </td>
   <td style="text-align:right;width: 0.5in; "> $241.76 </td>
   <td style="text-align:right;width: 0.5in; "> $182.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 0.5in; "> XLE </td>
   <td style="text-align:right;width: 0.5in; "> $131.24 </td>
   <td style="text-align:right;width: 0.5in; "> $105.37 </td>
   <td style="text-align:right;width: 0.5in; "> $147.90 </td>
  </tr>
</tbody>
</table>

#### Mean Profit by split


```r
dplyr::select(metrics, 
              c(symbol, mean_profit_all, mean_profit_above_upper,
                mean_profit_above_run_mean)) %>%
  tidyr::gather(., Metric, Value, -symbol) %>%
  ggplot(., aes(Metric, Value, fill = symbol)) + 
  geom_col(position = "dodge") +
  theme_minimal() +
  ylab("Mean Profit") + 
  xlab("Metric") +
  ggtitle("Mean Profit split by mean, upper band sd, and all trades")
```

<img src="/post/2018-08-07-put-richness_files/figure-html/unnamed-chunk-4-1.png" width="672" />

#### Win Percentages by split


```r
dplyr::select(metrics, 
              c(symbol, win_rate_all, win_rate_above_upper,
                win_rate_above_run_mean)) %>%
  tidyr::gather(., Metric, Value, -symbol) %>%
  dplyr::mutate(Value = Value * 100) %>%
  ggplot(., aes(Metric, Value, fill = symbol)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous("Win Rate", breaks = seq(55, 95, by = 10)) +
  coord_cartesian(ylim = c(55, 95)) +
  theme_minimal() + 
  xlab("Metric")  +
  ggtitle("Win percentages split by mean, upper band sd, and all trades")
```

<img src="/post/2018-08-07-put-richness_files/figure-html/unnamed-chunk-5-1.png" width="672" />

#### Mean Standard Deviation of profit by split


```r
dplyr::select(metrics, 
              c(symbol, sd_profit_all, sd_profit_above_upper,
                sd_profit_above_run_mean)) %>%
  tidyr::gather(., Metric, Value, -symbol) %>%
  ggplot(., aes(Metric, Value, fill = symbol)) + 
  geom_col( position = "dodge") +
  theme_minimal() +
  ylab("Mean SD Profit") + 
  xlab("Metric") +
  ggtitle("SD Mean Profit split by mean, upper band sd, and all trades")
```

<img src="/post/2018-08-07-put-richness_files/figure-html/unnamed-chunk-6-1.png" width="672" />

#### Mean ROC by split


```r
dplyr::select(metrics, 
              c(symbol, mean_roc_all, mean_roc_above_upper,
                mean_roc_above_run_mean)) %>%
  tidyr::gather(., Metric, Value, -symbol) %>%
  ggplot(., aes(Metric, Value, fill = symbol)) + 
  geom_col( position = "dodge") +
  theme_minimal() +
  ylab("Mean ROC") + 
  xlab("Metric") +
  ggtitle("Return on Capital split by mean, upper band sd, and all trades")
```

<img src="/post/2018-08-07-put-richness_files/figure-html/unnamed-chunk-7-1.png" width="672" />
