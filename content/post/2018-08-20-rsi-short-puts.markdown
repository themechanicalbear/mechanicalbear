---
title: RSI Short Puts
author: Jason
date: '2018-08-20'
draft: FALSE
slug: rsi-short-puts
categories:
  - Original
tags:
  - Short Put
  - RSI
  - Original Content
description: ''
topics: []
---

#### Study:

Traders often look for "oversold" stocks to get long. Our trading group recently discussed if using common technical analysis metrics like RSI (Relative Strength Index), could be used to find these opportune entries using options?

<!--more-->

* Calculate the historical closing 14 day RSI
* Identify when the previous day close was below 30 RSI (oversold) and then crossed above
* Open a short put closest to 30 delta and 45 DTE and hold to expiration
* Analyze the results and compare to random entry groups
  + calculate the number of trades entered in the period Jan 2012 - Mar 2018 
    - sample that same number of trades 1000 times
    - compare results of these buckets against RSI entry criteria trades

#### Results:

The broad market (ETFs SPY, IWM, QQQ, and DIA) do seem to show a statistically significant improvement in performance over randomly selecting trades over the same time period. Win percentages, Mean profits, and Total profits all ranking in the upper quartiles of random sampling. 

FXE in contrast resulted in one of the worst groups of trades in the samples taken. 

A few ideas on how to extend this study further:  

* Modify from short puts to short put verticals (possibly remove large losses)
* Implement exits at 50% profit rather than expiration only

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60))
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tidyverse", "tastytrade", "here", "TTR", 
                    "formattable", "kableExtra", "knitr", "scales")
  lapply(library_list, require, character.only = TRUE)})))
args <- expand.grid(symbol = c("SPY", "IWM", "GLD", "QQQ", "DIA",
                               "TLT", "XLE", "EEM", "EWZ", "FXE"),
                    tar_dte = 45,
                    tar_delta_put = -0.30,
                    stringsAsFactors = FALSE)
```

#### Summary of study function:

* Connect to the dataset in redshift
* Open short puts every day given the target DTE and Delta
* Calculate the 14 day RSI and 1 day lag to determine RSI cross days
* Collect a subset of options data that are possible expiration exits
* Close all trades at expiration and join together results


```r
study <- function(stock, tar_dte, tar_delta_put) {
  rs_conn <- tastytrade::redshift_connect("TASTYTRADE")
  options <- rs_conn %>%
    tbl(stock) %>%
    mutate(m_dte = abs(dte - tar_dte))
  
  rsi <- options %>%
    distinct(symbol, quotedate, close_price) %>%
    collect() %>%
    mutate(rsi = RSI(close_price, n = 14), 
           rsi_lag = lag(rsi, 1),
           quotedate = as.Date(quotedate, format = "%Y-%m-%d"))
  
  opened_puts <- tastytrade::open_short(options, tar_delta_put, "put")
  
  sub_options <- options %>%
    filter(quotedate == expiration,
           strike %in% opened_puts$put_strike) %>%
    collect() %>%
    mutate(quotedate = as.Date(quotedate, format = "%Y-%m-%d"),
           expiration = as.Date(expiration, format = "%Y-%m-%d"))
  
  pmap_dfr(list(list(sub_options), opened_puts$quotedate,
                opened_puts$expiration, list("put"),
                opened_puts$put_strike,
                opened_puts$put_open_credit,
                opened_puts$delta_strike,
                opened_puts$close_price),
           tastytrade::close_short_exp) %>%
    left_join(rsi, by = c("symbol", "open_date" = "quotedate", 
                          "open_stock_price" = "close_price"))
}
```

#### Run Study


```r
results <- pmap_dfr(list(args$symbol, args$tar_dte, args$tar_delta_put), study)
```

#### Resample 1000 times  

* Count # of trades for each symbol
* Create arguments list including (symbol, count, 1:1000)
  + SPY had 11 days when the RSI crossed above 30
  + create 1000 random groups with 11 trades in them


```r
set.seed(42)
grp_args <- results %>%
  group_by(symbol) %>%
  filter(rsi > 30, rsi_lag < 30) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  slice(rep(row_number(), 1000)) %>%
  group_by(symbol) %>%
  mutate(run = row_number()) %>%
  ungroup()

result_sample <- function(df, s, n, x) {
  df %>%
    filter(symbol == s, 
           complete.cases(.)) %>%
    sample_n(size = n) %>%
    mutate(run = x)
}

resampled <- pmap_dfr(list(list(results), grp_args$symbol, grp_args$count,
                           grp_args$run), result_sample)
```

#### Calculate metrics for all runs so we can see the distribution of random entry selections  

* Trade Count
* Win Rate
* Mean Credit
* Mean Profit
* Total Profit


```r
metrics <- resampled %>%
  group_by(symbol, run) %>%
  mutate(trade_count = n(),
         profitable = ifelse(put_profit > 0, 1, 0),
         win_rate = sum(profitable) / trade_count,
         mean_credit = mean(put_credit * 100),
         mean_profit = mean(put_profit * 100),
         total_profit = sum(put_profit * 100)) %>%
  distinct(symbol, trade_count, win_rate, mean_credit, 
           mean_profit, total_profit) %>%
  ungroup() %>%
  arrange(desc(win_rate))
```

#### Boxplot of Win Rates for random samples  


```r
metrics %>%
  select(symbol, win_rate) %>%
  gather(., metric, value, -symbol) %>%
  ggplot(., aes(metric, value, fill = symbol)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = "Win Rate") +
  scale_y_continuous(labels = scales::percent) +
  xlab("") +
  ylab("Win Rate %") +
  ggtitle("Win rate distributions for each symbol")
```

<img src="/post/2018-08-20-rsi-short-puts_files/figure-html/unnamed-chunk-3-1.png" width="672" />

We see a win rate percentage that exceeds the expected target of ~70% based on the 30 delta entry on average

#### Boxplot of Mean Profit at expiration for random samples  


```r
metrics %>%
  select(symbol, mean_profit) %>%
  gather(., metric, value, -symbol) %>%
  ggplot(., aes(metric, value, fill = symbol)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = "Mean Profit") +
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Mean Profit") +
  ggtitle("Mean Profit distributions for each symbol")
```

<img src="/post/2018-08-20-rsi-short-puts_files/figure-html/unnamed-chunk-4-1.png" width="672" />

EEM, EWZ, FXE, GLD, TLT, and XLE have distributions of profits at expiration that are not much greater than $0 and may not be showing a strong case for trading the 30 delta short put without additional entry criteria  

DIA, IWM, QQQ, and SPY seem to have a more positive distribution of mean profit with random entries  

#### Boxplot of Total Profit at expiration for random samples  


```r
metrics %>%
  select(symbol, total_profit) %>%
  gather(., metric, value, -symbol) %>%
  ggplot(., aes(metric, value, fill = symbol)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = "Total Profit") +
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Total Profit") +
  ggtitle("Total Profit distributions for each symbol")
```

<img src="/post/2018-08-20-rsi-short-puts_files/figure-html/unnamed-chunk-5-1.png" width="672" />

As expected by the mean profit plot the total profit distributions share the same general outcomes. This gives further evidence that additional criteria my be advantageous or required for this strategy.  

#### Calculate metrics for RSI cross-over entries  

* Trade Count
* Win Rate
* Mean Credit
* Mean Profit
* Total Profit  


```r
rsi_results <- results %>%
  group_by(symbol) %>%
  filter(rsi > 30, rsi_lag < 30) %>%
  ungroup()

rsi_metrics <- rsi_results %>%
  group_by(symbol) %>%
  mutate(trade_count = n(),
         profitable = ifelse(put_profit > 0, 1, 0),
         win_rate = sum(profitable) / trade_count,
         mean_credit = dollar(mean(put_credit * 100)),
         mean_profit = mean(put_profit * 100),
         mean_profit = ifelse(mean_profit < 0, 
                              cell_spec(dollar(mean_profit), 
                                        color = "red", italic = TRUE),
                              dollar(mean_profit)),
         total_profit = sum(put_profit * 100),
         total_profit = ifelse(total_profit < 0, 
                               cell_spec(dollar(total_profit), 
                                         color = "red", italic = TRUE),
                               dollar(total_profit))) %>%
  distinct(symbol, trade_count, win_rate, mean_credit, 
           mean_profit, total_profit) %>%
  ungroup() %>%
  arrange(desc(win_rate)) %>%
  mutate(win_rate = percent(win_rate))
```

#### Metrics table for RSI cross-over entries  


```r
kable(rsi_metrics, digits = 2, format = "html",
      caption = "Metrics for selling puts after cross above RSI 30",
      col.names = c("SYM", "TRADE COUNT", "WIN RATE", "MEAN CREDIT",
                    "MEAN PROFIT", "TOTAL PROFIT"),
      escape = FALSE,
      align = c("l", "r", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE) %>%
  column_spec(., 1:6, width = "1.0in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Metrics for selling puts after cross above RSI 30</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> TRADE COUNT </th>
   <th style="text-align:right;"> WIN RATE </th>
   <th style="text-align:right;"> MEAN CREDIT </th>
   <th style="text-align:right;"> MEAN PROFIT </th>
   <th style="text-align:right;"> TOTAL PROFIT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 1.0in; "> SPY </td>
   <td style="text-align:right;width: 1.0in; "> 11 </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $307.68 </td>
   <td style="text-align:right;width: 1.0in; "> $306.64 </td>
   <td style="text-align:right;width: 1.0in; "> $3,373 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> IWM </td>
   <td style="text-align:right;width: 1.0in; "> 10 </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $208.60 </td>
   <td style="text-align:right;width: 1.0in; "> $208.05 </td>
   <td style="text-align:right;width: 1.0in; "> $2,080.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> QQQ </td>
   <td style="text-align:right;width: 1.0in; "> 8 </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $155.63 </td>
   <td style="text-align:right;width: 1.0in; "> $155.13 </td>
   <td style="text-align:right;width: 1.0in; "> $1,241 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> DIA </td>
   <td style="text-align:right;width: 1.0in; "> 14 </td>
   <td style="text-align:right;width: 1.0in; "> 92.9% </td>
   <td style="text-align:right;width: 1.0in; "> $219.61 </td>
   <td style="text-align:right;width: 1.0in; "> $183.89 </td>
   <td style="text-align:right;width: 1.0in; "> $2,574.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> TLT </td>
   <td style="text-align:right;width: 1.0in; "> 16 </td>
   <td style="text-align:right;width: 1.0in; "> 81.2% </td>
   <td style="text-align:right;width: 1.0in; "> $128.44 </td>
   <td style="text-align:right;width: 1.0in; "> $82.31 </td>
   <td style="text-align:right;width: 1.0in; "> $1,317 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> XLE </td>
   <td style="text-align:right;width: 1.0in; "> 16 </td>
   <td style="text-align:right;width: 1.0in; "> 81.2% </td>
   <td style="text-align:right;width: 1.0in; "> $132.44 </td>
   <td style="text-align:right;width: 1.0in; "> $63.53 </td>
   <td style="text-align:right;width: 1.0in; "> $1,016.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> GLD </td>
   <td style="text-align:right;width: 1.0in; "> 25 </td>
   <td style="text-align:right;width: 1.0in; "> 80.0% </td>
   <td style="text-align:right;width: 1.0in; "> $149.18 </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-3.52</span> </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-88</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EEM </td>
   <td style="text-align:right;width: 1.0in; "> 14 </td>
   <td style="text-align:right;width: 1.0in; "> 78.6% </td>
   <td style="text-align:right;width: 1.0in; "> $73.61 </td>
   <td style="text-align:right;width: 1.0in; "> $10.11 </td>
   <td style="text-align:right;width: 1.0in; "> $141.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EWZ </td>
   <td style="text-align:right;width: 1.0in; "> 18 </td>
   <td style="text-align:right;width: 1.0in; "> 66.7% </td>
   <td style="text-align:right;width: 1.0in; "> $100.22 </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-59.86</span> </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-1,077.50</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> FXE </td>
   <td style="text-align:right;width: 1.0in; "> 14 </td>
   <td style="text-align:right;width: 1.0in; "> 50.0% </td>
   <td style="text-align:right;width: 1.0in; "> $82.86 </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-55.07</span> </td>
   <td style="text-align:right;width: 1.0in; "> <span style="  font-style: italic;   color: red;">$-771</span> </td>
  </tr>
</tbody>
</table>

The table above shows that using the RSI cross-over entry leads to at a high level similar results in that the same symbols (DIA, IWM, QQQ, and SPY) perform better overall than the others. Next we will compare these results more closely to see if there is evidence that the RSI indicator enhances the results.  

#### Does RSI cross-over improve results?  


```r
compare <- rsi_results %>%
  mutate(run = 1001) %>%
  bind_rows(resampled) %>%
  group_by(symbol, run) %>%
  mutate(trade_count = n(),
         profitable = ifelse(put_profit > 0, 1, 0),
         win_rate = sum(profitable) / trade_count,
         mean_credit = mean(put_credit * 100),
         mean_profit = mean(put_profit * 100),
         total_profit = sum(put_profit * 100)) %>%
  distinct(symbol, trade_count, win_rate, mean_credit, 
           mean_profit, total_profit) %>%
  ungroup() %>%
  group_by(symbol) %>%
  arrange(desc(win_rate)) %>%
  mutate(win_rank = cume_dist(win_rate)) %>%
  arrange(desc(mean_profit)) %>%
  mutate(mean_profit_rank = cume_dist(mean_profit)) %>%
  arrange(desc(total_profit)) %>%
  mutate(total_profit_rank = cume_dist(total_profit)) %>%
  ungroup() %>%
  filter(run == 1001) %>%
  select(-c(run, trade_count, mean_credit)) %>%
  arrange(desc(mean_profit_rank)) %>%
  mutate(win_rate = percent(win_rate),
         mean_profit = dollar(mean_profit),
         total_profit = dollar(total_profit),
         win_rank = percent(win_rank),
         mean_profit_rank = percent(mean_profit_rank),
         total_profit_rank = percent(total_profit_rank))
```

#### Metrics table Final Results    


```r
kable(compare, digits = 2, format = "html",
      caption = "How does using RSI compare to random entries?",
      col.names = c("SYM", "Win Rate", "Mean Profit", "Total Profit",
                    "Win Rate Rank", "Mean Profit Rank", "Total Profit Rank"),
      escape = FALSE,
      align = c("l", "r", "r", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = FALSE) %>%
  column_spec(1:7, width = "1.0in") %>%
  column_spec(5:7, bold = TRUE, color = "white", background = "orange") %>%
  add_header_above(., c("", "RSI Cross Entries" = 3, "Rank Against all 1000" = 3))
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2 How does using RSI compare to random entries?</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">RSI Cross Entries</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Rank Against all 1000</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> Win Rate </th>
   <th style="text-align:right;"> Mean Profit </th>
   <th style="text-align:right;"> Total Profit </th>
   <th style="text-align:right;"> Win Rate Rank </th>
   <th style="text-align:right;"> Mean Profit Rank </th>
   <th style="text-align:right;"> Total Profit Rank </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 1.0in; "> SPY </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $306.64 </td>
   <td style="text-align:right;width: 1.0in; "> $3,373.00 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> IWM </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $208.05 </td>
   <td style="text-align:right;width: 1.0in; "> $2,080.50 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> QQQ </td>
   <td style="text-align:right;width: 1.0in; "> 100.0% </td>
   <td style="text-align:right;width: 1.0in; "> $155.13 </td>
   <td style="text-align:right;width: 1.0in; "> $1,241.00 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 100.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 99.4% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 99.4% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> DIA </td>
   <td style="text-align:right;width: 1.0in; "> 92.9% </td>
   <td style="text-align:right;width: 1.0in; "> $183.89 </td>
   <td style="text-align:right;width: 1.0in; "> $2,574.50 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 75.8% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 96.7% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 96.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> TLT </td>
   <td style="text-align:right;width: 1.0in; "> 81.2% </td>
   <td style="text-align:right;width: 1.0in; "> $82.31 </td>
   <td style="text-align:right;width: 1.0in; "> $1,317.00 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 57.6% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 85.0% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 85.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> XLE </td>
   <td style="text-align:right;width: 1.0in; "> 81.2% </td>
   <td style="text-align:right;width: 1.0in; "> $63.53 </td>
   <td style="text-align:right;width: 1.0in; "> $1,016.50 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 71.6% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 78.1% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 78.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> GLD </td>
   <td style="text-align:right;width: 1.0in; "> 80.0% </td>
   <td style="text-align:right;width: 1.0in; "> $-3.52 </td>
   <td style="text-align:right;width: 1.0in; "> $-88.00 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 77.2% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 53.6% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 53.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EEM </td>
   <td style="text-align:right;width: 1.0in; "> 78.6% </td>
   <td style="text-align:right;width: 1.0in; "> $10.11 </td>
   <td style="text-align:right;width: 1.0in; "> $141.50 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 47.8% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 31.3% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 31.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EWZ </td>
   <td style="text-align:right;width: 1.0in; "> 66.7% </td>
   <td style="text-align:right;width: 1.0in; "> $-59.86 </td>
   <td style="text-align:right;width: 1.0in; "> $-1,077.50 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 28.4% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 13.1% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 13.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> FXE </td>
   <td style="text-align:right;width: 1.0in; "> 50.0% </td>
   <td style="text-align:right;width: 1.0in; "> $-55.07 </td>
   <td style="text-align:right;width: 1.0in; "> $-771.00 </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 1.9% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 6.8% </td>
   <td style="text-align:right;width: 1.0in; font-weight: bold;color: white;background-color: orange;"> 6.8% </td>
  </tr>
</tbody>
</table>

Using the above table we see that the broad market ETFs do seem to have better performance when using RSI cross-over as an entry criteria. Notable however is the lack of performance from the other stocks with FXE being one of the worst performing groups from the entire dataset of 1000 random samples. 

*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
