---
title: MACD - RSI - Short Puts
author: Jason Taylor
date: '2018-10-04'
slug: macd-rsi-short-puts
draft: FALSE
categories:
  - Original
tags:
  - backtest study
  - options
  - Original Content
  - RSI
  - rstats
  - Short Put
  - MACD
topics: []
description: ''
---

#### Summary:

The intent for this post is to build technical analysis tools (RSI, MACD) into our studies and use them as entry points. We have selected a single entry criteria and exit target combination to build these functions. In future posts we can then use grid search to perform hyperparameter optimization of these entry and exit criteria and compare results with a baseline strategy. The goal of this post is not to evaluate the performance of the strategy but to continue with building tools that allow futher research.

<!--more-->

In addition we have created and used functions for calculating the running margin in use *tastytrade::margin_use* and calculating concurrent open positions *tastytrade::concurrent_trades* to enable limiting the number of positions opened in the same underlying at the same time.



#### Study Setup:

* Calculate the RSI(14) and MACD(12, 26, 9, SMA) for each underlying 

* The entry criteria will be defined as:

  * Sell 30 delta short puts when the RSI(14) crosses above 50
  
  * RSI(14) has been below 40 within the last 10 trading days
  
  * MACD is positive (12 SMA above the 26 SMA)

* No new position is opened if there is an existing in the same underlying or it would cause the max margin in use to be exceeded

* Positions are closed at 50% of max profit, 2X loss, or expiration, whichever comes first

* The watch list is filtered to stocks without earnings to avoid binary events impacting results

  * "DIA", "EEM", "EWJ", "EWW", "EWZ", "FXE", "FXI", "GDX", "IWM", "IYR", "QQQ", "SLV", "SPY", "TLT", "UNG", "USO", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XME", "XOP", "XRT"

#### Study Arguments:



* Target DTE: 60

* Target Delta: -0.3

* Maximum Margin in Use: $10,000

* Maximum Margin per Contract: $2,000

* Minimum Return on Capital (ROC) at Entry: 1.50%

* Minimum Credit Recieved per Contract: 0.4

* Percent of Stock Price used for Margin: 20.0%

#### Study Function:


```r
study <- function(stock, tar_dte, tar_buffer, tar_delta) {
  rs_conn <- tastytrade::redshift_connect("TASTYTRADE")
  options <- rs_conn %>%
    tbl(stock) %>%
    mutate(m_dte = abs(dte - tar_dte))
  
  study_entries <- options %>%
    distinct(symbol, quotedate, close_price) %>%
    collect() %>%
    arrange(quotedate) %>%
    mutate(quotedate = as.Date(quotedate, format = "%Y-%m-%d"),
           rsi = RSI(close_price, n = 14),
           !!!tastytrade::lags(rsi, 10)) %>%
    tq_mutate_xy_(x = "close_price", mutate_fun = "MACD", 
                  nFast = 12, nSlow = 26, nSig = 9,
                  maType = SMA, percent = FALSE) %>%
    mutate(trigger = case_when(macd > signal ~ 1, TRUE ~ 0)) %>%
    filter(rsi > rsi_cross, 
           lag_rsi_01 < rsi_cross,
           trigger == 1) %>%
    filter_at(vars(starts_with("lag_rsi")), any_vars((. < rsi_recent_low)))
  
  opened_puts <- tastytrade::open_leg(
    data = filter(options, quotedate %in% study_entries$quotedate),
    put_call = "put",
    direction = "credit",
    tar_delta = tar_delta,
    tar_buffer = tar_buffer)
  
  pmap_dfr(list(df = list(options), 
                entry_date = as.character(opened_puts$quotedate),
                exp = as.character(opened_puts$expiration), 
                typ = list("put"),
                stk = opened_puts$strike,
                entry_mid = opened_puts$mid,
                entry_delta = opened_puts$put_delta_strike,
                stk_price = opened_puts$close_price,
                direction = list("short")),
           tastytrade::close_leg)
}
```

#### Map Study Function:


```r
results <- pmap_dfr(list(args$symbol, args$tar_dte, args$tar_buffer,
                         args$tar_delta), study)
```



#### Filter results to 50% profit, 2X loss, or expiration whichever comes first


```r
first_exit <- results %>%
  filter(entry_stock_price < 100,
         put_entry_mid >= min_entry_credit) %>%
  mutate(entry_margin = margin_percent * entry_stock_price,
         contracts = floor(margin_contract / entry_margin),
         entry_margin = entry_margin * contracts,
         put_entry_mid = 100 * put_entry_mid,
         put_exit_mid = 100 * put_exit_mid,
         put_profit = 100 * put_profit,
         entry_roc = (put_entry_mid * contracts) / entry_margin,
         tar_prof_hit =
           case_when(put_profit >= (put_entry_mid * tar_profit) ~ 1,
                     TRUE ~ 0),
         tar_loss_hit = 
           case_when(put_profit <= (put_entry_mid * stop_loss) ~ 1,
                     TRUE ~ 0),
         put_profit = case_when(
           tar_prof_hit == 1 ~ put_entry_mid * tar_profit * contracts,
           tar_loss_hit == 1 ~ put_entry_mid * stop_loss * contracts,
           TRUE ~ put_profit)) %>%
  group_by(symbol, entry_date) %>%
  filter(quotedate == expiration |
           tar_prof_hit == 1 | 
           tar_loss_hit == 1) %>%
  filter(quotedate == min(quotedate)) %>%
  ungroup() %>%
  filter(entry_roc >= min_entry_roc) %>%
  mutate(profitable = case_when(put_profit > 0 ~ 1, TRUE ~ 0)) %>%
  group_by(symbol) %>%
  mutate(num = n()) %>%
  ungroup()
```

#### Calculate Concurrent Positions:

As a requirement of the study we will not open multiple trades in the same underlying so we must calculate entries that overlap and can be filtered out.


```r
concurrent <- pmap_dfr(list(first_exit$symbol,
                                   as.character(first_exit$entry_date), 
                                   as.character(first_exit$quotedate)), 
                              tastytrade::concurrent_trades) %>%
  group_by(symbol, quote_date) %>%
  summarise(con_pos = n())

first_exit <- left_join(first_exit, concurrent, 
                        by = c("symbol", "entry_date" = "quote_date")) %>%
  filter(con_pos == 1)
```

#### Calulate Margin in Use:

Another requirement of the study is that we do not exceed the maximum margin in use of $10,000. Here we calculate the running margin to check if a new position in any underyling would push above this limit. If so, we will skip this trade. 

This last filter will give us the final list of trade executions so we can calculate the cummulative profit for the study.


```r
run_margin <-
  pmap_dfr(list(first_exit$symbol,
                as.character(first_exit$entry_date), 
                as.character(first_exit$quotedate), 
                first_exit$entry_margin), 
           tastytrade::margin_use) %>%
  group_by(entry_date) %>%
  summarise(margin_use = sum(margin, na.rm = TRUE))

first_exit <- left_join(first_exit, run_margin, by = "entry_date") %>%
  filter(margin_use < margin_limit) %>%
  arrange(entry_date) %>%
  mutate(run_profit = cumsum(put_profit))
```

#### Calculate metrics for all trades combined:


```r
total_metrics <- first_exit %>%
  mutate(num_trades = n(),
         mean_profit = mean(put_profit),
         profit = sum(put_profit),
         max_profit = case_when(
           max(put_profit) > 0 ~ max(put_profit),
           TRUE ~ 0),
         max_loss = case_when(
           min(put_profit) < 0 ~ min(put_profit),
           TRUE ~ 0),
         win_rate = sum(profitable) / num_trades,
         rate_prof_tar = sum(tar_prof_hit) / num_trades,
         rate_loss_tar = sum(tar_loss_hit) / num_trades,
         rate_exp = (num_trades - sum(tar_prof_hit) - sum(tar_loss_hit)) /
           num_trades) %>%
  distinct(num_trades, mean_profit,
           profit, max_profit, max_loss, win_rate,
           rate_prof_tar, rate_loss_tar, rate_exp) %>%
  mutate(win_rate = percent(win_rate),
         mean_profit = dollar(mean_profit),
         profit = dollar(profit),
         max_loss = case_when(
           max_loss < 0 ~ cell_spec(dollar(max_loss), color = "red", italic = TRUE),
           TRUE ~ dollar(max_loss)),
         max_profit = dollar(max_profit),
         rate_prof_tar = percent(rate_prof_tar),
         rate_loss_tar = percent(rate_loss_tar),
         rate_exp = percent(rate_exp))
```



```r
kable(total_metrics, digits = 2, format = "html",
      caption = "Summary Total Results",
      col.names = c("Num Trades", "Mean Profit", "Total Profit", "Max Profit",
                    "Max Loss", "Win Rate", "% Prof Tar", "% Loss Lim", "% Exp"),
      escape = FALSE,
      align = rep("l", 9)) %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = FALSE) %>%
  column_spec(1:9, width = "1.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Summary Total Results</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Num Trades </th>
   <th style="text-align:left;"> Mean Profit </th>
   <th style="text-align:left;"> Total Profit </th>
   <th style="text-align:left;"> Max Profit </th>
   <th style="text-align:left;"> Max Loss </th>
   <th style="text-align:left;"> Win Rate </th>
   <th style="text-align:left;"> % Prof Tar </th>
   <th style="text-align:left;"> % Loss Lim </th>
   <th style="text-align:left;"> % Exp </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 1.5in; "> 162 </td>
   <td style="text-align:left;width: 1.5in; "> $15.90 </td>
   <td style="text-align:left;width: 1.5in; "> $2,576.50 </td>
   <td style="text-align:left;width: 1.5in; "> $352.50 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-922.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 84.6% </td>
   <td style="text-align:left;width: 1.5in; "> 84.0% </td>
   <td style="text-align:left;width: 1.5in; "> 14.8% </td>
   <td style="text-align:left;width: 1.5in; "> 1.23% </td>
  </tr>
</tbody>
</table>

#### Calculate metrics for trades grouped by symbol:


```r
symbol_metrics <- first_exit %>%
  group_by(symbol) %>%
  mutate(num_trades = n(),
         mean_profit = mean(put_profit),
         symbol_profit = sum(put_profit),
         max_profit = case_when(
           max(put_profit) > 0 ~ max(put_profit),
           TRUE ~ 0),
         max_loss = case_when(
           min(put_profit) < 0 ~ min(put_profit),
           TRUE ~ 0),
         win_rate = sum(profitable) / num_trades) %>%
  ungroup() %>%
  distinct(symbol, num_trades, mean_profit, max_profit, max_loss,
           symbol_profit, win_rate) %>%
  arrange(symbol) %>%
  mutate(mean_profit = case_when(
    mean_profit < 0 ~ cell_spec(dollar(mean_profit), color = "red", italic = TRUE),
    TRUE ~ dollar(mean_profit)),
    symbol_profit = case_when(
      symbol_profit < 0 ~ cell_spec(dollar(symbol_profit), color = "red", italic = TRUE),
      TRUE ~ dollar(symbol_profit)),
    max_profit = dollar(max_profit),
    max_loss = case_when(
      max_loss < 0 ~ cell_spec(dollar(max_loss), color = "red", italic = TRUE),
      TRUE ~ dollar(max_loss)),
    win_rate = percent(win_rate))
```


```r
kable(symbol_metrics, digits = 2, format = "html",
      caption = "Summary Results",
      col.names = c("Symbol", "Num Trades", "Mean Profit", "Total Profit",
                    "Max Profit", "Max Loss", "Win Rate"),
      escape = FALSE,
      align = rep("l", 7)) %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = FALSE) %>%
  column_spec(1:7, width = "1.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2 Summary Results</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Symbol </th>
   <th style="text-align:left;"> Num Trades </th>
   <th style="text-align:left;"> Mean Profit </th>
   <th style="text-align:left;"> Total Profit </th>
   <th style="text-align:left;"> Max Profit </th>
   <th style="text-align:left;"> Max Loss </th>
   <th style="text-align:left;"> Win Rate </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 1.5in; "> EEM </td>
   <td style="text-align:left;width: 1.5in; "> 7 </td>
   <td style="text-align:left;width: 1.5in; "> $32.36 </td>
   <td style="text-align:left;width: 1.5in; "> $226.50 </td>
   <td style="text-align:left;width: 1.5in; "> $93.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-238.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 85.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> EWW </td>
   <td style="text-align:left;width: 1.5in; "> 12 </td>
   <td style="text-align:left;width: 1.5in; "> $31.25 </td>
   <td style="text-align:left;width: 1.5in; "> $375.00 </td>
   <td style="text-align:left;width: 1.5in; "> $119.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-162.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 83.3% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> EWZ </td>
   <td style="text-align:left;width: 1.5in; "> 8 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-147.44</span> </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-1,179.50</span> </td>
   <td style="text-align:left;width: 1.5in; "> $132.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-922.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 62.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> FXI </td>
   <td style="text-align:left;width: 1.5in; "> 9 </td>
   <td style="text-align:left;width: 1.5in; "> $74.42 </td>
   <td style="text-align:left;width: 1.5in; "> $669.75 </td>
   <td style="text-align:left;width: 1.5in; "> $95.25 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> GDX </td>
   <td style="text-align:left;width: 1.5in; "> 12 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-115.48</span> </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-1,385.75</span> </td>
   <td style="text-align:left;width: 1.5in; "> $178.50 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-696.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 66.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> IWM </td>
   <td style="text-align:left;width: 1.5in; "> 1 </td>
   <td style="text-align:left;width: 1.5in; "> $77.50 </td>
   <td style="text-align:left;width: 1.5in; "> $77.50 </td>
   <td style="text-align:left;width: 1.5in; "> $77.50 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> IYR </td>
   <td style="text-align:left;width: 1.5in; "> 11 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-19.32</span> </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-212.50</span> </td>
   <td style="text-align:left;width: 1.5in; "> $66.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-229.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 72.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> SLV </td>
   <td style="text-align:left;width: 1.5in; "> 9 </td>
   <td style="text-align:left;width: 1.5in; "> $49.22 </td>
   <td style="text-align:left;width: 1.5in; "> $443.00 </td>
   <td style="text-align:left;width: 1.5in; "> $162.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-564.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 88.9% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> UNG </td>
   <td style="text-align:left;width: 1.5in; "> 11 </td>
   <td style="text-align:left;width: 1.5in; "> $73.77 </td>
   <td style="text-align:left;width: 1.5in; "> $811.50 </td>
   <td style="text-align:left;width: 1.5in; "> $261.25 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-615.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 81.8% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> USO </td>
   <td style="text-align:left;width: 1.5in; "> 5 </td>
   <td style="text-align:left;width: 1.5in; "> $31.25 </td>
   <td style="text-align:left;width: 1.5in; "> $156.25 </td>
   <td style="text-align:left;width: 1.5in; "> $147.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-234.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 80.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLB </td>
   <td style="text-align:left;width: 1.5in; "> 8 </td>
   <td style="text-align:left;width: 1.5in; "> $0.87 </td>
   <td style="text-align:left;width: 1.5in; "> $7.00 </td>
   <td style="text-align:left;width: 1.5in; "> $88.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-242.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 75.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLE </td>
   <td style="text-align:left;width: 1.5in; "> 6 </td>
   <td style="text-align:left;width: 1.5in; "> $59.08 </td>
   <td style="text-align:left;width: 1.5in; "> $354.50 </td>
   <td style="text-align:left;width: 1.5in; "> $94.00 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLF </td>
   <td style="text-align:left;width: 1.5in; "> 1 </td>
   <td style="text-align:left;width: 1.5in; "> $86.00 </td>
   <td style="text-align:left;width: 1.5in; "> $86.00 </td>
   <td style="text-align:left;width: 1.5in; "> $86.00 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLI </td>
   <td style="text-align:left;width: 1.5in; "> 8 </td>
   <td style="text-align:left;width: 1.5in; "> $9.38 </td>
   <td style="text-align:left;width: 1.5in; "> $75.00 </td>
   <td style="text-align:left;width: 1.5in; "> $61.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-208.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 87.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLK </td>
   <td style="text-align:left;width: 1.5in; "> 3 </td>
   <td style="text-align:left;width: 1.5in; "> $65.42 </td>
   <td style="text-align:left;width: 1.5in; "> $196.25 </td>
   <td style="text-align:left;width: 1.5in; "> $68.50 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLP </td>
   <td style="text-align:left;width: 1.5in; "> 4 </td>
   <td style="text-align:left;width: 1.5in; "> $11.37 </td>
   <td style="text-align:left;width: 1.5in; "> $45.50 </td>
   <td style="text-align:left;width: 1.5in; "> $68.50 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-87.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 75.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLU </td>
   <td style="text-align:left;width: 1.5in; "> 12 </td>
   <td style="text-align:left;width: 1.5in; "> $45.58 </td>
   <td style="text-align:left;width: 1.5in; "> $547.00 </td>
   <td style="text-align:left;width: 1.5in; "> $74.50 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XLV </td>
   <td style="text-align:left;width: 1.5in; "> 8 </td>
   <td style="text-align:left;width: 1.5in; "> $19.91 </td>
   <td style="text-align:left;width: 1.5in; "> $159.25 </td>
   <td style="text-align:left;width: 1.5in; "> $64.00 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-157.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 87.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XME </td>
   <td style="text-align:left;width: 1.5in; "> 10 </td>
   <td style="text-align:left;width: 1.5in; "> $18.92 </td>
   <td style="text-align:left;width: 1.5in; "> $189.25 </td>
   <td style="text-align:left;width: 1.5in; "> $352.50 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-628.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 80.0% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XOP </td>
   <td style="text-align:left;width: 1.5in; "> 7 </td>
   <td style="text-align:left;width: 1.5in; "> $40.46 </td>
   <td style="text-align:left;width: 1.5in; "> $283.25 </td>
   <td style="text-align:left;width: 1.5in; "> $135.50 </td>
   <td style="text-align:left;width: 1.5in; "> <span style="  font-style: italic;   color: red;">$-283.00</span> </td>
   <td style="text-align:left;width: 1.5in; "> 85.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.5in; "> XRT </td>
   <td style="text-align:left;width: 1.5in; "> 10 </td>
   <td style="text-align:left;width: 1.5in; "> $65.17 </td>
   <td style="text-align:left;width: 1.5in; "> $651.75 </td>
   <td style="text-align:left;width: 1.5in; "> $79.50 </td>
   <td style="text-align:left;width: 1.5in; "> $0.00 </td>
   <td style="text-align:left;width: 1.5in; "> 100.0% </td>
  </tr>
</tbody>
</table>


```r
ggplot(data = first_exit, aes(x = entry_date, y = run_profit)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "blue", size = 1.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::dollar) +
  xlab("Entry Date") +
  ylab("Profit") +
  ggtitle("Cumulative Return")
```

<img src="/post/macd-rsi-short-puts_files/figure-html/unnamed-chunk-8-1.png" width="672" />


```r
ggplot(distinct(first_exit, entry_date, margin_use)) +
  geom_area(aes(x = entry_date, y = margin_use), fill = "steelblue2") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = scales::dollar) +
  xlab("") +
  ylab("Margin") +
  ggtitle("Margin in Use")                             
```

<img src="/post/macd-rsi-short-puts_files/figure-html/unnamed-chunk-9-1.png" width="672" />

*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
