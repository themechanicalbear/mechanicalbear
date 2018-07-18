---
title: Managing Earlier - Stock Movement
author: Jason Taylor
date: '2018-07-12'
draft: FALSE
slug: managing-earlier-stock-movement
summary: Why does managing positions earlier reduce portfolio volatility?
categories:
  - Tastytrade
tags:
  - Stock
  - Volatility
header:
  caption: ''
  image: ''
---

### Market Measures - July 12, 2018

Why does managing positions earlier reduce portfolio volatility? Gamma risk is one answer. But to be more intuitive, we can look at how a stock moves over the course of a trade held to expiration versus how much a stock changes when the position is managed early.

Compared stock movement by:

Holding-to-expiration and managing earlier
Stock prices at order entry and at exit point for all occurrences  

#### Results:  
We find that the stock moves (on average) a lot less over a 22 day period than a 45 day period. This translates to lower portfolio volatility because you are less exposed to directional movements.

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tastytrade", "dplyr", "ggplot2", "here", "purrr", "tidyr",
                    "data.table", "knitr", "kableExtra")
  lapply(library_list, require, character.only = TRUE)})))
stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE", "EEM",
                "FB", "FXI", "SLV", "EWZ", "FXE", "TBT", "IBM")
tar_dte <- 45
early_exit <- 22
```

#### Study function


```r
study <- function(stock) {
  options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte),
                  mid_dte = abs(dte - early_exit))
  
  expirations <- dplyr::filter(options, quotedate == expiration) %>%
    dplyr::distinct(quotedate, close, expiration)
  
  dte_45 <- options %>%
    dplyr::group_by(quotedate) %>%
    dplyr::filter(m_dte == min(m_dte)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(quotedate, close, expiration, dte)
  
  mid_exit <- function(exp) {
    options %>%
      dplyr::filter(expiration == exp) %>%
      dplyr::filter(mid_dte == min(mid_dte)) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::select(quotedate, close, expiration, dte)
  }
  
  dte_22 <- purrr::map_dfr(dte_45$expiration, mid_exit)
  
  results <- dplyr::left_join(dte_45, dte_22, by = "expiration") %>%
    dplyr::left_join(expirations, by = "expiration") %>%
    dplyr::distinct() %>%
    magrittr::set_colnames(c("open_date", "open_price", "expiration", "open_dte", 
                             "mid_date", "mid_price", "mid_dte", "close_date",
                             "close_price")) %>%
    dplyr::mutate(symbol = stock,
                  mid_diff = mid_price - open_price,
                  close_diff = close_price - open_price,
                  mid_return = (mid_price - open_price) / open_price,
                  close_return = (close_price - open_price) / open_price) %>%
    dplyr::mutate(early_better = ifelse(abs(close_diff) >= abs(mid_diff), 1, 0)) %>%
    dplyr::filter(complete.cases(.))
}
```

#### Run study against stock list


```r
results <- purrr::map_dfr(stock_list, study)
```

#### Plot absolute value of price changes showing less volatility when closed early


```r
group_one_returns <- results %>%
  dplyr::filter(symbol %in% c("EWZ", "TLT", "SLV", "FXI", "XLE", "EEM", "FXE"))
group_two_returns <- results %>%
  dplyr::filter(symbol %in% c("GLD", "QQQ", "DIA", "IWM", "IBM", "SPY"))

grouped_plot <- function(df) {
  ggplot(data = df, aes(x = open_date, y = abs(mid_diff), color = "mid price")) +
    geom_line() +
    geom_line(data = df, aes(x = open_date, y = abs(close_diff), 
                             color = "close price")) +
    scale_fill_brewer() +
    theme_dark() + 
    labs(title = "ABS stock value change compared to open price", x = "Date",
         y = "Stock change") +
    facet_grid(rows = vars(symbol), scales = "free_y")
}

grouped_plot(group_one_returns)
```

<img src="/post/2018-07-12-managing-earlier-stock-movement_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
grouped_plot(group_two_returns)
```

<img src="/post/2018-07-12-managing-earlier-stock-movement_files/figure-html/unnamed-chunk-2-2.png" width="672" />

#### Percentage of days when closing earlier reduced abs price change


```r
early_better_pct <- results %>%
  dplyr::group_by(symbol, early_better) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(early_better == 1) %>%
  dplyr::select(symbol, freq) %>%
  dplyr::arrange(desc(freq))

knitr::kable(early_better_pct, digits = 3, 
             caption = "Percent when price change is less after 22 days than 45") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Percent when price change is less after 22 days than 45</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:right;"> freq </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:right;"> 0.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:right;"> 0.705 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:right;"> 0.690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:right;"> 0.687 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:right;"> 0.686 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:right;"> 0.684 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:right;"> 0.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:right;"> 0.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:right;"> 0.658 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:right;"> 0.652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:right;"> 0.651 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:right;"> 0.650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:right;"> 0.644 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:right;"> 0.603 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> 0.565 </td>
  </tr>
</tbody>
</table>

#### Table with closing metrics


```r
return_metrics <- results %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(avg_mid = scales::percent(mean(mid_return)),
                avg_close = scales::percent(mean(close_return)),
                max_mid = scales::percent(max(mid_return)),
                max_close = scales::percent(max(close_return)),
                min_mid = scales::percent(min(mid_return)),
                min_close = scales::percent(min(close_return)),
                sd_mid = scales::percent(sd(mid_return)),
                sd_close = scales::percent(sd(close_return))) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(symbol, avg_mid, avg_close, max_mid, max_close, min_mid,
                  min_close, sd_mid, sd_close) %>%
  dplyr::arrange(symbol)

knitr::kable(return_metrics, digits = 3, 
             caption = "Percentage Returns and Standard Deviation") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 2 Percentage Returns and Standard Deviation</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> symbol </th>
   <th style="text-align:left;"> avg_mid </th>
   <th style="text-align:left;"> avg_close </th>
   <th style="text-align:left;"> max_mid </th>
   <th style="text-align:left;"> max_close </th>
   <th style="text-align:left;"> min_mid </th>
   <th style="text-align:left;"> min_close </th>
   <th style="text-align:left;"> sd_mid </th>
   <th style="text-align:left;"> sd_close </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> DIA </td>
   <td style="text-align:left;"> 0.674% </td>
   <td style="text-align:left;"> 1.39% </td>
   <td style="text-align:left;"> 9.14% </td>
   <td style="text-align:left;"> 12.1% </td>
   <td style="text-align:left;"> -8.9% </td>
   <td style="text-align:left;"> -10.7% </td>
   <td style="text-align:left;"> 2.59% </td>
   <td style="text-align:left;"> 3.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EEM </td>
   <td style="text-align:left;"> 0.0928% </td>
   <td style="text-align:left;"> 0.458% </td>
   <td style="text-align:left;"> 14.3% </td>
   <td style="text-align:left;"> 16.8% </td>
   <td style="text-align:left;"> -12.2% </td>
   <td style="text-align:left;"> -18.6% </td>
   <td style="text-align:left;"> 4.07% </td>
   <td style="text-align:left;"> 5.95% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EWZ </td>
   <td style="text-align:left;"> -0.298% </td>
   <td style="text-align:left;"> -0.159% </td>
   <td style="text-align:left;"> 33.3% </td>
   <td style="text-align:left;"> 47.4% </td>
   <td style="text-align:left;"> -19.3% </td>
   <td style="text-align:left;"> -27.3% </td>
   <td style="text-align:left;"> 7.2% </td>
   <td style="text-align:left;"> 11.2% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FB </td>
   <td style="text-align:left;"> 2.88% </td>
   <td style="text-align:left;"> 4.45% </td>
   <td style="text-align:left;"> 55.7% </td>
   <td style="text-align:left;"> 79.1% </td>
   <td style="text-align:left;"> -34.9% </td>
   <td style="text-align:left;"> -42.4% </td>
   <td style="text-align:left;"> 9.09% </td>
   <td style="text-align:left;"> 12.1% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXE </td>
   <td style="text-align:left;"> -0.0154% </td>
   <td style="text-align:left;"> -0.179% </td>
   <td style="text-align:left;"> 6.46% </td>
   <td style="text-align:left;"> 7.48% </td>
   <td style="text-align:left;"> -6.14% </td>
   <td style="text-align:left;"> -10% </td>
   <td style="text-align:left;"> 1.89% </td>
   <td style="text-align:left;"> 2.7% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FXI </td>
   <td style="text-align:left;"> 0.212% </td>
   <td style="text-align:left;"> 0.794% </td>
   <td style="text-align:left;"> 23% </td>
   <td style="text-align:left;"> 27.3% </td>
   <td style="text-align:left;"> -16% </td>
   <td style="text-align:left;"> -21.7% </td>
   <td style="text-align:left;"> 5.44% </td>
   <td style="text-align:left;"> 7.45% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GLD </td>
   <td style="text-align:left;"> -0.169% </td>
   <td style="text-align:left;"> -0.588% </td>
   <td style="text-align:left;"> 13.4% </td>
   <td style="text-align:left;"> 16.7% </td>
   <td style="text-align:left;"> -15.2% </td>
   <td style="text-align:left;"> -15.6% </td>
   <td style="text-align:left;"> 3.82% </td>
   <td style="text-align:left;"> 5.11% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IBM </td>
   <td style="text-align:left;"> -0.11% </td>
   <td style="text-align:left;"> -0.389% </td>
   <td style="text-align:left;"> 15.7% </td>
   <td style="text-align:left;"> 25.5% </td>
   <td style="text-align:left;"> -14% </td>
   <td style="text-align:left;"> -15.5% </td>
   <td style="text-align:left;"> 4.18% </td>
   <td style="text-align:left;"> 5.89% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IWM </td>
   <td style="text-align:left;"> 0.758% </td>
   <td style="text-align:left;"> 1.58% </td>
   <td style="text-align:left;"> 16.2% </td>
   <td style="text-align:left;"> 18.2% </td>
   <td style="text-align:left;"> -12.6% </td>
   <td style="text-align:left;"> -16.5% </td>
   <td style="text-align:left;"> 3.51% </td>
   <td style="text-align:left;"> 4.92% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QQQ </td>
   <td style="text-align:left;"> 0.977% </td>
   <td style="text-align:left;"> 2.1% </td>
   <td style="text-align:left;"> 11.9% </td>
   <td style="text-align:left;"> 15.5% </td>
   <td style="text-align:left;"> -9.95% </td>
   <td style="text-align:left;"> -14.2% </td>
   <td style="text-align:left;"> 3.13% </td>
   <td style="text-align:left;"> 4.54% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:left;"> -0.467% </td>
   <td style="text-align:left;"> -1.32% </td>
   <td style="text-align:left;"> 22.1% </td>
   <td style="text-align:left;"> 25.9% </td>
   <td style="text-align:left;"> -18.6% </td>
   <td style="text-align:left;"> -23.1% </td>
   <td style="text-align:left;"> 6.36% </td>
   <td style="text-align:left;"> 8.33% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SPY </td>
   <td style="text-align:left;"> 0.709% </td>
   <td style="text-align:left;"> 1.49% </td>
   <td style="text-align:left;"> 9.06% </td>
   <td style="text-align:left;"> 11.8% </td>
   <td style="text-align:left;"> -8.99% </td>
   <td style="text-align:left;"> -10.9% </td>
   <td style="text-align:left;"> 2.48% </td>
   <td style="text-align:left;"> 3.58% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:left;"> 1.14% </td>
   <td style="text-align:left;"> 5.06% </td>
   <td style="text-align:left;"> 324% </td>
   <td style="text-align:left;"> 332% </td>
   <td style="text-align:left;"> -75.6% </td>
   <td style="text-align:left;"> -21.7% </td>
   <td style="text-align:left;"> 25.2% </td>
   <td style="text-align:left;"> 43.5% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLT </td>
   <td style="text-align:left;"> 0.142% </td>
   <td style="text-align:left;"> 0.159% </td>
   <td style="text-align:left;"> 8.84% </td>
   <td style="text-align:left;"> 12.3% </td>
   <td style="text-align:left;"> -9.1% </td>
   <td style="text-align:left;"> -12.7% </td>
   <td style="text-align:left;"> 2.66% </td>
   <td style="text-align:left;"> 4.19% </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XLE </td>
   <td style="text-align:left;"> 0.0443% </td>
   <td style="text-align:left;"> 0.136% </td>
   <td style="text-align:left;"> 15.6% </td>
   <td style="text-align:left;"> 17% </td>
   <td style="text-align:left;"> -14.8% </td>
   <td style="text-align:left;"> -21% </td>
   <td style="text-align:left;"> 4.25% </td>
   <td style="text-align:left;"> 6.14% </td>
  </tr>
</tbody>
</table>
