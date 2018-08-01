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
  library_list <- c("tastytrade", "tidyverse", "here", "RJDBC",
                    "data.table", "knitr", "kableExtra")
  lapply(library_list, require, character.only = TRUE)})))
stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA",
                "TLT", "XLE", "EEM", "EWZ", "FXE")
args <- expand.grid(symbol = c("SPY", "IWM", "GLD", "QQQ", "DIA",
                               "TLT", "XLE", "EEM", "EWZ", "FXE"),
                    tar_dte = 45,
                    tar_mid_dte = 21,
                    stringsAsFactors = FALSE) %>%
  dplyr::arrange(symbol)
monthly <- readRDS(paste0(here::here(), "/data/monthly.RDS"))
```

#### Study function


```r
study <- function(stock, tar_dte, early_exit) {
  rs_conn <- tastytrade::redshift_connect("TASTYTRADE")
  options <- rs_conn %>%
    dplyr::tbl(stock) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte))
  
  expirations <- dplyr::filter(options, quotedate == expiration) %>%
    dplyr::distinct(quotedate, close_price, expiration) %>%
    dplyr::collect()
  
  dte_45 <- options %>%
    dplyr::filter(quotedate %in% monthly$date) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::filter(m_dte == min(m_dte, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(quotedate, close_price, expiration, dte) %>%
    dplyr::collect()
  
  dte_22 <- purrr::pmap_dfr(list(list(options),
                                 early_exit, dte_45$expiration),
                            tastytrade::dte_exit)
  
  RJDBC::dbDisconnect(rs_conn)
  
  dplyr::left_join(dte_45, dte_22, by = "expiration") %>%
    dplyr::left_join(expirations, by = "expiration") %>%
    dplyr::distinct() %>%
    magrittr::set_colnames(., c("open_date", "open_price", "expiration",
                                "open_dte", "mid_date", "mid_price",
                                "mid_dte", "close_date", "close_price")) %>%
    dplyr::mutate(symbol = stock,
                  mid_diff = mid_price - open_price,
                  close_diff = close_price - open_price,
                  mid_return = (mid_price - open_price) / open_price,
                  close_return = (close_price - open_price) / open_price) %>%
    dplyr::mutate(early_better = ifelse(abs(close_diff) >= abs(mid_diff),
                                        1, 0)) %>%
    dplyr::filter(complete.cases(.))
}
```

#### Run study against stock list


```r
results <- 
  purrr::pmap_dfr(list(args$symbol, args$tar_dte, args$tar_mid_dte), study)
```

#### Plot absolute value of price changes showing less volatility when closed early


```r
gp1 <- dplyr::filter(results, symbol %in% c("SPY", "IWM", "GLD", "QQQ", "DIA"))
gp2 <- dplyr::filter(results, symbol %in% c("TLT", "XLE", "EEM", "EWZ", "FXE"))

grouped_plot <- function(df) {
  ggplot(data = df, aes(x = as.Date(open_date, format = "%Y-%m-%d"), 
                        y = abs(mid_diff), color = "mid price")) +
    geom_line() +
    geom_line(data = df, aes(x = as.Date(open_date, format = "%Y-%m-%d"),
                             y = abs(close_diff), color = "close price")) +
    scale_fill_brewer() +
    theme_minimal() +
    labs(title = "(abs) stock value change compared to open price", x = "Date",
         y = "Stock change") +
    facet_grid(rows = vars(symbol), scales = "free_y")
}

grouped_plot(gp1)
```

<img src="/post/2018-07-12-managing-earlier-stock-movement_files/figure-html/plots-1.png" width="672" />

```r
grouped_plot(gp2)
```

<img src="/post/2018-07-12-managing-earlier-stock-movement_files/figure-html/plots-2.png" width="672" />

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

ggplot(early_better_pct, aes(x = symbol, y = freq)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Percent when price change is less after 22 days than 45",
       x = "Symbol", y = "Percentage")
```

<img src="/post/2018-07-12-managing-earlier-stock-movement_files/figure-html/unnamed-chunk-1-1.png" width="672" />

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
  dplyr::distinct(symbol, avg_mid, avg_close, max_mid, max_close,
                  min_mid, min_close, sd_mid, sd_close) %>%
  dplyr::arrange(symbol)

knitr::kable(return_metrics, digits = 2, format = "html",
             caption = "Percentage Returns and Standard Deviation",
             col.names = c("SYM", "MID", "CLOSE", "MID", "CLOSE",
                           "MID", "CLOSE", "MID", "CLOSE"),
             escape = FALSE, 
             align = c('l', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r')) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", 
                            full_width = FALSE) %>%
  kableExtra::add_header_above(., c(" ", "  AVG" = 2, "  MAX" = 2,
                                    "  MIN" = 2, "  SD" = 2)) %>%
  kableExtra::column_spec(., 1:9, width = "1.0in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Percentage Returns and Standard Deviation</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">  AVG</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">  MAX</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">  MIN</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">  SD</div></th>
</tr>
  <tr>
   <th style="text-align:left;"> SYM </th>
   <th style="text-align:right;"> MID </th>
   <th style="text-align:right;"> CLOSE </th>
   <th style="text-align:right;"> MID </th>
   <th style="text-align:right;"> CLOSE </th>
   <th style="text-align:right;"> MID </th>
   <th style="text-align:right;"> CLOSE </th>
   <th style="text-align:right;"> MID </th>
   <th style="text-align:right;"> CLOSE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 1.0in; "> DIA </td>
   <td style="text-align:right;width: 1.0in; "> 0.935% </td>
   <td style="text-align:right;width: 1.0in; "> 1.4% </td>
   <td style="text-align:right;width: 1.0in; "> 8.54% </td>
   <td style="text-align:right;width: 1.0in; "> 10% </td>
   <td style="text-align:right;width: 1.0in; "> -6.24% </td>
   <td style="text-align:right;width: 1.0in; "> -10.7% </td>
   <td style="text-align:right;width: 1.0in; "> 2.86% </td>
   <td style="text-align:right;width: 1.0in; "> 4.05% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EEM </td>
   <td style="text-align:right;width: 1.0in; "> 0.298% </td>
   <td style="text-align:right;width: 1.0in; "> 0.418% </td>
   <td style="text-align:right;width: 1.0in; "> 10.1% </td>
   <td style="text-align:right;width: 1.0in; "> 13.2% </td>
   <td style="text-align:right;width: 1.0in; "> -12.6% </td>
   <td style="text-align:right;width: 1.0in; "> -17.3% </td>
   <td style="text-align:right;width: 1.0in; "> 4.19% </td>
   <td style="text-align:right;width: 1.0in; "> 6.69% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> EWZ </td>
   <td style="text-align:right;width: 1.0in; "> -0.148% </td>
   <td style="text-align:right;width: 1.0in; "> 0.0395% </td>
   <td style="text-align:right;width: 1.0in; "> 19.1% </td>
   <td style="text-align:right;width: 1.0in; "> 34.8% </td>
   <td style="text-align:right;width: 1.0in; "> -14.8% </td>
   <td style="text-align:right;width: 1.0in; "> -22.1% </td>
   <td style="text-align:right;width: 1.0in; "> 6.91% </td>
   <td style="text-align:right;width: 1.0in; "> 11.6% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> FXE </td>
   <td style="text-align:right;width: 1.0in; "> -0.244% </td>
   <td style="text-align:right;width: 1.0in; "> -0.15% </td>
   <td style="text-align:right;width: 1.0in; "> 3.5% </td>
   <td style="text-align:right;width: 1.0in; "> 6.34% </td>
   <td style="text-align:right;width: 1.0in; "> -6.66% </td>
   <td style="text-align:right;width: 1.0in; "> -7.28% </td>
   <td style="text-align:right;width: 1.0in; "> 2.12% </td>
   <td style="text-align:right;width: 1.0in; "> 2.83% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> GLD </td>
   <td style="text-align:right;width: 1.0in; "> -0.192% </td>
   <td style="text-align:right;width: 1.0in; "> -0.453% </td>
   <td style="text-align:right;width: 1.0in; "> 8.9% </td>
   <td style="text-align:right;width: 1.0in; "> 14.3% </td>
   <td style="text-align:right;width: 1.0in; "> -12.7% </td>
   <td style="text-align:right;width: 1.0in; "> -15.3% </td>
   <td style="text-align:right;width: 1.0in; "> 4.37% </td>
   <td style="text-align:right;width: 1.0in; "> 5.63% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> IWM </td>
   <td style="text-align:right;width: 1.0in; "> 1.13% </td>
   <td style="text-align:right;width: 1.0in; "> 1.6% </td>
   <td style="text-align:right;width: 1.0in; "> 14.5% </td>
   <td style="text-align:right;width: 1.0in; "> 16.1% </td>
   <td style="text-align:right;width: 1.0in; "> -6.41% </td>
   <td style="text-align:right;width: 1.0in; "> -16.5% </td>
   <td style="text-align:right;width: 1.0in; "> 3.95% </td>
   <td style="text-align:right;width: 1.0in; "> 5.53% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> QQQ </td>
   <td style="text-align:right;width: 1.0in; "> 1.4% </td>
   <td style="text-align:right;width: 1.0in; "> 1.88% </td>
   <td style="text-align:right;width: 1.0in; "> 10.3% </td>
   <td style="text-align:right;width: 1.0in; "> 11.5% </td>
   <td style="text-align:right;width: 1.0in; "> -7.18% </td>
   <td style="text-align:right;width: 1.0in; "> -12.4% </td>
   <td style="text-align:right;width: 1.0in; "> 3.38% </td>
   <td style="text-align:right;width: 1.0in; "> 4.87% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> SPY </td>
   <td style="text-align:right;width: 1.0in; "> 0.98% </td>
   <td style="text-align:right;width: 1.0in; "> 1.42% </td>
   <td style="text-align:right;width: 1.0in; "> 7.99% </td>
   <td style="text-align:right;width: 1.0in; "> 6.99% </td>
   <td style="text-align:right;width: 1.0in; "> -6.14% </td>
   <td style="text-align:right;width: 1.0in; "> -10.9% </td>
   <td style="text-align:right;width: 1.0in; "> 2.72% </td>
   <td style="text-align:right;width: 1.0in; "> 3.95% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> TLT </td>
   <td style="text-align:right;width: 1.0in; "> 0.126% </td>
   <td style="text-align:right;width: 1.0in; "> 0.154% </td>
   <td style="text-align:right;width: 1.0in; "> 5.99% </td>
   <td style="text-align:right;width: 1.0in; "> 10.4% </td>
   <td style="text-align:right;width: 1.0in; "> -7.78% </td>
   <td style="text-align:right;width: 1.0in; "> -12.6% </td>
   <td style="text-align:right;width: 1.0in; "> 2.99% </td>
   <td style="text-align:right;width: 1.0in; "> 4.67% </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 1.0in; "> XLE </td>
   <td style="text-align:right;width: 1.0in; "> 0.0403% </td>
   <td style="text-align:right;width: 1.0in; "> 0.0239% </td>
   <td style="text-align:right;width: 1.0in; "> 11.3% </td>
   <td style="text-align:right;width: 1.0in; "> 11.4% </td>
   <td style="text-align:right;width: 1.0in; "> -10.2% </td>
   <td style="text-align:right;width: 1.0in; "> -20.7% </td>
   <td style="text-align:right;width: 1.0in; "> 4.51% </td>
   <td style="text-align:right;width: 1.0in; "> 6.92% </td>
  </tr>
</tbody>
</table>
