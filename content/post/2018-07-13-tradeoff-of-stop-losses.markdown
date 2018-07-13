---
title: Tradeoff of Stop Losses
author: Jason Taylor
date: '2018-07-13'
slug: tradeoff-of-stop-losses
summary: Stop losses are a popular strategy used by traders to achieve better performance
  by closing out of a losing trade. But do they work
categories:
  - Tastytrade
tags:
  - Strangles
  - Stop Loss
header:
  caption: ''
  image: ''
---

### Market Measures - July 10, 2018

The setup - Montly SPY 16 delta strangles, 45DTE, hold to expiration, 1-5x loss of credit recieved

*Stop losses are a popular strategy used by traders to achieve better performance by closing out of a losing trade.*

*tastytrade ran a study where they compared managing 16 delta strangles in SPY, 45 DTE at expiration and 1x-5x credit received losses. They found that on average, stop losses hurt performance over the long run, because most of the trades you stopped out of eventually had a better P/L at expiration.*

Here I will recreate this study and extend it to include more underlyings to practice using the purrr package. This is the 
first attempt at recreating a Market Measure study and will be the basis of the tastytrade package on  [github](https://github.com/themechanicalbear/tastytrade).

#### Setup global options, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tastytrade", "dplyr", "ggplot2", "plotly", "gridExtra")
  lapply(library_list, require, character.only = TRUE)})))

stock_list <- c("SPY", "IWM", "GLD", "QQQ", "DIA", "TLT", "XLE", "EEM",
                "MA", "FB", "FXI", "SLV", "EWZ", "FXE", "TBT", "IBM")
tar_dte <- 45
tar_delta_put <- -.16
tar_delta_call <- .16
all_loss_table <- data.frame()
all_results <- data.frame()
```

#### Study function


```r
study <- function(stock) {
  options <- readRDS(paste0(here::here(), "/data/options/", stock, ".RDS")) %>%
    dplyr::mutate(mid = (bid + ask) / 2)
  monthly <- readRDS(paste0(here::here(), "/data/monthly.RDS"))
  
  options_filtered <- options %>%
    dplyr::filter(quotedate %in% monthly$date) %>%
    dplyr::mutate(m_dte = abs(dte - tar_dte))
  
  short_put_opens <- tastytrade::open_short_put(options_filtered, stock, 
                                                tar_delta_put)
  short_call_opens <- tastytrade::open_short_call(options_filtered, stock,
                                                  tar_delta_call)
  
  all_trades <- dplyr::full_join(short_call_opens, short_put_opens, 
                                 by = c("quotedate", "expiration", "dte")) %>%
    dplyr::mutate(credit = mid_put + mid_call)
  
  all_closes <- data.frame()
  
  possible_closes <- function(date, exp, c_strike, p_strike, credit) {
    closes <- options %>%
      dplyr::filter(quotedate > date,
                    quotedate <= exp,
                    expiration == exp) %>%
      dplyr::filter((strike == c_strike & type == "call") |
                      (strike == p_strike & type == "put")) %>%
      dplyr::group_by(quotedate) %>%
      dplyr::mutate(open_date = as.Date(date, origin = "1970-01-01"),
                    open_credit = credit,
                    debit = sum(mid),
                    profit = open_credit - debit,
                    loss_1_x = ifelse(debit >= 2 * credit, 1, 0),
                    loss_2_x = ifelse(debit >= 3 * credit, 1, 0),
                    loss_3_x = ifelse(debit >= 4 * credit, 1, 0),
                    loss_4_x = ifelse(debit >= 5 * credit, 1, 0),
                    loss_5_x = ifelse(debit >= 6 * credit, 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::select(symbol, quotedate, expiration, open_date, open_credit,
                    debit, profit, loss_1_x, loss_2_x, loss_3_x, loss_4_x,
                    loss_5_x) %>%
      dplyr::distinct()
    
    all_closes <<- rbind(all_closes, closes)
  }
  
  invisible(purrr::pmap(list(all_trades$quotedate, all_trades$expiration,
                             all_trades$strike_call, all_trades$strike_put, 
                             all_trades$credit), possible_closes))
  
  invisible(purrr::pmap(list(df = list(all_closes), 
                             col_name = list("loss_1_x", "loss_2_x", "loss_3_x",
                                             "loss_4_x", "loss_5_x")),
                        tastytrade::stop_loss))
  
  expiration <- all_closes %>%
    dplyr::group_by(open_date) %>%
    dplyr::filter(quotedate == expiration) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(quotedate) %>%
    dplyr::mutate(portfolio = cumsum(profit) * 100,
                  loss_type = "expiration")
  
  symbol_results <- dplyr::bind_rows(loss_1_x, loss_2_x) %>%
    dplyr::bind_rows(loss_3_x) %>%
    dplyr::bind_rows(loss_4_x) %>%
    dplyr::bind_rows(loss_5_x) %>%
    dplyr::bind_rows(expiration)
  
  all_results <- rbind(all_results, symbol_results)
  assign("all_results", all_results, envir = .GlobalEnv)
  
  this_loss_table <- dplyr::bind_rows(loss_1_x, loss_2_x) %>%
    dplyr::bind_rows(loss_3_x) %>%
    dplyr::bind_rows(loss_4_x) %>%
    dplyr::bind_rows(loss_5_x) %>%
    dplyr::bind_rows(expiration) %>%
    dplyr::group_by(loss_type) %>%
    dplyr::filter(open_date == max(open_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rank = rank(-portfolio)) %>%
    dplyr::select(symbol, loss_type, rank) %>%
    tidyr::spread(., key = loss_type, value = rank)
  
  all_loss_table <- rbind(all_loss_table, this_loss_table) 
  assign("all_loss_table", all_loss_table, envir = .GlobalEnv)
}
```

#### Run the study with purrr::map on each symbol

```r
invisible(purrr::map(stock_list, study))
```

#### Group returns to similar portfolio outcomes and split so the plots are not as large


```r
group_one_returns <- all_results %>%
  dplyr::filter(symbol %in% c("EWZ", "TLT", "SLV", "FXI", "XLE", "EEM", "FXE"))
group_two_returns <- all_results %>%
  dplyr::filter(symbol %in% c("GLD", "QQQ", "DIA", "IWM", "IBM", "SPY", "MA"))

grouped_plot <- function(df) {
  ggplot(data = df, aes(x = quotedate, y = portfolio)) +
    geom_line(data = dplyr::filter(df, loss_type == "loss_1_x"), 
              aes(group = loss_type, color = "1X Stop")) +
    geom_line(data = dplyr::filter(df, loss_type == "loss_2_x"),
              aes(color = "2X Stop")) +
    geom_line(data = dplyr::filter(df, loss_type == "loss_3_x"),
              aes(color = "3X Stop")) +
    geom_line(data = dplyr::filter(df, loss_type == "loss_4_x"),
              aes(color = "4X Stop")) +
    geom_line(data = dplyr::filter(df, loss_type == "loss_5_x"),
              aes(color = "5X Stop")) +
    geom_line(data = dplyr::filter(df, loss_type == "expiration"),
              aes(color = "expiration")) +
    scale_fill_brewer() +
    theme_dark() + 
    labs(title = "Portfolio Total Return (by stop loss)", x = "Trade Open Date",
         y = "Portfolio Value") +
    facet_grid(rows = vars(symbol), scales = "free_y")
}

grouped_plot(group_one_returns)
```

<img src="/post/2018-07-13-tradeoff-of-stop-losses_files/figure-html/ggplots-1.png" width="768" />

```r
grouped_plot(group_two_returns)
```

<img src="/post/2018-07-13-tradeoff-of-stop-losses_files/figure-html/ggplots-2.png" width="768" />

#### This heat map shows the outcomes by stop loss type the darker the color the better the outcome  
These are ranked from (1-6) 1 being best  
On average holding to expiration performed best and stopping out too early performed the worst as seen in the mean total row at the top.


```r
heat_map_data <- all_loss_table %>%
  dplyr::bind_rows(summarise_all(., funs(if (is.numeric(.)) mean(.) else "Mean Total"))) %>%
  tibble::remove_rownames(.) %>%
  tibble::column_to_rownames(var = "symbol")
heat_map_data <- as.matrix(heat_map_data)

plot_ly(x = colnames(heat_map_data), y = rownames(heat_map_data), 
        z = heat_map_data, type = "heatmap", 
        colors = colorRamp(c("red", "yellow")))
```

<!--html_preserve--><div id="a9296c48ad78" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="a9296c48ad78">{"x":{"visdat":{"a9297f32613":["function () ","plotlyVisDat"]},"cur_data":"a9297f32613","attrs":{"a9297f32613":{"x":["expiration","loss_1_x","loss_2_x","loss_3_x","loss_4_x","loss_5_x"],"y":["SPY","IWM","GLD","QQQ","DIA","TLT","XLE","EEM","MA","FB","FXI","SLV","EWZ","FXE","TBT","IBM","Mean Total"],"z":[[1,6,3,4,5,2],[3,6,5,4,2,1],[2,6,3,1,4,5],[1,6,5,4,3,2],[1,6,3,4,5,2],[3,1,5,6,2,4],[2,5,6,4,1,3],[1.5,5,6,3,4,1.5],[1,6,5,4,3,2],[3,5,6,4,1.5,1.5],[1,2,5,6,3,4],[1,5,6,4,2,3],[4,2,1,3,5,6],[2,4,3,6,5,1],[5,1,3,6,2,4],[2,6,3,5,4,1],[2.09375,4.5,4.25,4.25,3.21875,2.6875]],"colors":["function (x) ","roundcolor(cbind(palette[[1L]](x), palette[[2L]](x), palette[[3L]](x), ","    if (alpha) palette[[4L]](x))) * 255"],"alpha":1,"sizes":[10,100],"type":"heatmap"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1]},"yaxis":{"domain":[0,1]},"hovermode":"closest","showlegend":false,"legend":{"y":0.5,"yanchor":"top"}},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"colorbar":{"title":"","ticklen":2,"len":0.5,"y":1,"lenmode":"fraction","yanchor":"top"},"colorscale":[["0","rgba(255,0,0,1)"],["0.1","rgba(255,25,0,1)"],["0.2","rgba(255,51,0,1)"],["0.379166666666667","rgba(255,96,0,1)"],["0.4","rgba(255,102,0,1)"],["0.521875","rgba(255,133,0,1)"],["0.6","rgba(255,153,0,1)"],["0.65","rgba(255,165,0,1)"],["0.8","rgba(255,204,0,1)"],["1","rgba(255,255,0,1)"]],"showscale":true,"x":["expiration","loss_1_x","loss_2_x","loss_3_x","loss_4_x","loss_5_x"],"y":["SPY","IWM","GLD","QQQ","DIA","TLT","XLE","EEM","MA","FB","FXI","SLV","EWZ","FXE","TBT","IBM","Mean Total"],"z":[[1,6,3,4,5,2],[3,6,5,4,2,1],[2,6,3,1,4,5],[1,6,5,4,3,2],[1,6,3,4,5,2],[3,1,5,6,2,4],[2,5,6,4,1,3],[1.5,5,6,3,4,1.5],[1,6,5,4,3,2],[3,5,6,4,1.5,1.5],[1,2,5,6,3,4],[1,5,6,4,2,3],[4,2,1,3,5,6],[2,4,3,6,5,1],[5,1,3,6,2,4],[2,6,3,5,4,1],[2.09375,4.5,4.25,4.25,3.21875,2.6875]],"type":"heatmap","xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->
