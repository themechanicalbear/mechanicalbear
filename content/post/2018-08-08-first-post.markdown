---
title: tastytrade - rstats - options...
author: Jason Taylor
date: '2018-08-08'
slug: first-post
draft: FALSE
categories:
  - Original
tags:
  - tastytrade
  - 'rstats'
  - options
description: 'So, here is the plan...'
topics: [first post, news]
---

## Goals & Objectives  
I have been an options trader and follower of tastytrade research and methods since 2012. For the past few years, I have backtested trading ideas to learn investment strategies and improve my skills in rstats and data analysis. This blog will chronicle those interests and developments. 

<!--more-->

As a student of tastytrade, I've learned how to sell premium in highly liquid underlyings with elevated volatility. If you have been lucky enough to follow their data science team's *Market Measures* content you should be able to follow along with this blog nicely. If this is new to you, or are here more for the #rstats, that is great too, welcome! 

I lead a weekly investment club at my work where we discuss options trading strategies and use tastytrade research as an education tool. Our group often has questions about what would happen if a 'Market Measure' was extended to include more underlyings or if multiple segments or ideas were combined. As I build out my #rstats code to make these tests easier, we will attempt to answer some of those questions. The research contained on this blog is meant to be educational and should not be used as trading advice or in any way as criticism of the tastytrade research it's based on. The changes to those studies always begin with sincere appreciation and lead us toward greater understanding, and learning.

So what content can you expect?...

## Posts types:

#### * Function and package development with #rstats. This includes the tasytrade package on [github](https://github.com/themechanicalbear/tastytrade)  

Example: This function takes in an option chain dataframe, target delta, and put/call type and returns a dataframe of daily shorts  

*These functions will be updated as new inputs are used in studies and new techniques and learned. Several improvements have already been made including using quasiquotation from rlang, moving the data to AWS redshift, and utilizing the purrr package*  


```r
#' @export
open_short <- function(data, tar_delta, put_call) {
  tp <- paste0(put_call, "_type")
  s <- paste0(put_call, "_strike")
  ds <- paste0(put_call, "_delta_strike")
  oc <- paste0(put_call, "_open_credit")
  
  data %>%
    filter(type == put_call) %>%
    mutate(abs_delta = abs(delta_strike - tar_delta)) %>%
    group_by(quotedate) %>%
    filter(m_dte == min(m_dte, na.rm = TRUE)) %>%
    filter(abs_delta == min(abs_delta, na.rm = TRUE)) %>%
    ungroup() %>%
    collect() %>%
    mutate(!!tp := type,
           !!s := strike,
           !!ds := delta_strike,
           !!oc := mid,
           quotedate = as.Date(quotedate, origin = "1970-01-01"),
           expiration = as.Date(expiration, origin = "1970-01-01")) %>%
    select(symbol, quotedate, !!tp, expiration, !!s, !!ds, dte, !!oc)
}
```

#### * AWS related code: S3, redshift, connections  

Example: This function takes in connection, credential, and format details to copy data from S3 in CSV format into a redshift table  


```r
#' @export
# Copy data from S3 and append to redshift
copy_S3_redshift <- 
  function(connection, table_name, bucket_path,
           credentials = Sys.getenv("TASTYTRADE_REDSHIFT_IAM_ROLE"),
           role =  Sys.getenv("TASTYTRADE_REDSHIFT_S3_ROLE"),
           delimiter = ",",
           region= Sys.getenv("TASTYTRADE_REDSHIFT_REGION"),
           ignore_header = "1",
           dateformat = "auto",
           null = "NA",
           file_format = "csv") {
    
    RJDBC::dbSendUpdate(connection, 
                        paste0("COPY ", table_name, " FROM '", bucket_path,
                               "'credentials '", credentials, ":role/", role,
                               "' delimiter '", delimiter, "' region '",
                               region, "' IGNOREHEADER ", ignore_header,
                               " dateformat '", dateformat, "' NULL '", null,
                               "' ", file_format, ";"))
  }
```

#### * Recreation of tastytrade *Market Measure* research  

Each weekday the tastytrade research team delivers a backtesting study. These studies provide a wealth of research ideas and opportunities to learn. Posts of this type will be recreations of *Market Measure* research with the data I have available. I hope to add more stocks to expand the entry and exit criteria and experiment with new visuals as I learn them.  

#### * Original trading strategy research  

Original research ideas will include posts which will act as an original *Market Measure*. These can also add the creation of technical indicators, downloading data from the web, and others.

*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
