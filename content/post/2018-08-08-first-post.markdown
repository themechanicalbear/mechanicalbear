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
  - '#rstats'
  - options
description: ''
topics: [first post, news]
---

## Goals & Objectives  
I have been an options trader and follower of tastytrade research and methods since 2012. For the past few years I have been backtesting trading ideas to learn investment strategies and improve my skills in rstats and data analysis. This blog will chronicle those interests and developments.  
<!--more-->
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
    dplyr::filter(type == put_call) %>%
    dplyr::mutate(abs_delta = abs(delta_strike - tar_delta)) %>%
    dplyr::group_by(quotedate) %>%
    dplyr::filter(m_dte == min(m_dte, na.rm = TRUE)) %>%
    dplyr::filter(abs_delta == min(abs_delta, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::collect() %>%
    dplyr::mutate(!!tp := type,
                  !!s := strike,
                  !!ds := delta_strike,
                  !!oc := mid,
                  quotedate = as.Date(quotedate, origin = "1970-01-01"),
                  expiration = as.Date(expiration, origin = "1970-01-01")) %>%
    dplyr::select(symbol, quotedate, !!tp, expiration, !!s, !!ds, dte, !!oc)
}
```

#### * AWS related code: S3, redshift, connections  

Example: This function takes in connection, credential, and format details to copy data from S3 in csv format into a redshift table  


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

Each weekday the tastytrade research team delivers a backtesting study. These studies provide a wealth of research ideas and opportunities to learn. Posts of this type will be recreations of *Market Measure* research with the data I have available. I hope to add more stocks expand the entry and exit criteria and experiment with new visuals as I learn them.  

#### * Original trading strategy research  

When I have original research ideas I will include posts on those which will act as an original *Market Measure*. These can also include creation of technical indicators, downloading data from the web, and others.

*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
