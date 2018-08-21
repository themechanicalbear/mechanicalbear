---
title: "{{ replace .TranslationBaseName "-" " " | title }}"
date: "{{ .Date }}"
tags: []
topics: []
description: ""
---

#### Study:



<!--more-->



#### Results:



#### Setup global options, load libraries:

```{r global_options, include = TRUE, results = FALSE}
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

```{r study}

```

#### Run Study

```{r run study}

```



*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
