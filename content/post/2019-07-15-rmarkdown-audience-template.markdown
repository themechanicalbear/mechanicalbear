---
title: rmarkdown Audience Template
author: Jason Taylor
date: '2019-07-15'
slug: rmarkdown-audience-template
draft: FALSE
categories:
  - Original
tags:
  - rstats
  - rmarkdown
topics: []
description: ''
params:
    audience:
        label: "Audience"
        value: Customer
        input: select
        choices: [Analyst, Customer]
editor_options: 
  chunk_output_type: console
---

#### Objective:

When working on an rmarkdown project I often have different types of 'customers' for the output. Here is one way to handle those cases using parameters.   

<!--more-->

There are 3 parts to making this work:

1. The YAML for the rmarkdown needs to include a params section with output reciever types
2. An 'r chunk' that sets the default for subsequent chunks to have code/output shown
3. Apply the appropriate logic for echo to each chunk (default here is FALSE)

### Step 1: (YAML)

Our audience parameter has:

* label of 'Audience'
* default value of Customer
* input type of select
* choices of (Analyst, Customer)



```r
title: rmarkdown Audience Template  
author: Jason Taylor  
date: '2019-07-15'  
slug: rmarkdown-audience-template    
draft: FALSE  
categories:  
  - Original  
tags:  
  - rstats  
  - rmarkdown  
topics: []  
description: ''  
params:  
  audience:  
    label: "Audience"  
    value: Customer  
    input: select  
    choices: [Analyst, Customer]  
editor_options:   
  chunk_output_type: console
```

### Step 2: (Chunk defaults)

#### Load Libraries and Setup Global Options:

* knitr::opts_chunk$set 'echo' defines if a chunk should print the code when rendered.
* Here we set the default to FALSE by: 
  + default audience parameter above as 'Customer'
  + if_else statement in the setup chunk defining echo to be TRUE when the 'Analyst' type is chosen


```r
library.list <- c("tidyverse", "mobstr", "here")
lapply(library.list, require, character.only = TRUE)

knitr::opts_chunk$set(
  echo = if_else(params$audience == "Analyst", TRUE, FALSE),
  warning = if_else(params$audience == "Analyst", TRUE, FALSE),
  include = TRUE,
  comment = NA)
```

### Step 3: (Apply)

* By default 'r chunks' will not echo their code since the paramater defaults to 'Customer'
* If you want to show all 'r chunks' choose 'Analyst' Audience type when you knit
* If you want to echo a specific 'r chunk' regardless of the parameter selection during knit you can use the chunk option echo = TRUE like so... (# added to show the chunk begin and end)

#### Example 'r chunk' with 'echo = TRUE'


```r
# ```{r echo = TRUE}
head(mtcars)
```

```
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

```r
# ```
```


*If you have suggestions for studies, improvements for rstats code, or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
