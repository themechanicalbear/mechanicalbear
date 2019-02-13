---
title: Using R with AWS Athena
author: Jason Taylor
date: '2019-02-08'
slug: using-r-with-aws-athena
categories:
  - Original
draft: FALSE
tags:
  - AWS
  - Original Content
  - rstats
topics: []
description: ''
---

### Summary:

> Complete guide to setting up AWS S3, and Athena for use with #RSTATS

<!--more-->

<br>

***

<center><h2>AWS User Account Creation:</h2></center>

***

<br>

### User Account Step 1:

* Log into AWS site and navigate to the IAM Management Console found under the (Security, Identity, & Compliance) section
* Next goto Users and Choose Add User
* Enter a name for the new user and select "Programmatic access" type

<br>

![Alt text] (/img/add_user_step_1.png)

<br>

### User Account Step 2:

> Create a group so that you can assign the same permissions to additional users in the future

<br>

![Alt text] (/img/add_user_step_2.png)

<br>

### User Account Step 3:

* In this example I created a group named "blogger"
* Assign the following roles
  + AWSQuicksightAthenaAccess
  + AmazonS3FullAccess
  + AmazonAthenaFullAccess

<br>

![Alt text] (/img/add_user_step_3.png)

<br>

### User Account Step 4:

> Make sure to add the new user to the group you just created

![Alt text] (/img/add_user_step_4.png)

<br>

### User Account Step 5:

* The last step #5 shows that you successfully created the new user and provides you with the accounts Access key ID and Secret access key
* <b>Write these down the access key will be available in the user account details screen but the secret key is only shown here and will need to be reset once you leave this screen if you loose it.</b>

<br>

![Alt text] (/img/add_user_step_5.png)

<br>

***

<center><h2>AWS S3 bucket creation:</h2></center>

***

<br>

### S3 Bucket Step 1:

* Navigate to the S3 Console found under the (Storage) section
* Choose create bucket

![Alt text] (/img/s3_bucket_step_1.png)

<br>

### S3 Bucket Step 2:

* Give the bucket a unique and informative name
* Take note of the region you are working in in this case US West Oregon
* For the R configuration later this is "us-west-2"

![Alt text] (/img/s3_bucket_step_2.png)

<br>

### S3 Bucket Step 3:

> You can choose versioning, logs, etc, but I've left them all blank for this example

![Alt text] (/img/s3_bucket_step_3.png)

<br>

### S3 Bucket Step 4:

> Enable the 4 check boxes for keeping the bucket private if they are not already set

![Alt text] (/img/s3_bucket_step_4.png)

<br>

### S3 Bucket Step 5:

> Review the settings you chose and click Create bucket to complete the process

![Alt text] (/img/s3_bucket_step_5.png)

<br>

### S3 Bucket Step 6:

> Back on the S3 console you will now see the new bucket including that it is private, and in the US West region

![Alt text] (/img/s3_bucket_step_6.png)

<br>

***

<center><h2>Let's get to the fun part in R:</h2></center>

***
<br>

* For the remainder of this post we will use R to:
  + create a csv file
  + connect to AWS and populate the S3 bucket we just created with the csv
  + load the csv we added to S3 into a table in Athena
  + then pull that data out of Athena back into R
  
* We will use the following packages:
  + tidyverse (CRAN)
  + here (CRAN)
  + knitr (CRAN)
  + kableExtra (CRAN)
  + mobstr (github) remotes::install_github("themechanicalbear/mobstr")
  
<br>


```r
knitr::opts_chunk$set(message = FALSE, eval = FALSE, 
                      tidy.opts = list(width.cutoff = 100))
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("tidyverse", "here", "mobstr", "knitr", "kableExtra")
  lapply(library_list, require, character.only = TRUE)})))
```

<br>

***

<center><h2>Add AWS Credentials to .Rprofile:</h2></center>

***

<br>

* Open your .Rprofile and add the following with the values specific to your account we created above.

* This will allow your scripts to connect to AWS resources without having to include your keys in the scripts themselves

<br>


```r
Sys.setenv("AWS_ACCESS_KEY_ID" = "YOUR_AWS_ACCESS_KEY_ID",
           "AWS_SECRET_ACCESS_KEY" = "YOUR_AWS_SECRET_ACCESS_KEY",
           "AWS_DEFAULT_REGION" = "us-west-2") # Whatever region your S3 bucket you created is in
```

<br>

* The <b>mobstr</b> package includes the <b>write_2_S3</b> function to write stock data to AWS S3 for analysis. 
  + We can use this function to write any csv file we want.
  + First let's create a sample csv to upload
  + Here I am saving this file to the static data directory to be included in the blog folder structure you don't need to do that
  + Let's use the <b>mtcars</b> dataset to create a sample csv to upload

> <b>*Athena requires there only be one csv per folder/bucket so the write_2_S3 function will create a new folder inside our blog-s3-test-example bucket to place our csv inside*</b>

<br>


```r
readr::write_csv(mtcars, paste0(here::here(), "/static/data/mtcars.csv"))

mobstr::write_2_S3(file = "mtcars", 
                   path = paste0(here::here(), "/static/data/"),
                   bucket = "blog-s3-test-example")
```

<br>

***

<center><h2>Check AWS S3 for our file!</h2></center>

***

<br>

![Alt text] (/img/file_upload_complete.png)

<br>

***

<center><h2>Create the table in Athena</h2></center>

***

<br>

* The <b>mobstr</b> package includes the <b>athena_connect</b> function to connect to the Athena service providing the database name. We are going to use the default database here.
* The <b>mobstr</b> package also includes the <b>athena_load</b> function which will use the connection to populate out table
* <b>athena_load</b> uses the following arguments:
  + <b>conn</b> - The odbc connection created with athena_connect
  + <b>database</b> - The database name
  + <b>s3_bucket</b> - The name of our S3 bucket
  + <b>name</b> - The name we want to assign to the table we are creating
  + <b>df</b> - The dataframe name we are extracting the column names and types from  
  *<b>(if you don't have mtcars available in your environment you need to load it with data(mtcars))</b>* or read in whatever csv you loaded to S3 instead so they match

<br>


```r
athena <- mobstr::athena_connect("default")

mobstr::athena_load(athena, "default", "blog-s3-test-example", "mtcars", mtcars)
```

<br>

#### mobstr Function Code: 

* This package is in development as Mechanical Options Backtesting System w/ Tidy R (mobstr) so I'll include the code here so that you will not need to load the package to follow this process.


```r
# mobstr::write_2_S3
write_2_S3 <- function(file, path, bucket) {
  file_path <- paste0(path, file, ".csv")
  file_name <- paste0(file, ".csv")
  bucket_name <- paste0(bucket, "/", file)
  region_name <- Sys.getenv("AWS_S3_REGION")

  aws.s3::put_bucket(bucket_name)

  aws.s3::put_object(file = file_path,
                     object = file_name,
                     bucket = bucket_name,
                     region = region_name)
}

# mobstr::athena_connect
athena_connect <- function(database) {
  DBI::dbConnect(
    odbc::odbc(),
    driver = Sys.getenv("AWS_ATHENA_DRIVER_FILE"),
    Schema = database,
    AwsRegion = Sys.getenv("AWS_DEFAULT_REGION"),
    UID = Sys.getenv("AWS_ACCESS_KEY_ID"),
    PWD = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    S3OutputLocation = Sys.getenv("AWS_ATHENA_S3_OUTPUT")
  )
}

# mobstr::athena_load
athena_load <- function(conn, database, s3_bucket, name, df) {
  table_vars <- paste0("`", names(df), "` ", sapply(df, typeof), ",", collapse = '')
  table_vars <- sub(",$", "", table_vars)

  # SQL create table statement in Athena
  DBI::dbExecute(conn, paste0("CREATE EXTERNAL TABLE IF NOT EXISTS ",
                              database, ".", name, " (", table_vars, " )",
                              "ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.lazy.LazySimpleSerDe'",
                              "WITH SERDEPROPERTIES ('serialization.format' = ',', 'field.delim' = ',' )",
                              "LOCATION 's3://", s3_bucket, "/", name, "/'",
                              "TBLPROPERTIES ('has_encrypted_data'='false', 'skip.header.line.count'='1');"))
}
```

<br>

***

<center><h2>That's It, did it work?.....</h2></center>

***

<br>

> We can now connect read the table and bring the data back into R and View

<br>


```r
athena_cars <- mobstr::athena_connect("default") %>%
  dplyr::tbl("mtcars") %>%
  dplyr::filter(mpg >= 25) %>%
  dplyr::collect()

knitr::kable(athena_cars, digits = 2, format = "html",
             caption = "Athena Cars",
             col.names = c("MPG", "CYL", "DISP", "HP", "DRAT", "WT", "QSEC",
                           "VS", "AM", "GEAR", "CARB"),
             escape = FALSE,
             align = rep("r", 11)) %>%
  kableExtra::kable_styling(., bootstrap_options = "striped", position = "center",
                            full_width = FALSE) %>%
  kableExtra::column_spec(., 1:11, width = "1.5in")
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 1 Athena Cars</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> MPG </th>
   <th style="text-align:right;"> CYL </th>
   <th style="text-align:right;"> DISP </th>
   <th style="text-align:right;"> HP </th>
   <th style="text-align:right;"> DRAT </th>
   <th style="text-align:right;"> WT </th>
   <th style="text-align:right;"> QSEC </th>
   <th style="text-align:right;"> VS </th>
   <th style="text-align:right;"> AM </th>
   <th style="text-align:right;"> GEAR </th>
   <th style="text-align:right;"> CARB </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 32.4 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 78.7 </td>
   <td style="text-align:right;width: 1.5in; "> 66 </td>
   <td style="text-align:right;width: 1.5in; "> 4.08 </td>
   <td style="text-align:right;width: 1.5in; "> 2.20 </td>
   <td style="text-align:right;width: 1.5in; "> 19.47 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 30.4 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 75.7 </td>
   <td style="text-align:right;width: 1.5in; "> 52 </td>
   <td style="text-align:right;width: 1.5in; "> 4.93 </td>
   <td style="text-align:right;width: 1.5in; "> 1.62 </td>
   <td style="text-align:right;width: 1.5in; "> 18.52 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 33.9 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 71.1 </td>
   <td style="text-align:right;width: 1.5in; "> 65 </td>
   <td style="text-align:right;width: 1.5in; "> 4.22 </td>
   <td style="text-align:right;width: 1.5in; "> 1.84 </td>
   <td style="text-align:right;width: 1.5in; "> 19.90 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 27.3 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 79.0 </td>
   <td style="text-align:right;width: 1.5in; "> 66 </td>
   <td style="text-align:right;width: 1.5in; "> 4.08 </td>
   <td style="text-align:right;width: 1.5in; "> 1.94 </td>
   <td style="text-align:right;width: 1.5in; "> 18.90 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 26.0 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 120.3 </td>
   <td style="text-align:right;width: 1.5in; "> 91 </td>
   <td style="text-align:right;width: 1.5in; "> 4.43 </td>
   <td style="text-align:right;width: 1.5in; "> 2.14 </td>
   <td style="text-align:right;width: 1.5in; "> 16.70 </td>
   <td style="text-align:right;width: 1.5in; "> 0 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 5 </td>
   <td style="text-align:right;width: 1.5in; "> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;width: 1.5in; "> 30.4 </td>
   <td style="text-align:right;width: 1.5in; "> 4 </td>
   <td style="text-align:right;width: 1.5in; "> 95.1 </td>
   <td style="text-align:right;width: 1.5in; "> 113 </td>
   <td style="text-align:right;width: 1.5in; "> 3.77 </td>
   <td style="text-align:right;width: 1.5in; "> 1.51 </td>
   <td style="text-align:right;width: 1.5in; "> 16.90 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 1 </td>
   <td style="text-align:right;width: 1.5in; "> 5 </td>
   <td style="text-align:right;width: 1.5in; "> 2 </td>
  </tr>
</tbody>
</table>

<br>

*If you have suggestions or any other feedback please reach out with the contact links on the sidebar*

#### Best,
#### Jason
