# Arbor -- Tree Visualization Tool

## Introduction

Arbor is a RShiny app to directly visualize tree data and its associated heat maps and bar plots. In order to visualize tree data in multiple aspects, users may upload tree data in .nwk, and heat map or bar plot data in .csv, and download the plots in either .png, .jpg, or .pdf. Hope you would find this app helpful!

## Example 1

After uploading data in Data Upload, select variables for X-aixs and Y-aixs for the plots and then click "Upload" to complete data uploading. Click "Back" to re-upload or upload another files. Users can upload as many .csv files as they want, but only one tree data is accepted.

Once the data are successfully uploaded, check Upload Status to see the total number of data uploaded.

Please check Plots for the result.

![](images/tree.nwk.jpg)

## Example 2

First, use the function "prepare_data.R" to prepare all the data in the correct format that ready for arbor.

```{r}
source('prepare_data.R')

ps<-load("ps.Rdata") # Tree data
bacteria <- read.csv("~/Documents/Tree/crc/eocrc.vs.ctrl.male.csv") # heat map and bar plot data
crc<-paste("crc") # File name

prepare_data(ps, bacteria, crc)
```

Then, upload the output files onto arbor.R in the same way as in example 1:

![](images/Screen%20Shot%202022-04-04%20at%2002.34.40-01.png)

Check Plots for the results:

![](images/crc%20.nwk-02.jpg)
