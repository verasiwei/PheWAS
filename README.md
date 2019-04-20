# PheWASExtension...

This is an extension of PheWAS package. It will save a lot of time and be much more efficient for different kinds of data pre-processing/cleaning and arrangement which satisfied different pre-requirements. See the attached description for each function.

## Installation

```
$ git clone https://github.com/verasiwei/PheWASExtension
```

Then install the required packages in your environment

```
packages <- c("PheWAS","Matrix","devtools","dplyr",
              "tidyr","ggplot2","parallel","MASS",
              "meta","ggrepel","DT","MatchIt","spam",
              "readxl","shiny")

install.packages(packages)

library(devtools)
library(PheWAS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(MASS)
library(meta)
library(ggrepel)
library(DT)
library(Matrix)
library(MatchIt)
library(spam)
library(readxl)
library(shiny)

```

## Phenotype Data
A sparse matrix that rows should be individuals and columns are the Phecodes. It does not matter when there is an initial label of the phecodes in your table. The functions will check this. 

## Demographic Data
A dataframe that should at least have the column of `id` and some other covariates.

## Status Data
A dataframe that should have the column of `id` and the corresponding case/control status.

## PheWAS Plot
A simple plot shiny app to have a look of different phewas results

![Alt text](https://github.com/tbilab/PheWASExtension/blob/master/phewas_example.png)




