# Visualising Uncertainty

Website: [https://numbat-tutorials.github.io/tutorial_visualising_uncertainity/](https://numbat-tutorials.github.io/tutorial_visualising_uncertainity/)

From exploring variability in a dataset to communicating the distribution of estimates, 
uncertainty visualisation plays a role at every stage of data analysis. 
This tutorial provides an overview of uncertainty, examines how it is represented in R, 
and introduces a range of techniques for visualising it. The first session will introduce 
the concept of uncertainty and cover general visualisation approaches. The second session 
will focus on spatial data and the creative methods that emerge when uncertainty 
must be expressed using only a limited set of visual aesthetics. Wherever there are 
statistics and data, there is uncertainty, making the ability to visualise it effectively 
an essential skill for any statistician.


**Presenters**:

[Dianne Cook](https://www.dicook.org), a Professor of 
Statistics at Monash University in Melbourne, Australia, is a global leader
in data visualisation. She has delivered over 100 invited talks 
internationally and published extensively on various aspects of data 
visualisation. Dr. Cook is a Fellow of the American Statistical 
Association, an elected member of the International Statistical 
Institute, past editor of the Journal of Computational and Graphical 
Statistics, and the R Journal. She has served as a Board Member of the 
R Foundation and is currently the co-chair of the Statistical Computing 
and Visualisation Section of the [Statistical Society of Australia](https://www.statsoc.org.au).

[Harriet Mason](https://harrietmason.netlify.app/) is a final year PhD student. She is 
researching how uncertainty can be effectively communicated through statistical graphics, 
particularly in contexts where misleading interpretations must be actively avoided. 
She is the author of several R packages, including `cassowaryr` and `ggdibbler`.

**Background**: You should have a basic understanding of R to the level of working after reading 
[R4DS](https://r4ds.hadley.nz) and working knowledge of making plots with 
[ggplot2](https://ggplot2.tidyverse.org). Good background reading includes 
[Fundamentals of Data Visualization](https://clauswilke.com/dataviz/) and 
[Data Visualization: A practical introduction](https://socviz.co).

## Structure of tutorial

### Session 1: Foundations of uncertainty in data visualisations

This session will focus on introducing common ways of representing uncertainty in data plots, why it is important, and ways to assess new approaches.

| **Topic**               | **Description**                                                                                                        |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| **Introduction**        | What is uncertainty? What is the goal for visualisation |
| **Common measures and representations**      | Common approaches for including representation of uncertainty in data plots relative to the type of uncertainty measure: show the data, error bars, estimates, models |
| **How this affects perception**          | Including or omitting uncertainty representation can change the interpretation of information. Understand how this might affect communication and decisions. |
| **Uncertainty representation in different types of problems**             | Understanding and comparing univariate and bivariate distributions, temporal and spatial data, indexes, networks |
| **Deciding which is the best design**             | How to compare and test different designs in relation to information perceived |

### Session 2: Diving deeper into uncertainty visualisation using examples in spatial data

This session will focus specifically on uncertainty visualisation for spatial data. 
In spatial contexts, both the x and y axes are often already used, 
leaving less room for visualising uncertainty through conventional means. 
This session explores these challenges and introduces tools and techniques designed 
to incorporate uncertainty when visualisation options are limited.

| **Topic**                                                                | **Description**                                                                                                     |
| ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| **Introduction to spatial visualisation**                                    | Understand the unique challenges of visualising map data.  Learn why many newly developed techniques have a large focus on spatial visualisations. |
| **Hands on mapping example**                                        | Work through the initial stages of a visualisation task with special focus on understanding how to quantify uncertainty, and avoid losing it as you move through the stages of a data analysis. |
| **R packages for visualising uncertainty in maps**                                                         | Go through some popular packages for visualising spatial uncertainty in R. Understand the data format each package accepts, and see the various plot options.  |



## Getting started 

You should have a reasonably up to date version of R and R Studio, eg RStudio 2025.05.0+496 "Mariposa Orchid" and R version 4.5.0 (2025-04-11) "How About a Twenty-Six". Install these essential packages, and their dependencies.

```
install.packages(c("ggplot2", 
                   "tidyr", 
                   "dplyr", 
                   "readr", 
                   "readxl",
                   "stringr", 
                   "forcats",
                   "colorspace", 
                   "patchwork",
                   "broom", 
                   "ggbeeswarm",
                   "ggforce",
                   "ggdist",
                   "ggridges",
                   "ggdibbler",
                   "nullabor",
                   "distributional",
                   "sf",
                   "ggthemes",
                   "conflicted"), 
                   dependencies=c("Depends", "Imports"))
```

and to fully reproduce all the examples in the slides, also install these packages:

```
install.packages(c("rmapshaper",
                   "brolgar",
                   "modelr",
                   "cartogram",
                   "lubridate",
                   "fable",
                   "kableExtra",
                   "tsibble"), 
                   dependencies=c("Depends", "Imports"))
```

Copyright: Dianne Cook and Harriet Masonm 2025

These materials are licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg

