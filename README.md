# Batther et al. (2025) supplemental

This repository holds data and supplementary source code to reproduce calculations and plots in *Batther et al. (submitted)*.

## What can I do with this code? <a href="https://creativecommons.org/licenses/by/4.0/"><img src="https://mirrors.creativecommons.org/presskit/buttons/88x31/png/by.png" align = "right" width = "100"/></a>

We hope that this code, or any part of it, might prove useful to other members of the scientific community interested in the subject matter. This repository is released under a [Creative Commons BY (CC-BY)](https://creativecommons.org/licenses/by/4.0/) license, which means all code can be shared and adapted for any purpose as long as appropriate credit is given. See [Attribution section](https://creativecommons.org/licenses/by/4.0/) for details. 

## What is Quarto?

[Quarto](https://quarto.org/) is a so-called "literate programming" notebook format that enables easy creation of dynamic documents with [R](https://quarto.org/docs/computations/r.html) and other programming languages. HTML and PDF reports can be generated from Quarto files using [knitr](http://yihui.name/knitr/), which can be installed automatically with [RStudio](http://www.rstudio.com/), and is fully integrated into this cross-platform IDE. All software used for these reports (Quarto, R, RStudio, etc.) is freely available and completely open-source. 

## How can I run this code?

The quickest and easiest way is to use RStudio.

 1. Download and install [R](https://cran.rstudio.com/) for your operating system
 1. Download and install [RStudio](https://www.rstudio.com/products/rstudio/download/) for your operating system
 1. Download and install [Quarto](https://quarto.org/) for your operating system
 1. Download a [zip file of this repository](https://github.com/KopfLab/2025_batther_et_al_C_limitation/archive/master.zip) and unpack it in an easy to find directory on your computer
 1. Navigate to the directory and double-click the `project.Rproj` file to start RStudio and load this project.
 1. Install the required libraries by running the following command in the Console in RStudio: `install.packages("pak"); pak::pak(c("tidyverse", "latex2exp", "knitr", "broom", "openxlsx"))` or by installing them manually in RStudio's Packages manager.
 1. Open the `.qmd` notebooks in the file browser
 1. To generate an HTML report ("render HTML"), select File --> Render document from the menu. The HTML report will be displayed upon successful completion and is saved as a standalone file in the same directory. All generated data figures are saved as PDF and PNG in the `plots` sub-directory. All generated data tables are saved as XLSX in the `output` sub-directory.
 
## Troubleshooting notes

The R Markdown files in this repository make use of various R modules for data processing, plotting and modelling. All of these should be installed automatically when the first R Markdown file is knitted (if the knitting fails because of a missing package, please install it manually, an error will indicate which package could not be installed). 