# targeter

Efficient Visual Targets Exploration 


## Purpose

Automated Explanatory Data Analysis (EDA) for targets exploration, 
 for both binary and numeric targets. Describes a target by crossing it with 
 any other candidate explanatory variables. Generates aggregated statistics
 allowing to prioritize inspection, such as Information Value (including an
 extension of this metric for continuous targets). We also provide plot methods,
 automated reports based on markdown and a shiny gadget for interactive
 explorations. Package is aimed at investigating big datasets, both in terms of
 records and variables through the usage of data.table package.


## Installation

To install the latest development version of the package directly from GitHub 
use the following code:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("weloevdatascience/targeter")
```


## References

* Alec Zhixiao Lin, PayPal Credit, Timonium, MD
Tung-Ying Hsieh, Dept. of Mathematics & Statistics, Univ of Maryland, Baltimore, MD - [Expanding the Use of Weight of Evidence and Information Value to Continuous Dependent Variables for Variable Reduction and 
Scorecard Development](https://www.lexjansen.com/sesug/2014/SD-20.pdf) (2014) 

## Contact

We use the following for support and communications between user and developer community:

* [GitHub Issues](https://github.com/welovedatascience/targeter/issues)---for direct feedback, enhancement requests or raising bugs

## Acknowledgments

Along with the authors and contributors, thanks to the following people for their work/input on the package: 
Manuel Piette (good old Qualifix).