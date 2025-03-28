
# targeter 1.6.0

* 20250328
* Nnow relies only on quarto for reporting, removed all
  dependencies to rmarkdown. This is a breaking change, as the previous
  templates are not compatible with quarto. The new templates are in the
  `inst/templates` directory and are based on the new `quarto` package.
* Internal refactoring of main compuation functions based on Claude Sonnet 3.7 
  suggestions. In addition to clarity for users who want to understand the code,
  this also provides some speed improvements.


# targeter 1.5.1

* 20250310
* New function targeter_big for computations on data with many columns


# targeter 1.5.0

* 20250308
* New functions to create decision trees adn associated report
* Global default index for all reports listing with categories


# targeter 1.4.1

* 20250306
* Brand new reporting mechanism fullly based on quarto engine, very flexible and  customizable (brands, logos, colors, etc.).
  slidify function is removed, replaced by updated render function.

# targeter 1.3.2

* released on 20250303 
* slidify function and its default template allow generating presentations in
  PowerPoint, HTML (revealJS) or PDF (LaTeX beamer) formats.


# targeter 1.3.1

* released on 20250302 
* beta version of presentations generation (currently powerpoint, only unix,
  few tests). Quarto is a requirement for that.

# targeter 1.3

* improve table for report (name changed to table_crossvar)
* change binary report template default parameters
* add tracking of variable usage/presence in targeter (new slot variables)
* remove from computations variabels with a single value
* focus gained arguments to use a max value for criteria and to force variables 
  per provided groups of variables.

# targeter 1.2

* Added decision trees in computation (option) and default templates.  


# targeter 1.1.1

* reverse back to correct WoE computing for binary/numeric targets; adaptation
  for categorical targets to be done.




# targeter 1.1.0

* default templates now have parameters to display tables or not and `report` 
  gains a new argument to pass parameters to templates rendering (possible value
  for argument `template_parameters`: "ask" (if templates allows that).

* `targeter` handles ordered factors when using `order`="auto" (the default). In
  those situations, factor's order will be respected in all tables/graphics. 
  When using naming conventions, ordered factors names start with "O_".


# targeter 1.0.0

* First public version, version 20230911.


# targeter 0.9.2

## Main changes

* new functionality: shinygadget to explore relations: `explore` function.

## Minor improvements and bug fixes

* fullplot now has a parameter which_plot to subset plots.
