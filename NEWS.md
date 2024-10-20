
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