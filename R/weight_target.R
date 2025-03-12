

# adapted from 
# https://github.com/rolkra/explore
# to removr dependency to explore package that has a conflictual report function
# remove tidyverse framework... (complexiy a lot by the may of)
# TODO: add tests

weight_target <- function (data, target) {
    observed_prop <- table(data[[target]])/nrow(data)
    minClass <- min(observed_prop)
    names(minClass) <- names(which(observed_prop == minClass))
    weights = ifelse(data[[target]] == names(minClass), max(observed_prop)/min(observed_prop), 
        1)
    return(weights)
}