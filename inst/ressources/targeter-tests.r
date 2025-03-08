library(targeter)


data(adult)

tar <- targeter(adult, target="ABOVE50K")
report.targeter(tar)
class(tar)
