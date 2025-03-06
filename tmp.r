

#library(targter)
data(adult)
object <- tar <- targeter(adult, target = "ABOVE50K")
report(tar, quarto_root_dir  ="/hackdata/share/_tmp")

