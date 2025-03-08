library(targeter)


data(adult)

object <- tar <- targeter(adult, target="ABOVE50K")
(tarsum <- summary(tar))
(mod <- tartree(adult, tar, tarsum ))



tar_report(tar)


tar_report(mod)


attributes(mod$tar_summary_model)

library(yaml)

report_categories <- "[tree, test]"
class(report_categories) <- "tar_report_categories"
verbatim_tar_report_categories <- function(x) {return(x)}
yaml::as.yaml(
  report_categories,
  handlers = list("tar_report_categories"=verbatim_tar_report_categories))

yaml::verbatim_logical("[test]")

test <- yaml::read_yaml('tmp.yaml')
yaml::write_yaml(test, "yes.yaml")
