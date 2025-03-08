library(targeter)


data(adult)

object <- tar <- targeter(adult, target="ABOVE50K")
report(tar)

(tarsum <- summary(tar))


(mod <- tartree(adult, tar, tarsum ))


targeter:::report.targeter(mod)


attributes(mod$tar_summary_model)
