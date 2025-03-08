library(targeter)


data(adult)

object <- tar <- targeter(adult, target="ABOVE50K")
report.targeter(tar)

(tarsum <- summary(tar))


(mod <- tartree(adult, tar, tarsum ))

attributes(mod$tar_summary_model)
