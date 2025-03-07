library(targeter)
data <- adult
tar_object <- targeter(data, target='ABOVE50K', target_type='binary')
tarsum_object <- summary(tar_object)

mod <- tartree(data, tar_object = tar_object, tarsum_object = tarsum_object)
class(mod)

tar_report(mod)


tar_mod <- tartree(data, tar_object = tar_object, tarsum_object = tarsum_object, decision_tree_sample = 1)
names(attributes(tar_mod))
getwd()
saveRDS(mod, file = 'tar_mod.rds')
saveRDS(mod, file = './inst/ressources/tar_mod.rds')
