
data <- adult
tar_object <- targeter(data, target='ABOVE50K', target_type='binary')
tarsum_object <- summary(tar_object)

mod <- tartree(data, tar_object = tar_object, tarsum_object = tarsum_object)

mod <- tartree(data, tar_object = tar_object, tarsum_object = tarsum_object, decision_tree_sample = 1)
