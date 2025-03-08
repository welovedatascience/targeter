
# library(targeter)
# data(adult)
# tar <- targeter(adult, target="ABOVE50K")

# summary(tar)
# meta <- data.frame(variable=names(adult), label=paste("var:", names(adult)))
# attr(tar, "metadata") <- meta
# saveRDS(tar, "./inst/ressources/tar.rds")

# object <- tar

# params <- list(metadata_var_field="variable", metadata_var_label="label")
# metadata <- attr(object, "metadata")

# label(object$target,metadata, var_field = params$metadata_var_field, label_field= params$metadata_var_label )

# quarto::is_using_quarto
# quarto:::quarto_rsc_metadata()


# quarto::quarto_render()

# # --reference-doc=FILE|URL
# setwd('./inst/ressources')

# quarto::quarto_render("report-template.qmd",
#   output_format = "pptx",
#   debug=TRUE,
#   pandoc_args = c(
#     # "--data-dir=.",
#     "--reference-doc=slidify-pptx-targeter-template.pptx"
#     )
#   )

# quarto::quarto_render("report-template.qmd",
#   output_format = "pptx",
#   debug=TRUE,
#   pandoc_args = c(
#     # "--data-dir=.",
#     "--reference-doc=slidify-pptx-targeter-template.pptx"
#     )
#   )


# f (!missing(metadata)) {
#         if (!missing(metadata_file)) {
#             metadata <- merge_list(yaml::read_yaml(metadata_file,
#                 eval.expr = FALSE), metadata)
#         }
#         meta_file <- tempfile(pattern = "quarto-meta", fileext = ".yml")
#         on.exit(unlink(meta_file), add = TRUE)
#         write_yaml(metadata, meta_file)
#         args <- c(args, "--metadata-file", meta_file)
#     }