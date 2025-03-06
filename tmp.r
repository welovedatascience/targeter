

#library(targter)
data(adult)
object <- tar <- targeter(adult, target = "ABOVE50K")
report(tar)


# quarto::is_using_quarto()
# quarto::quarto_create_project(
#   name = "targeter",
#   dir = "/tmp",
#   no_prompt = TRUE,
#   quiet = TRUE,
#   quarto_args = c("to"==c("html","docx","pptx","revealjs","pdf"))
#   )

# deploy all files
getwd()
file.copy(
  from = "./inst/ressources",
  to = file.path(find.package("targeter", lib.loc = .libPaths()),"ressources"), overwrite = TRUE)

# cp -r /hackdata/share/code/R/packages/targeter/inst/ressources/* /hackdata/Rlibrary-4.2.2/targeter/ressources




 custom_fields = list("report_type" = "targeter", "freeze" = TRUE)
yaml::as.yaml(custom_fields)
