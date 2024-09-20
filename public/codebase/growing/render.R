library(quarto)

quarto:::quarto_render("growing.qmd")

shell(cmd = "staticrypt growing.html -p r -t template.html --short")

# shell(cmd = "E:\\GITHUB\\project-bluebeam\\public\\codebase\\growing\\encrypted\\growing.html")

unlink("growing.html")

