library(quarto)

quarto:::quarto_render("growing.qmd")

shell(cmd = "staticrypt growing.html -p r -t template.html --short -d .")

# --short bỏ đi thông báo password ngắn
# -d . lưu lại ngay file ở directory
# - t template.html lưu file theo template
# -p r set password là r

# shell(cmd = "E:\\GITHUB\\project-bluebeam\\public\\codebase\\growing\\encrypted\\growing.html")

# unlink("growing.html")

