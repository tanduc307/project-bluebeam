df <- read.csv("students.csv")

library(kableExtra)
df %>% kbl(format = "html") %>%
  kable_styling(bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed", 
                                      "bordered", 
                                      "responsive")) %>%
  row_spec(0, bold = TRUE, align = "c", color = "white", background = "#1d6c00") %>% 
  kable_classic(full_width = TRUE, html_font = "arial") -> output

save_kable(output, file = "output.html")