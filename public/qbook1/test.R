library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length, color = Species, fill = Species)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(limits = c(3, 9)) +
  labs(title = "Phân phối về chiều dài đài hoa giữa các loài trong chi Iris") +
  theme_classic() +
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))
