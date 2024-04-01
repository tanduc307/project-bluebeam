# windows()

library(survminer)
yes <- survminer::ggsurvplot(all_survival,
                             data = treat_all, 
                             censor.shape="|", 
                             censor.size = 2,
                             # conf.int = TRUE,
                             # pval = TRUE,
                             risk.table = TRUE,
                             risk.table.col = "black",
                             fontsize = 3,
                             break.time.by = 6,
                             legend.labs = c("Basal", 
                                             "B50 8%", 
                                             "B50 16%", 
                                             "B60 8%", 
                                             "B60 16%"),
                             cumevents = TRUE,
                             cumcensor = TRUE,
                             # table.height = 0.02,
                             # surv.plot.height = 0.9,
                             ggtheme = theme_bw() + 
                               theme(panel.border = element_blank(),
                                     axis.text = element_text(face = "bold",
                                                              color = "black"),
                                     plot.margin = margin(20, 20, 20, 20)),
                             legend.title = "Diet",
                             xlim = c(0, 132),
                             tables.y.text.col = FALSE,
                             test.for.trend = TRUE,
                             tables.col = "black",
                             xlab = "Time in hours post challange",
                             surv.median.line = "hv",
                             )

final <- yes$plot + 
  ggplot2::annotate("text", 
                    x = 12, 
                    y = 0.2, 
                    label = bquote("\u03C7"^2 == 7.2 ~~~~~~ "p = 0.1"), 
                    size = 4) +
  geom_hline(yintercept = 0.5,
             linetype = 2)

library(patchwork)

final / yes$table / yes$cumevents / yes$ncensor.plot













