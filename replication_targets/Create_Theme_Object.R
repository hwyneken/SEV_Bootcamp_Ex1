require(ggplot2)
require(ggthemes)

Create_Theme_Object <- function() {
  sevBootcamp1Theme <- theme_tufte()
  sevBootcamp1Theme <- sevBootcamp1Theme + theme(axis.text = element_text(size=12),
                                                 axis.title = element_text(size=14),
                                                 title = element_text(size=16),
                                                 legend.title = element_text(size=14),
                                                 legend.text = element_text(size=12),
                                                 strip.text = element_text(size=14),
                                                 panel.background = element_rect(fill="#f8f8f8"),
                                                 strip.background = element_rect(fill="gray"))
  saveRDS(sevBootcamp1Theme,file = "data/sevBootcamp1Theme.RDS")  
}

