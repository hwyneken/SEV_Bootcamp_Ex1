require(ggplot2)
require(ggthemes)
require(scales)

Make_Sepien_Dist_Plot <- function(sepienDistList) {
  NSim <- length(sepienDistList[[1]])
  
  plotDF <- data.frame(z = c(sepienDistList[["N2 = 16"]],
                             sepienDistList[["N2 = 100"]],
                             sepienDistList[["N2 = 175"]],
                             sepienDistList[["N2 = 500"]],
                             sepienDistList[["N2 = 1000"]],
                             sepienDistList[["N2 = 10000"]]),
                       N2Choice = rep(names(sepienDistList),each=NSim))
  plotDF$N2Choice <- factor(plotDF$N2Choice,
                            levels = names(sepienDistList))
  plotDF$Outcome <- factor(ifelse(plotDF$z > 1.644,"Sepien Publishes","File Drawer"),
                           levels = c("Sepien Publishes","File Drawer"))
  
  sevBootcamp1Theme <- readRDS(file = "data/sevBootcamp1Theme.RDS")
  
  plot1 <- ggplot(data=plotDF,aes(x=z,fill=Outcome)) + geom_histogram(boundary = 1.644)
  plot1 <- plot1 + geom_vline(xintercept = 1.644)
  plot1 <- plot1 + facet_wrap(~ N2Choice,nrow=2,scales="free")
  plot1 <- plot1 + scale_fill_manual(values = c("#223B3C","#9FCEA7"))
  plot1 <- plot1 + sevBootcamp1Theme
  
  ## put labels on the plot
  
  minX <- c(min(layer_scales(plot1,1,1)$x$range$range),
            min(layer_scales(plot1,1,2)$x$range$range),
            min(layer_scales(plot1,1,3)$x$range$range),
            min(layer_scales(plot1,2,1)$x$range$range),
            min(layer_scales(plot1,2,2)$x$range$range),
            min(layer_scales(plot1,2,3)$x$range$range))
  maxX <- c(max(layer_scales(plot1,1,1)$x$range$range),
            max(layer_scales(plot1,1,2)$x$range$range),
            max(layer_scales(plot1,1,3)$x$range$range),
            max(layer_scales(plot1,2,1)$x$range$range),
            max(layer_scales(plot1,2,2)$x$range$range),
            max(layer_scales(plot1,2,3)$x$range$range))
  diffX <- maxX - minX
  labelX <- minX + 0.2 * diffX
  
  minY <- c(min(layer_scales(plot1,1,1)$y$range$range),
            min(layer_scales(plot1,1,2)$y$range$range),
            min(layer_scales(plot1,1,3)$y$range$range),
            min(layer_scales(plot1,2,1)$y$range$range),
            min(layer_scales(plot1,2,2)$y$range$range),
            min(layer_scales(plot1,2,3)$y$range$range))
  maxY <- c(max(layer_scales(plot1,1,1)$y$range$range),
            max(layer_scales(plot1,1,2)$y$range$range),
            max(layer_scales(plot1,1,3)$y$range$range),
            max(layer_scales(plot1,2,1)$y$range$range),
            max(layer_scales(plot1,2,2)$y$range$range),
            max(layer_scales(plot1,2,3)$y$range$range))
  diffY <- maxY - minY
  labelY <- minY + 0.75 * diffY
  
  labelVec <- c(sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 16"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 16"]],probs = 0.95,na.rm=TRUE)),
                sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 100"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 100"]],probs = 0.95,na.rm=TRUE)),
                sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 175"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 175"]],probs = 0.95,na.rm=TRUE)),
                sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 500"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 500"]],probs = 0.95,na.rm=TRUE)),
                sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 1000"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 1000"]],probs = 0.95,na.rm=TRUE)),
                sprintf("P(Z > 1.644): %0.2f\n95%% Quantile: %0.2f",
                        mean(sepienDistList[["N2 = 10000"]] > 1.644,na.rm=TRUE),
                        quantile(sepienDistList[["N2 = 10000"]],probs = 0.95,na.rm=TRUE)))
  
  labelDF <- data.frame(z = labelX,y=labelY,
                        N2Choice = factor(names(sepienDistList),
                                          levels=names(sepienDistList)),
                        label = labelVec,
                        Outcome = NA)
  plot1 <- plot1 + geom_label(data=labelDF,show.legend=FALSE,
                              size = 2.5,
                              aes(x=z,
                                  label = label,
                                  y = y))
  plot1 <- plot1 + labs(x = "Distribution of z | mu = 0 and sampling occurs when z_1 is in [1, 1.644]")
  
  ggsave(plot1,file = "images/SepiensDist.png",units="in",
         width = 12, height = 8)  
}


