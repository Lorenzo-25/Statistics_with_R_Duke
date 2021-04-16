HistoStats <- function(data,x_axis,binwidth = 100){
        stats<-pull(data,x_axis)
        stats <- stats[!is.na(stats)]
        print.data.frame(summarise(data,
                        "25Quantile_g"= quantile(stats,0.25),
                        "Median_b"= median(stats),
                        "Mean_r" = mean(stats),
                        "75Quantile_o" = quantile(stats,0.75),
                        "Min" = min(stats),
                        "Max" = max(stats),
                        "Sd" = sd(stats),
                        "IQR" = IQR(stats),
                        "n" = n()))
        ggplot(data) + 
                geom_histogram(aes_string(x = x_axis), binwidth = binwidth) +
                geom_vline(xintercept = c(median(stats),mean(stats),quantile(stats,0.25),quantile(stats,0.75)), 
                           color = c("blue","red","green","orange"))}

# Example 
        # data("ames")
        # HistoStats(ames,"area")
        # HistoStats(ames,"price",10000)

