# guassian data with power calculations


plot(c(-3, 6),c(0, dnorm(0)), type = "n", frame = false, xlab = "Z value", ylab = "")




library(ggplot2)
draw_plot <- function(mu_0, sigma_0, mu_a, sigma_a, lin) {
    xvals <- seq(-3, 6, length = 1000)
    y2 <- dnorm(xvals, mean = mu_a, sd = sigma_a)
    y1 <- dnorm(xvals, mean = mu_0, sd = sigma_0)
    
    null_grp <- rep("none", length(y2))
    null_grp[xvals < lin] = "TN"
    null_grp[xvals >= lin] = "FP"
    
    alt_grp <- rep("none", length(y1))
    alt_grp[xvals < lin] = "FN"
    alt_grp[xvals >= lin] = "TP"
    
    categories <- c(null_grp, alt_grp)
    
    y <- c(y2,y1)
    dist <- c( rep("Null", length(y2)), rep("Alternative", length(y1)) )
    df <- data.frame(x=c(xvals,xvals), y=y, dist=dist, categories=categories)
    
    ggplot(df, aes(x,y, group=dist)) + geom_line() + 
        geom_ribbon(data=subset(df,x>lin & x<max(x)),
                    aes(x=x, ymax=y, group=dist, fill=categories),
                    ymin=0,
                    colour="black",
                    size=1.25,
                    alpha=0.5) + 
        geom_ribbon(data=subset(df,x>min(x) & x<lin),
                    aes(x=x, ymax=y, group=dist, fill=categories),
                    ymin=0,
                    colour="black",
                    size=1.25,
                    alpha=0.5) +
        geom_vline(xintercept = lin, size=2, color="dark orange") +
        theme_bw(base_family = "Gill Sans", base_size = 14)
}

mu_0 = 0; mu_a = 2;
sigma_0 <- 1; sigma_a <- 1.5;
lin = 1

draw_plot(0, sigma_0, mu_a, sigma_a, 2)



qplot(x, y, data=df, geom="line") +
geom_ribbon(data=subset(df,x>0 & x<max(x)),aes(ymax=y),ymin=0,
            fill="red",colour=NA,alpha=0.5) +

