# Anscombe's Quartet
anscombe
str(anscombe)
(ans_x_mean <- anscombe %>%
    select(x1,x2,x3,x4) %>%
    summarise_each(list(mean),x1,x2,x3,x4))
(ans_y_mean <- anscombe %>%
    select(y1,y2,y3,y4) %>%
    summarise_each(list(mean),y1,y2,y3,y4))
(ans_x_var <- anscombe %>%
    select(x1,x2,x3,x4) %>%
    summarise_each(list(var),x1,x2,x3,x4))
(ans_y_var <- anscombe %>%
    select(y1,y2,y3,y4) %>%
    summarise_each(list(var),y1,y2,y3,y4))
ans_cor <- c()
for (i in 1:4) {
  ans_cor <- c(ans_cor,cor(anscombe[,i],anscombe[,i+4]))
}
ans_cor

p1 <- ggplot(anscombe) + geom_point(aes(x1, y1), color = "darkorange", size = 3) + 
  theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + labs(title = "dataset 1")
p2 <- ggplot(anscombe) + geom_point(aes(x2, y2), color = "darkorange", size = 3) + 
  theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + labs(title = "dataset 2")
p3 <- ggplot(anscombe) + geom_point(aes(x3, y3), color = "darkorange", size = 3) + 
  theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + labs(title = "dataset 3")
p4 <- ggplot(anscombe) + geom_point(aes(x4, y4), color = "darkorange", size = 3) + 
  theme_bw() + scale_x_continuous(breaks = seq(0, 20, 2)) + scale_y_continuous(breaks = seq(0, 12, 2)) + 
  geom_abline(intercept = 3, slope = 0.5, color = "cornflowerblue") + 
  expand_limits(x = 0, y = 0) + labs(title = "dataset 4")

grid.arrange(p1, p2, p3, p4, ncol=2, top = "Anscombe's Quartet")
