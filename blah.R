We can check out the sensitivity of our results to the strength of the dependency between layers:

```{r}
    res1 <- status1(C + (0.05 * kronecker(A, I))) #weak dependency
    res2 <- status1(C + (10 * kronecker(A, I))) #moderate dependency
    res3 <- status1(C + (100 * kronecker(A, I))) #strong dependency
```

```{r, echo = FALSE}
    dat.f <- data.frame(s = c(res1[1:21]/max(res1[1:21]), res2[1:21]/max(res2[1:21]), res3[1:21]/max(res3[1:21])), alpha = c(rep("0.05", 21), rep("10", 21), rep("100", 21)), node = c(1:21, 1:21, 1:21))
    dat.f$alpha <- as.factor(dat.f$alpha)

    dat.a <- data.frame(s = c(res1[22:42]/max(res1[22:42]), res2[22:42]/max(res2[22:42]), res3[22:42]/max(res3[22:42])), alpha = c(rep("0.05", 21), rep("10", 21), rep("100", 21)), node = c(1:21, 1:21, 1:21))
    dat.a$alpha <- as.factor(dat.a$alpha)

    dat.r <- data.frame(s = c(res1[43:63]/max(res1[43:63]), res2[43:63]/max(res2[43:63]), res3[43:63]/max(res3[43:63])), alpha = c(rep("0.05", 21), rep("10", 21), rep("100", 21)), node = c(1:21, 1:21, 1:21))
    dat.r$alpha <- as.factor(dat.r$alpha)
```

```{r, echo = FALSE}
    library(ggplot2)
    p <- ggplot(dat.f, aes(x = as.factor(node), y = s, color = alpha, group = alpha))
    p <- p + geom_point(size = 3)
    p <- p + theme_minimal()
    p <- p + theme(axis.title  = element_blank(),
              axis.text = element_text(size = 14),
              legend.title = element_blank(),
              legend.position = "bottom"
              )
    p + ggtitle("Friendship Layer")

    p <- ggplot(dat.a, aes(x = as.factor(node), y = s, color = alpha, group = alpha))
    p <- p + geom_point(size = 3)
    p <- p + theme_minimal()
    p <- p + theme(axis.title  = element_blank(),
              axis.text = element_text(size = 14),
              legend.title = element_blank(),
              legend.position = "bottom"
              )
    p + ggtitle("Advice Layer")


    p <- ggplot(dat.r, aes(x = as.factor(node), y = s, color = alpha, group = alpha))
    p <- p + geom_point(size = 3)
    p <- p + theme_minimal()
    p <- p + theme(axis.title  = element_blank(),
              axis.text = element_text(size = 14),
              legend.title = element_blank(),
              legend.position = "bottom"
              )
    p + ggtitle("Reports to Layer")
```