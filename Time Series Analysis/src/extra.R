### Differencing
After differencing the series at lag 12, the mean appears stable. The partial 
```{r dev='png', fig.cap="Differenced (d=12) Monthly Mean Flow Series", fig.width=10, fig.align='center', htmlcap=5, cache=TRUE}
tsd = diff(dfg$Flow, 12)
tsd = data.frame(value = tsd, idx = dfg$idx[-1:-12])
p1 = ggplot(tsd, aes(y=value, x=idx)) + geom_line() + theme_bw() 
p1 = p1 + ggtitle("Differenced (d=12) Monthly Mean Flows for Ganges [ 1946 - 2012 ]")
p1 = p1 + xlab("Index") + ylab("Flow (CFS)")
p1 = p1 + theme(axis.title.y = element_text(vjust=1.5),
                axis.title.x = element_text(vjust=-0.5),
                plot.title = element_text(vjust=1.5)
)
p1
```

```{r dev='png', fig.cap="Total and partial correlograms for the differenced (d=12) series.", fig.width=10, fig.align='center', htmlcap=6, cache=TRUE}
q1 = ggacf(tsd$value) + xlab(" ") + theme(plot.margin=unit(c(2,2,2,2),"mm"))
q2 = ggpacf(tsd$value) + theme(plot.margin=unit(c(2,2,2,2),"mm"))
grid.arrange(q1, q2, nrow = 2)
```


### ARIMA
Here we con
```{r, fig.width=10, fig.height=10}

AIC =  matrix(ncol = 14, nrow = 14)
colnames(AIC) = paste('', 0:13, sep='')
rownames(AIC) = paste('', 0:13, sep='')

load("gAIC")

for(p in 0:13){
  for(q in 0:13){
    tryCatch({
      #a <- arima(tsd$value, order=c(p, 0, q))
      
      a <- Arima(dfgs2, order=c(p,0,q))
      if(exists("a")) AIC[q + 1, p + 1] <- floor(a$aic)
      
      
    }, error = function(cond){})
  }
}

pq = which(AIC == min(AIC), arr.ind=TRUE)
qbest = pq[,1]-1;
pbest = pq[,2]-1;

levelplot(t(AIC), panel=function(...) {
  arg <- list(...)
  panel.levelplot(...)
  panel.text(arg$x, arg$y, round(arg$z,1))
  panel.rect(xleft=pbest+0.5, xright=pbest+1.5, ytop=qbest+1.5, ybottom=qbest+0.5, lwd=4, border="red")
},
col.regions=colorRampPalette(brewer.pal(9,"Greys")[3:7])(25),
xlab = "p",
ylab = list("q", rot=0),
main = "AIC")


```




```{r, warning=FALSE, fig.height=4, fig.width=10, fig.cap="Differenced (d=12) Monthly Mean Flow Series", cache=TRUE, eval=TRUE}
tsd.fit = arima(tsd$value, order=c(pbest, 0, qbest))

q1 = ggacf.th(tsd$value, fit = tsd.fit) + xlab(" ") + theme(plot.margin=unit(c(2,2,2,2),"mm"))
q2 = ggpacf.th(tsd$value, fit = tsd.fit) + theme(plot.margin=unit(c(2,2,2,2),"mm"))

grid.arrange(q1, q2, nrow = 2)
```


## ARIMA

```{r, warning=FALSE, fig.height=4, fig.width=10, fig.cap="Differenced (d=12) Monthly Mean Flow Series", cache=TRUE, eval=TRUE}

tsd = dfmgr
tsd = data.frame(value = tsd, idx = dfg$idx)

tsd.fit = arima(tsd$value, order=c(pbest, 1, qbest))

q1 = ggacf.th(tsd$value, fit = tsd.fit) + xlab(" ") + theme(plot.margin=unit(c(2,2,2,2),"mm"))
q2 = ggpacf.th(tsd$value, fit = tsd.fit) + theme(plot.margin=unit(c(2,2,2,2),"mm"))

grid.arrange(q1, q2, nrow = 2)
```

```{r}



```


```{r}
for(i in 1:13){
  plot(Lag(tsd.fit$residuals,i), tsd.fit$residuals, main=paste(i))
}

resid = tsd.fit$residuals;

for(i in 1:6){
  plot(resid, type='p', pch=19, col="grey");
  residl = rep(NA, length(resid))
  residl[1:6==(i)] = resid[1:6==(i)]
  points(residl, col="red", pch=19)
}

for(i in 1:12){
  plot(resid, type='p', pch=19, col="grey");
  residl = rep(NA, length(resid))
  residl[1:12==(i)] = resid[1:12==(i)]
  points(residl, col="red", pch=19)
}


```


```{r anim, dev='png', fig.width=10, fig.show='animate', warning=FALSE, cache=TRUE, interval=0.1, cache=FALSE, htmlcap=3, fig.cap="Ganges monthly mean flows with _i_+12 observations highlighted."}
for(i in 1:12){
  dfg.lag12 = rep(NA, length(dfg$Flow))
  dfg.lag12[1:12==(i)] = log(dfg$Flow[1:12==(i)])
  p1 = ggplot(dfg, aes(y=log(Flow), x=idx)) + geom_line() + theme_bw() + geom_point(aes(y=dfg.lag12))
  p1 = p1 + ggtitle("Monthly Mean Flows for Ganges [ 1934 - 2012 ]")
  p1 = p1 + xlab("Index") + ylab("Flow (CFS)")
  p1 = p1 + theme(axis.title.y = element_text(vjust=1.5),
                  axis.title.x = element_text(vjust=-0.5),
                  plot.title = element_text(vjust=1.5)
  )
  print(p1)
}
```



