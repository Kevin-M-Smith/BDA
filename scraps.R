After applying order 12 differencing, much of the structure in the partial and 
```{r, dev='png', echo=FALSE, fig.height=4, fig.width=10, fig.cap="Differenced (d=12) Monthly Mean Flow Series", cache=TRUE, eval=TRUE}
tsd = dfg
tsd$Flow = diff(dfg$Flow, 12)
p1 = ggplot(tsd, aes(y=Flow, x=idx)) + geom_line() + theme_bw() 
p1 = p1 + ggtitle("Differenced Monthly Mean Flows for Ganges [ 1934 - 2012 ]")
p1 = p1 + xlab("") + ylab("Flow (CFS)")
p1 = p1 + theme(axis.title.y = element_text(vjust=1.5),
                axis.title.x = element_text(vjust=-0.5),
                plot.title = element_text(vjust=1.5)
)




```



```{r, fig.cap="Total and partial correlograms of differenced (d = 12) series.", eval=FALSE}
q1 = ggacf(tsd) + xlab(" ") + theme(plot.margin=unit(c(2,2,2,2),"mm"))
q2 = ggpacf(tsd) + theme(plot.margin=unit(c(2,2,2,0),"mm"))
grid.arrange(q1, q2, nrow = 2)

```


```{r, echo=FALSE, eval=FALSE}
#source("plot.table.R")

AIC =  matrix(ncol = 4, nrow = 4)
colnames(AIC) = paste('', 1:4, sep='')
rownames(AIC) = paste('', 1:4, sep='')

for(p in 1:4){
  for(q in 1:4){
    tryCatch({
      a <- arima(dfg$Flow, order=c(p, 0, q))
      if(exists("a")) AIC[q, p] <- floor(a$aic)
    }, error = function(cond){})
  }
}

levelplot(t(AIC), panel=function(...) {
  arg <- list(...)
  panel.levelplot(...)
  panel.text(arg$x, arg$y, round(arg$z,1))},
  col.regions=colorRampPalette(brewer.pal(9,"Greys")[3:7])(25),
  xlab = "p",
  ylab = list("q", rot=0),
  main = "AIC")
```


<br>
  **1.1.d** - **_ARIMA Model_**
  
  
  \pagebreak


```{r, dev='png', echo=FALSE, fig.cap="", fig.height=3.25, fig.width=6.5, eval=FALSE}

tsd = diff(dfg$Flow, 12)

#qq1 = ggacf(tsd) + xlab(" ") + theme(plot.margin=unit(c(2,2,2,2),"mm"))
#qq2 = ggpacf(tsd) + theme(plot.margin=unit(c(2,2,2,0),"mm"))


#qq1
#qq2

```