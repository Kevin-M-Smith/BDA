# Time Series Analysis
  
  



<script type="text/javascript" src="js/jquery.min.js"></script>
<script type="text/javascript" src="js/jquery-ui.min.js"></script>
<script type="text/javascript" src="js/jquery.fancybox-1.3.4.pack.min.js"></script>
<script type="text/javascript" src="js/jquery.tocify.js"></script>
<script type="text/javascript" src="js/jquery.scianimator.min.js"></script>

<link type="text/css" rel="stylesheet" href="css/jquery.tocify.css" />
<link type="text/css" rel="stylesheet" media="screen" href="css/jquery.fancybox-1.3.4.css" />
<link type="text/css" rel="stylesheet" href="css/scianimator.light.css" />


<head>
<div id="TOC" class="TOC"></div>

<style type="text/css">
  a.fancybox img {
    border: none;
    -o-transform: scale(1,1); -ms-transform: scale(1,1); -moz-transform: scale(1,1); -webkit-transform: scale(1,1); transform: scale(1,1); -o-transition: all 0.2s ease-in-out; -ms-transition: all 0.2s ease-in-out; -moz-transition: all 0.2s ease-in-out; -webkit-transition: all 0.2s ease-in-out; transition: all 0.2s ease-in-out;
  } 
a.fancybox:hover img {
  position: relative; z-index: 999; -o-transform: scale(1.03,1.03); -ms-transform: scale(1.03,1.03); -moz-transform: scale(1.03,1.03); -webkit-transform: scale(1.03,1.03); transform: scale(1.03,1.03);
}

#TOC {
  position: fixed;
  left: 0;
  top: 0;
  width: 250px;
  overflow:auto;
}

#source{
  position: fixed;
  left: 0;
  bottom: 0;
  width: 250px;
  overflow:auto;
}

body {
  float: left; 
  margin: auto;
  margin-left:300px;
  line-height: 20px;
}

p { line-height: 1.5; }

.fancybox-title-inside {
  margin-left: 0px !important;
}


</style>
</head>


<body>
  <script type="text/javascript">
  
  
  $(function($){
    $("#TOC").tocify({extendPage: true, selectors: "h1,h2,h3,h4"});
    var addToAll = true;
    var gallery = false;
    var titlePosition = 'inside';
    $(addToAll ? 'img' : 'img.fancybox').each(function(){
      var $this = $(this);
      var title = $this.attr('title');
      var src = $this.attr('data-big') || $this.attr('src');
      var a = $('<a href="#" class="fancybox"></a>').attr('href', src).attr('title', title);
      $this.wrap(a);
    });
    if (gallery)
      $('a.fancybox').attr('rel', 'fancyboxgallery');
    $('a.fancybox').fancybox({
      titlePosition: titlePosition
    });
  });
  
$.noConflict();

  
</script>

<script>
function toggle_R(){
$("pre").slideToggle(1);
setTimeout(function(){
$("div[data-unique*=" + $(".active")[0].getAttribute("data-unique") + "]")[0].scrollIntoView();
}, 100);

}
</script>


</body>

<div id="source" class="tocify"> 
<ul class="tocify-header nav nav-list">
<li class="tocify-item active" style="cursor: pointer;">
<a onclick='toggle_R();' >Show / Hide Source</a>
</li></ul>
</div>
__Kevin M. Smith // primisadfont.es // November 20th, 2014__
<hr>



# Overview
This project explores basic time series analysis techniques for two datasets. The first dataset is a complete record of the mean monthly flows of the Ganges from January 1934 to December 2013. The second dataset is a complete record of the mean monthly flows of the Nile (collected at Aswan Dam) from January 1871 to December 1984.

<hr>
# Monthly Flow in the Ganges
## Data Cleaning
### Importing the Data

First let's load our data into a __data frame__ object using the __read.csv()__ command. Then we'll follup with a __head()__ command to look at the first few rows of the dataset. 

```r
ganges <- read.csv("data/Ganges.csv")
head(ganges)
```

```
##   Year  Jan  Feb  Mar  Apr  May  Jun   Jul   Aug   Sep   Oct  Nov  Dec
## 1 1934 2778 2458 2228 2138 1987 3613 19775 36277 40084 18625 6197 3432
## 2 1935 2389 2056 1625 1888 1654 2918 13086 38239 27645 13908 4531 3356
## 3 1936 2858 2312 2442 1434 1778 5189 22873 39497 38061 16688 6665 3830
## 4 1937 2630 2495 2392 1803 1877 3440 11893 33913 30065 18748 6379 3495
## 5 1938 2550 2176 2013 2216 2053 8557 28319 43681 33735 10132 4880 3254
## 6 1939 2291 2164 2096 1750 1790 3090 13052 28606 26545 12759 5277 3132
```

The data are organized into 13 columns. The first contains the year of the observations in a given row, while the other 12 contain the mean monthly flow in cubic feet per second (CFS). 

### Checking for Missing Values
Let's verify that the dataset is complete. With a dataset this small it is easy to look for missing data visually, but with larger datasets this is difficult. For a simple check we can use __sapply()__ and __is.na()__. Here we count each occurance of missing data using __sum()__. The result is a column-wise count of the missing values. If the dataset is complete we should see all zeros.

```r
sapply(ganges, function(x) sum(is.na(x)))
```

```
## Year  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec 
##    0    0    0    0    0    0    0    0    0    0    0    0    0
```

### Reshaping the Data Frame
The shape of the data frame is not currently very conducive to plotting as a time series. It would be easier to plot discharge as a function of time if there were instead one observation of discharge per row. An easy way to make this happen is to "melt" the data frame using the __reshape2__ package. The __melt()__ takes arguments that specify how the data frame should be "melted." The __id.vars__ argument specifies the names of the columns that should be preserved as columnar variables. Here we want to preserve the __Year__ column. The rest of the columns melt into attributes within the rows. We will assign the column headings to a new column __Month.Abb__ and their associated values to the column __Flow__, using the __variable.name__ and __value.name__ arguments respectively. 


```r
require(reshape2)
ganges <- melt(ganges, id.vars="Year", variable.name="Month.Abb", value.name="Flow")
head(ganges)
```

```
##   Year Month.Abb Flow
## 1 1934       Jan 2778
## 2 1935       Jan 2389
## 3 1936       Jan 2858
## 4 1937       Jan 2630
## 5 1938       Jan 2550
## 6 1939       Jan 2291
```

### Augmenting the Data Frame
Now that we have the data in the format we'd like, let's add a few extra attributes that will be helpful. We'll be using the __plyr__ package  and the __transform()__ function for this purpose. As a first step, let's add a column ___MID___ as an integer representation of the __Month.Abb__ attribute. The __base__ package in __R__ pre-loads three-letter abbreviations for months into the vector __month.abb__. It can be accessed anytime by simply calling it:

```r
month.abb 
```

```
##  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
## [12] "Dec"
```
The ordering here is intuitive. January = 1, February = 2, etc. We can use this to create a function that match the ___Month.Abb___ attribute in our dataset with index of the abbreviation in __month.abb__. 

```r
getMID <- function(x){ match(x, month.abb) }
```

Let's give it a try.

```r
getMID("May")
```

```
## [1] 5
```

Now let's pass the __getMID()__ to the __transform()__ function from the __plyr__ package to augment our data set.

```r
require(plyr)
ganges <- transform(ganges, MID = getMID(Month.Abb))
head(ganges)
```

```
##   Year Month.Abb Flow MID
## 1 1934       Jan 2778   1
## 2 1935       Jan 2389   1
## 3 1936       Jan 2858   1
## 4 1937       Jan 2630   1
## 5 1938       Jan 2550   1
## 6 1939       Jan 2291   1
```

### Sorting the Data
Our data is not currently sorted by time. However, now that we have numeric representations of both the years and months of our observations, we can use __arrange()__ from the __plyr__ package to quickly sort our data. Let's __arrange()__ the data in ascending order of __Year__ and then __MID__. 

```r
ganges <- arrange(ganges, Year, MID)
head(ganges)
```

```
##   Year Month.Abb Flow MID
## 1 1934       Jan 2778   1
## 2 1934       Feb 2458   2
## 3 1934       Mar 2228   3
## 4 1934       Apr 2138   4
## 5 1934       May 1987   5
## 6 1934       Jun 3613   6
```

__N.B.__: We have been using __head()__ to check the first 6 rows of our data, but we can also use __tail()__ to see the last six rows. 

```r
tail(ganges)
```

```
##     Year Month.Abb  Flow MID
## 943 2012       Jul 11955   7
## 944 2012       Aug 24402   8
## 945 2012       Sep 28627   9
## 946 2012       Oct 12034  10
## 947 2012       Nov  4122  11
## 948 2012       Dec  2186  12
```

### Indexing the Data
As a final step, it is useful to have an index of the data. Here we will just use the order of the rows, since the data is sorted. This time we will just use the __\$__ accessor method to access the desired column from the data frame. When __\$__ is used with a name that is not currently in the dataframe a new column is created. We will use the colon operator to generate a regular sequence (e.g. 1, 2, 3...) from 1 to the number of rows. We can use __nrow()__ to calculate the number of rows.  


```r
ganges$Index <- 1:nrow(ganges)
head(ganges)
```

```
##   Year Month.Abb Flow MID Index
## 1 1934       Jan 2778   1     1
## 2 1934       Feb 2458   2     2
## 3 1934       Mar 2228   3     3
## 4 1934       Apr 2138   4     4
## 5 1934       May 1987   5     5
## 6 1934       Jun 3613   6     6
```

<hr>

## The Full Monthly Series
### Plotting the Time Series
We're finally ready to plot the data. The __ggplot2__ package provides decent graphics capabilities out of the box. Let's fire them up and plot the monthly flow series. 

```r
require(ggplot2)

timerange <- paste("(", min(ganges$Year), "-", max(ganges$Year), ")")
p1 <- ggplot(ganges, aes(y=Flow, x=Index)) + geom_line()
p1 <- p1 + ggtitle(paste("Time Series of Ganges Monthly Mean Flow", timerange))
p1 <- p1 + ylab("Flow (CFS)") + theme_bw()
p1
```

![](index_files/figure-html/unnamed-chunk-12-1.png) 

### Boxplots by Month
The data seems to be exhibiting serious seasonality, but it is difficult to tell what is really going on at this scale. Let's bin the data by month and plot a boxplot. 

```r
p1 <- ggplot(ganges, aes(y=Flow, x=Month.Abb)) + geom_boxplot() + theme_bw()
p1 <- p1 + ggtitle(paste("Boxplot of Ganges Monthly Mean Flows by Month", timerange))
p1
```

![](index_files/figure-html/unnamed-chunk-13-1.png) 

Now the structure of the within-year variability is clearer. There appear to be both 'wet' and 'dry' seasons of differing lengths. We will eximine these sections independantly later on. For now, we will continue to work with the full time series. 

### Summary Statistics
It can be helpful to have a table of summary statistics to refer to. The __ddply()__ function in the __plyr__ package is very powerful. It includes a sub-command __summarize__ that will return a data frame summarizing the data to your specifications. Here we'd like a summary table of the Mean, Standard Deviation, and Coefficient of Variation by Month. 

```r
sum.stats <- ddply(ganges, "Month.Abb", summarize, 
           Mean = mean(Flow),
           SD = sd(Flow), 
           CV = Mean/SD)
sum.stats
```

```
##    Month.Abb      Mean        SD       CV
## 1        Jan  2537.051 1123.8789 2.257406
## 2        Feb  2013.582  843.4563 2.387299
## 3        Mar  1663.063  765.5877 2.172270
## 4        Apr  1519.101  648.6989 2.341766
## 5        May  1758.266  574.2320 3.061943
## 6        Jun  3987.658 1617.7573 2.464930
## 7        Jul 18763.671 5860.1183 3.201927
## 8        Aug 37200.608 8042.0985 4.625734
## 9        Sep 35987.203 8398.9176 4.284743
## 10       Oct 17374.139 6488.6895 2.677604
## 11       Nov  6505.380 2290.9823 2.839559
## 12       Dec  3761.633 1657.3911 2.269611
```

The __pander__ package includes some nice table formatting features. Let's apply it to our table by calling the __pander()__ function. 

```r
require(pander)
panderOptions('digits', 3)
panderOptions('keep.trailing.zeros', TRUE)
pander(sum.stats)
```


----------------------------
 Month.Abb   Mean   SD   CV 
----------- ------ ---- ----
    Jan      2537  1124 2.26

    Feb      2014  843  2.39

    Mar      1663  766  2.17

    Apr      1519  649  2.34

    May      1758  574  3.06

    Jun      3988  1618 2.46

    Jul     18764  5860 3.20

    Aug     37201  8042 4.63

    Sep     35987  8399 4.28

    Oct     17374  6489 2.68

    Nov      6505  2291 2.84

    Dec      3762  1657 2.27
----------------------------

### Making The Series Stationary
#### Transformations
We have already seen that the plot of monthly flows is highly seasonal with neither a constant mean or variance. __Applying a transform to the data can help stabalize the variance.__ Let's see how a log transform looks. 

```r
timerange <- paste("(", min(ganges$Year), "-", max(ganges$Year), ")")
p1 <- ggplot(ganges, aes(y=log(Flow), x=Index)) + geom_line()
p1 <- p1 + ggtitle(paste("Time Series of Ganges Monthly Log-Transformed Mean Flow", timerange))
p1 <- p1 + ylab("Log Flow") + theme_bw()
p1
```

![](index_files/figure-html/unnamed-chunk-16-1.png) 

The transform looks like it helped stablize both the mean and the variance, but perhaps we can do better. The seasonal structure of the data revealed by the boxplots can be further highlighted by highlighting each ___12th___ observation. 


```r
for(i in 1:12){
  ganges.lag12 = rep(NA, nrow(ganges))
  ganges.lag12[1:12==(i)] = log(ganges$Flow[1:12==(i)])
  
  p1 <- ggplot(ganges, aes(y=log(Flow), x=Index)) + theme_bw()
  p1 <- p1 + geom_line() + geom_point(aes(y=ganges.lag12)) + ylab("Log Flow")
  p1 <- p1 + ggtitle(paste("Highlighting the Lag 12 Correlation in Ganges Mean Monthly Flows", 
                           timerange))
  print(p1)
}
```


<div class="scianimator">
<div id="unnamed_chunk_17" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(12);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "index_files/figure-html/unnamed-chunk-17-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_17").scianimator({
          "images": imgs,
          "delay": 100,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_17").scianimator("play");
    });
  })(jQuery);
</script>

#### Removing Monthly Log-Means
We can remove some of the correlation between the observations at lag 12 by removing monthly means. However, we will continue to work in log space so that we can gauruntee non-negative values when we reverse the transformation. Again, we'll turn to the __plyr__ package. First let's add a column for our log-transformed data using the __transform()__ function.

```r
ganges <- transform(ganges, Log.Flow = log(Flow))
head(ganges)
```

```
##   Year Month.Abb Flow MID Index Log.Flow
## 1 1934       Jan 2778   1     1 7.929487
## 2 1934       Feb 2458   2     2 7.807103
## 3 1934       Mar 2228   3     3 7.708860
## 4 1934       Apr 2138   4     4 7.667626
## 5 1934       May 1987   5     5 7.594381
## 6 1934       Jun 3613   6     6 8.192294
```

Now we'll use __ddply()__ and __summarize()__ to create summary statistics about the means of the log-transformed flows. 

```r
log.flow.monthly.stats <- ddply(ganges, "Month.Abb", summarize, Log.Flow.M.Mean = mean(Log.Flow), Log.Flow.M.SD = sd(Log.Flow))
head(log.flow.monthly.stats)
```

```
##   Month.Abb Log.Flow.M.Mean Log.Flow.M.SD
## 1       Jan        7.758477     0.3953657
## 2       Feb        7.516288     0.4442656
## 3       Mar        7.292052     0.5324115
## 4       Apr        7.214886     0.5055721
## 5       May        7.413945     0.3569764
## 6       Jun        8.208680     0.4194766
```

Next, we join the summary statistics back to the original dataset using __plyr__'s __join()__ function. 

```r
ganges <- join(ganges, log.flow.monthly.stats, by="Month.Abb")
head(ganges)
```

```
##   Year Month.Abb Flow MID Index Log.Flow Log.Flow.M.Mean Log.Flow.M.SD
## 1 1934       Jan 2778   1     1 7.929487        7.758477     0.3953657
## 2 1934       Feb 2458   2     2 7.807103        7.516288     0.4442656
## 3 1934       Mar 2228   3     3 7.708860        7.292052     0.5324115
## 4 1934       Apr 2138   4     4 7.667626        7.214886     0.5055721
## 5 1934       May 1987   5     5 7.594381        7.413945     0.3569764
## 6 1934       Jun 3613   6     6 8.192294        8.208680     0.4194766
```

Now, we'll __mutate()__ the data frame one last time to get the standardized series. (Mutate is similar to transform, except that the newly declared columns can be reused right away to declare other new columns.) Let's call it __Log.Flow.Standardized__. 

```r
ganges <- mutate(ganges, 
                 Log.Flow.M.Mean.Removed = Log.Flow - Log.Flow.M.Mean, 
                 Log.Flow.Standardized = Log.Flow.M.Mean.Removed / Log.Flow.M.SD)
head(ganges)
```

```
##   Year Month.Abb Flow MID Index Log.Flow Log.Flow.M.Mean Log.Flow.M.SD
## 1 1934       Jan 2778   1     1 7.929487        7.758477     0.3953657
## 2 1934       Feb 2458   2     2 7.807103        7.516288     0.4442656
## 3 1934       Mar 2228   3     3 7.708860        7.292052     0.5324115
## 4 1934       Apr 2138   4     4 7.667626        7.214886     0.5055721
## 5 1934       May 1987   5     5 7.594381        7.413945     0.3569764
## 6 1934       Jun 3613   6     6 8.192294        8.208680     0.4194766
##   Log.Flow.M.Mean.Removed Log.Flow.Standardized
## 1              0.17100998            0.43253625
## 2              0.29081527            0.65459781
## 3              0.41680773            0.78286758
## 4              0.45274049            0.89550128
## 5              0.18043650            0.50545773
## 6             -0.01638627           -0.03906361
```

Finally, let's check in on our time series. 

```r
for(i in 1:12){
  ganges.lag12 = rep(NA, nrow(ganges))
  ganges.lag12[1:12==(i)] = ganges$Log.Flow.Standardized[1:12==(i)]
  
  p1 <- ggplot(ganges, aes(y=Log.Flow.Standardized, x=Index)) + theme_bw() + ylab("Mean-Differenced Log-Transformed Flows")
  p1 <- p1 + geom_point(color="darkgrey") + geom_point(aes(y=ganges.lag12)) 
  p1 <- p1 + ggtitle(paste("Highlighting the Lag 12 Correlation in \nGanges Mean-Differenced Log-Transformed Monthly Flows", 
                           timerange))
  print(p1)
}
```


<div class="scianimator">
<div id="unnamed_chunk_22" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(12);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "index_files/figure-html/unnamed-chunk-22-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_22").scianimator({
          "images": imgs,
          "delay": 100,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_22").scianimator("play");
    });
  })(jQuery);
</script>

It is clear that there is still some dependence at lag 12, and we will deal with that formally in a bit. For now let's determine if our attempts have managed to make the time series weak-sense stationary. 

#### Formal Stationarity Tests


```r
AIC =  matrix(ncol = 6, nrow = 6)
colnames(AIC) = paste('', 0:5, sep='')
rownames(AIC) = paste('', 0:5, sep='')

for(p in 0:5){
  for(q in 0:5){
    tryCatch({
      #a <- arima(tsd$value, order=c(p, 0, q))
      
      a <- Arima(ganges$L.F.D.S, lambda=0, order=c(p,0,q))
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

AIC = d

p = 0:4
d = 0:4
q = 0:4

AIC <- expand.grid(p=p, d=d, q=q)

AIC <- adply(AIC, 1, transform, AIC = Arima(ganges$L.F.D.S, order=c(p,d,q))$aic)