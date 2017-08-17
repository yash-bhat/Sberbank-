library(ggplot2)
library(scales)
library(corrplot)
library(psych)
library(gplots)
library(vcd)

#plot data frame missing values
library("VIM")
aggr(macro)
library(Amelia)
missmap(macro)
missmap(macro, legend = TRUE, col = c("navyblue","cornsilk"), main = "NAs in Macro",
        y.labels="frequency", y.at=1,
        y.cex = 0.8, x.cex = 0.8, csvar = NULL, tsvar =
          NULL, rank.order = TRUE)


#histogram
#1 macro
ggplot(data=macro, aes(aftermacro$ppi)) + geom_histogram()

#2 train
ggplot(data=train, aes(train$raion_popul)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_bw() +
  xlab("Raoin population") +
  ylab("Frequency")+
  ggtitle("TRAIN") +
  theme(text=element_text(size=15))+
  geom_histogram(col="black", 
                 fill="gold",
                 aes(fill="...count..."))

#1 test
ggplot(data=test, aes(train$id)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_bw() +
  ggtitle("TRAIN") +
  theme(text=element_text(size=15))+
  geom_histogram(col="black", 
            
                 aes(fill=..count..))

#
plot(beforemacro,beforemacro$ppi,type="l",col="red")
lines(aftermacro,aftermacro$ppi,col="green")
#
plot.ts(beforemacro)


plot(beforemacro, beforemacro$ppi, type='l', xlab='t /s', ylab='s1')
points(aftermacro, ppi, type='l')


install.packages("igraph")
library(igraph)
as_data_frame(g, what = c("edges", "vertices", "both"))

g=graph_from_data_frame(beforemacro, directed = TRUE, vertices = NULL)




for ( i in seq(1,length( beforemacro ),1) ) plot(beforemacro[,i],
                                                 ylab=names(beforemacro[i]),type="l")
for ( i in seq(1,length( aftermacro ),1) ) plot(aftermacro[,i],
                                                 ylab=names(aftermacro[i]),type="l")

df <- data.frame(time = 1:10,
                 a = cumsum(rnorm(10)),
                 b = cumsum(rnorm(10)),
                 c = cumsum(rnorm(10)))
df <- melt(df ,  id.vars = 'time', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(time,value)) + geom_line(aes(colour = series))

plot.ts(beforemacro[1:10])
plot.ts(aftermacro[1:10])


#multiple columns histogram
d <- melt(diamonds[,-c(2:4)])
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()



ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = macro,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(macro)

library(VIM)


ggplot(data = df, 
       aes(x = (df$area_m/df$max_floor), y = price_doc)) + geom_point(alpha = 0.4, color = "#1E555C") +
  labs(title = "", x = "Area by Floor", y = "Price")






df$room_per_floor <- df$num_room/df$max_floor


ggplot(data = subset(df, df$room_per_floor < quantile(df$room_per_floor, 0.99, na.rm = T)), 
       aes(x = room_per_floor, y = price_doc)) + geom_jitter(alpha = 0.5, color = "maroon") +
  labs(title = "Room Per Floor", x = "Room Per Floor", y = "Price")


library(lattice)
#Build the horizontal and vertical axis information
hor <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
ver <- paste("Variabl1", hor, sep="")

#Build the fake correlation matrix
nrowcol <- length(ver)
cor <- matrix(runif(nrowcol*nrowcol, min=0.4), nrow=nrowcol, ncol=nrowcol, 
              dimnames = list(hor, ver))
for (i in 1:nrowcol) cor[i,i] = 1

#Build the plot
rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
levelplot(cor, main="DF correlation matrix", xlab="", 
          ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))



library(corrplot)
df_num = cor(df_num)
temp=df_num[1:10]
colnames(temp) = c("X","gdp","gdp_growth","ppi","cpi","trade",
                   "t_growth","eurub","brent","capital ")
temp = cor(temp)

corrplot(temp, method="circle", cl.lim=c(-50, 100))
corrplot(temp,method="square")
y=data.frame(pred1)

t.test(df_num)
x=data.frame(log(df$price_doc[1:7662]))
plot(x, y , xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)
