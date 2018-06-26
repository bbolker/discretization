
bcolours <- matrix(c(0.95,0.39,0.39),
                   ncol=3,
                   dimnames=list(c("pink"),c("R","G","B")))

## colour definitions, close to Google's colours
gcolours <- matrix(c(0.9,0.2,0.05,
                         0.1,0.4,0.95,
                         0,0.74,0.24,
                         0,0,0),
                       byrow=TRUE,
                       ncol=3,
                       dimnames=list(c("red","blue","green","black"),
                         c("R","G","B")))

gridsize <- c(1,20)
getchartdat <- function(fn="chart.png",
                        refpt=c(1880,0),
                        gridsize,    ## distance between first two x- and y- grid lines
                        invert=TRUE, ## invert y axis (hack)
                        colours=gcolours,
                        key=NULL)    ## conversion from colours to terms
  {
    if (!require(png)) cat("please install the 'png' package and try again")
    r <- readPNG(fn)
    ## white <- apply(r==1,c(1,2),all)
    ## this is considerably faster ...
    white <- (t(apply(r,1,rowSums))>2.99)
    ## note ROWS are y-axis, COLS are x-axis
    ygrid <- which(rowMeans(white)<0.1)
    xgrid <- which(colMeans(white)<0.1)
    ## distance per pixel
    ## FIXME: should use dx <- diff(xgrid); mean(dx[dx>5]) etc.
    dx <- if (length(xgrid)>0) 1 else diff(xgrid)[1]
    dy <- if (length(ygrid)>0) 1 else diff(ygrid)[1]
    gridspace <- gridsize/c(dx,dy)
    pic <- r[min(ygrid):max(ygrid),,]
    if (length(xgrid)>0) pic <- pic[,-(1:min(xgrid)),]
    if (FALSE) {
      writePNG(pic,"chart2.png") ## check
      system("display chart2.png &")
    }
    getcols <- function(i,data=pic,vthresh=0.05,debug=FALSE) {
      p1 <- pic[,i,]
      ## collapse to non-white points
      cols <- cbind(1:nrow(p1),p1)[rowSums(p1)<3,,drop=FALSE]
      if (debug) cat("**",i,dim(cols),"\n")
      ## collapse to non-grayscale points (variance among RGB above threshold)
      cols2 <- cols[apply(cols[,-1,drop=FALSE],1,var)>vthresh,,drop=FALSE]
      if (length(cols2)==0) return(data.frame(val=numeric(0),y=numeric(0)))
      ## find distances between predetermined color matrix & colors in pic
      reldists <- outer(cols2[,-1,drop=FALSE],colours,"-")^2
      coldists <- apply(reldists,c(1,3),function(x) sum(x[cbind(1:3,1:3)]^2))
      ## find closest color
      newcols <- apply(coldists,1,which.min)
      ind <- c(0,cumsum(diff(newcols)!=0))
      if (debug) cat(i,length(newcols),length(ind),"\n")
      d <- data.frame(x=i,val=tapply(newcols,ind,head,1),
                 y=tapply(cols2[,1],ind,mean))
      d$val <- rownames(colours)[d$val]
      if (!is.null(key)) d$val <- key[match(d$val,names(key))]
      d
    }
    dframe <- do.call(rbind,lapply(1:ncol(pic),getcols))
    if (invert) dframe$y <- dim(pic)[1]-dframe$y ## inverted (why?)
    dframe$y <- dframe$y*gridspace[2]+refpt[2]
    dframe$x <- dframe$x*gridspace[1]+refpt[1]
    dframe
  }

d <- getchartdat("bitcoin.png",gridsize=c(20,1),
                 colours=bcolours)
date <- seq.Date(as.Date("2017-02-05"),as.Date("2018-06-30"),
                 length=nrow(d))
dd <- data.frame(date,y=d[["y"]])

## https://en.wikipedia.org/wiki/List_of_countries_by_electricity_consumption
library(rvest)
library(dplyr)
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_electricity_consumption"
cnum <- function(x) as.numeric(gsub(",","",x))
etab <- (url
    %>% read_html()
    %>% html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/div/table')
    %>% html_table()
)
etab2 <- (etab[[1]]
    %>% setNames(c("rank","country/region","consumption","data_year","source",
                   "pop","year","energy_percap",
                   "power_percap"))
    %>% mutate(consumption=cnum(consumption)/1e9, ## convert to TWh/year
               energy_percap=cnum(energy_percap),
               pop=cnum(pop))
)

etab3 <- filter(etab2,100<consumption & consumption<max(dd$y))
##
digi_url <- "https://digiconomist.net/bitcoin-energy-consumption"
library(ggplot2)
library(ggrepel)
ggplot(dd, aes(date,y))+
    labs(x="Date",y="estimated TWh/year",
         caption=sprintf("%s\n%s",digi_url,url),
         title="Bitcoin energy consumption")+
    expand_limits(y=0)+
    theme_bw() +
    geom_label_repel(data=etab3,aes(y=consumption,label=`country/region`),
              x=sort(dd$date)[10],
              hjust=0,size=4,
              segment.color=NA)+
    geom_line(colour="red",size=2)
              
ggsave("bitcoin2.png",width=4,height=7)


d <- getchartdat("chart.png",gridsize=c(20,2e-7),
                 key=c(blue="English country dance", red="contra dance",
                   green="swing dance"))

d2 <- getchartdat("chart2.png",gridsize=c(20,5e-6),
                 key=c(blue="tango",red="square dance"))

d3 <- getchartdat("chart3.png",gridsize=c(20,2e-5),
                 key=c(blue="waltz"))

library(directlabels)
library(ggplot2)

g1 <- qplot(x,y,data=rbind(d,d2,d3),colour=val,geom="line")+theme_bw()+
  ylim(5e-8,1e-4)+scale_y_log10()+
  labs(x="Year",y="Frequency")+
  xlim(1880,2040)
png("google_ngram_hack.png",width=720)
print(direct.label(g1,"last.points"))
dev.off()



## could try to get it directly by constructing the URL for the search term: constructing
##  the URL is easy, but I'm not sure how to extract/save the PNG programatically
## http://ngrams.googlelabs.com/graph?content=English+country+dance%2Ccontra+dance%2Cswing+dance&year_start=1880&year_end=2008&corpus=0&smoothing=3
