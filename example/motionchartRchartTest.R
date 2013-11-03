#http://www.gapminder.org/data/ Income per person (GDP/capita, PPP$ inflation-adjusted)
gdp <- read.csv("./data/gapminder_gdp_per_capita_ppp.csv")

#http://www.gapminder.org/data/ Children per woman (total fertility)
fertility <- read.csv("./data/gapminder_total_fertility.csv")
require(reshape2)
gdp.m=melt(gdp)
fertility.m=melt(fertility)
gdp.m$variable=as.numeric(as.character(sub('X','',gdp.m$variable)))
fertility.m$variable=as.numeric(as.character(sub('X','',fertility.m$variable)))
fertility.m=fertility.m[!(is.na(fertility.m$value)),]
ff=fertility.m[!(is.na(fertility.m$value)),]
names(ff)=c('Country','Year','Fertility')
rownames(ff)=NULL
gg=gdp.m[!(is.na(gdp.m$value)),]
rownames(gg)=NULL
names(gg)=c('Country','Year','GDP')
fg=merge(ff,gg,by=c('Country','Year'))

fg$Region='Unknown'
fg$Population=11000000

#----http://stackoverflow.com/a/19729235/454773
pluck_ = function (element){
  function(x) x[[element]]
}

#' Zip two vectors
zip_ <- function(..., names = F){
  x = list(...)
  y = lapply(seq_along(x[[1]]), function(i) lapply(x, pluck_(i)))
  if (names) names(y) = seq_along(y)
  return(y)
}

#' Sort a vector based on elements at a given position
sort_ <- function(v, i = 1){
  v[sort(sapply(v, '[[', i), index.return = T)$ix]
}

library(plyr)
dat2 <- dlply(fg, .(Country, Region), function(d){
  list(
    Country = d$Country[1],
    Region = d$Region[1],
    Fertility = sort_(zip_(d$Year, d$Fertility)),
    GDP = sort_(zip_(d$Year, d$GDP)),
    Population=sort_(zip_(d$Year, d$Population))
  )
})

#cat(rjson::toJSON(setNames(dat2, NULL)))

#---end

library(rCharts)
rChart <- rCharts$new()
rChart$setLib('../motionchart')
rChart$setTemplate(script = "../motionchart/layouts/motionchart_Demo.html")

rChart$set(
  
  countryHighlights='',
  yearMin=1800,
  yearMax=2005,
  
  x='Fertility',
  y='GDP',
  radius='Population',
  color='Region',
  key='Country',
  
  ymin=250 ,
  ymax=120000,
  xmin=0,
  xmax=10,
  rmin=0,
  rmax=21000000,
  
  xlabel="Fertility",
  ylabel="GDP",
  
  yscale='linear',
  xscale='linear'
)

rChart$set( data= rjson::toJSON(setNames(dat2, NULL)) )

rChart


