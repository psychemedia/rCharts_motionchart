#World Bank Indicators demo
require(WDI)
require(rCharts)

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

#cat(rjson::toJSON(setNames(dat2, NULL)))

#---end


#---- https://code.google.com/p/google-motion-charts-with-r/source/browse/trunk/demo/WorldBank.R?r=286
getWorldBankCountries <- function(){
  require(RJSONIO)
  wbCountries <- fromJSON("http://api.worldbank.org/country?per_page=300&format=json")
  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub("\\(all income levels\\)",
                                           "", levels(wbCountries$region.value))
  return(wbCountries)
}
#----

params=list(
  start=1950,
  end=2010,
  x='Fertility',
  y='GDP',
  radius='Population',
  color='Region',
  key='Country',
  yscale='log',
  xscale='linear',
  rmin=0,
  xmin=0

  )

getdata.motionchart=function(countries=c("BD",'GB')){
  
  tmp=getWorldBankCountries()[,c('iso2Code','region.value')]
  names(tmp)=c('iso2Code','Region')
  
  data <- WDI(indicator=c('SP.DYN.TFRT.IN','SP.POP.TOTL','NY.GDP.PCAP.CD'),start = params$start, end = params$end,country=countries)
  names(data)=c('iso2Code','Country','Year','Fertility','Population','GDP')
  
  data=merge(data,tmp,by='iso2Code')
  dlply(data, .(Country, Region), function(d){
    list(
      Country = d$Country[1],
      Region = d$Region[1],
      Fertility = sort_(zip_(d$Year, d$Fertility)),
      GDP = sort_(zip_(d$Year, d$GDP)),
      Population=sort_(zip_(d$Year, d$Population))
    )
  })
}

dat2=getdata.motionchart()

#---


paramsTidy=function(params){
  if (!('ymin' %in% names(params))) params$ymin= signif(min(0.9*data[[params$y]]),3)
  if (!('ymax' %in% names(params))) params$ymax= signif(max(1.1*data[[params$y]]),3)
  if (!('xmin' %in% names(params))) params$xmin= signif(min(0.9*data[[params$x]]),3)
  if (!('xmax' %in% names(params))) params$xmax= signif(max(1.1*data[[params$x]]),3)
  if (!('rmin' %in% names(params))) params$rmin= signif(min(0.9*data[[params$radius]]),3)
  if (!('rmax' %in% names(params))) params$rmax= signif(max(1.1*data[[params$radius]]),3)
  params
}

params=paramsTidy(params)

#Suggestion to use the same motionchart interface as googleVis
#gvisMotionChart
#function (data, idvar = "id", timevar = "time", xvar = "", yvar = "", 
#          colorvar = "", sizevar = "", date.format = "%Y/%m/%d", options = list(), 
#          chartid) 
rChart.generator=function(params, h=400,w=800){
  rChart <- rCharts$new()
  rChart$setLib('../motionchart')
  rChart$setTemplate(script = "../motionchart/layouts/motionchart_Demo.html")

  rChart$set(
  
   countryHighlights='',
   yearMin= params$start,
   yearMax=params$end,
  
   x=params$x,
   y=params$y,
   radius=params$radius,
   color=params$color,
   key=params$key,
  
   ymin=params$ymin,
   ymax=params$ymax,
   xmin=params$xmin,
   xmax=params$xmax,
   rmin=params$rmin,
   rmax=params$rmax,
  
   xlabel=params$x,
   ylabel=params$y,
  
   yscale=params$yscale,
   xscale=params$xscale,
   
   width=w,
   height=h
 )

 rChart$set( data= rjson::toJSON(setNames(dat2, NULL)) )

 rChart
}

rChart.generator(params,w=1000,h=600)