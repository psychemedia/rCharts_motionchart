library(plyr)
require(rCharts)

#gvisMotionChart
#function (data, idvar = "id", timevar = "time", xvar = "", yvar = "", 
#          colorvar = "", sizevar = "", date.format = "%Y/%m/%d", options = list(), 
#          chartid) 
#rChartMotionChart(data,'Country','Year','Fertility','GDP','Region','Population')
rChartMotionChart =function (data, idvar = "id", timevar = "time", xvar = "", yvar = "", 
          colorvar = "", sizevar = "", date.format = "%Y/%m/%d", options = list(), 
          chartid) {
  #rChartMotionChart(dat2,'Country',)
  params=list(
    start=1950,
    end=2010,
    x=xvar,
    y=yvar,
    radius=sizevar,
    color=colorvar,
    key=idvar,
    yscale='log',
    xscale='linear',
    rmin=0,
    xmin=0,
    timevar=timevar
  )
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
  dat2=rChart.dataFormatter(data,idvar, timevar, xvar, yvar,colorvar, sizevar)
  rChart.generator(dat2,params, h=400,w=800)
}

#rChart.dataFormatter(data,'Country','Year','Fertility','GDP','Region','Population')
rChart.dataFormatter =function(data, idvar = "id", timevar = "time", xvar = "", yvar = "", 
                               colorvar = "", sizevar = "", date.format = "%Y/%m/%d"){
  
  data=data[,c(idvar,timevar,xvar,yvar,colorvar,sizevar)]
  
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
  
  dlply(data, .(get(idvar),get(colorvar)), function(d){
    ll=list(
      key= d[[idvar]][1],
      color= d[[colorvar]][1],
      x=sort_(zip_(d[[timevar]], d[[xvar]])),
      y=sort_(zip_(d[[timevar]], d[[yvar]])),
      radius=sort_(zip_(d[[timevar]], d[[sizevar]]))
      )
    names(ll)=c(idvar,colorvar,xvar,yvar,sizevar)
    ll
    })

}

rChart.generator=function(data,params, h=400,w=800){
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
  
  rChart$set( data= rjson::toJSON(setNames(data, NULL)) )
  
  rChart
}

#rChartMotionChart(data,'Country','Year','Fertility','GDP','Region','Population')