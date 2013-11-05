source('testfn_rChartMotionChart.R')
source('testfn_worldbankdata.R')

test.data=getdata.WDI(c('GB','US','ES','BD'))
rChartMotionChart(test.data,'Country','Year','Fertility','GDP','Region','Population')
rChartMotionChart(data,'Country','Year','Fertility','GDP','Region','Population')