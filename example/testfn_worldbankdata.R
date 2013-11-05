require(WDI)

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


getdata.WDI=function(countries=c("BD",'GB'),start=1950,end=2012){
  tmp=getWorldBankCountries()[,c('iso2Code','region.value')]
  names(tmp)=c('iso2Code','Region')
  
  data <- WDI(indicator=c('SP.DYN.TFRT.IN','SP.POP.TOTL','NY.GDP.PCAP.CD'),start = start, end = end,country=countries)
  names(data)=c('iso2Code','Country','Year','Fertility','Population','GDP')
  
  merge(data,tmp,by='iso2Code')
}


#dat2=getdata.WDI()
#dat2=getdata.WDI(c('GB','US','ES','BD'))