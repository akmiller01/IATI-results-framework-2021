list.of.packages <- c("data.table","readr","reshape2","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/IATI-results-framework-2021")

dat <- read.csv("WEOOct2020all.xls",sep="\t",na.strings=c("","n/a","--"),fileEncoding='utf-16',check.names=T)
dat = data.table(dat)
dat = subset(dat,WEO.Subject.Code %in% c("NGDP","NGDPD"))
keep = c("ISO","Country","Units",paste0("X",1980:2025))
dat = dat[,keep,with=F]

mdat <- melt(dat,id.vars=c("ISO","Country","Units"))
mdat$year = as.numeric(substr(mdat$variable,2,5))
mdat <- mdat[complete.cases(mdat),]
mdat$value = as.numeric(gsub(",","",mdat$value))
wdat <- dcast(mdat,ISO+Country+year~Units)
wdat <- wdat[complete.cases(wdat),]
names(wdat) <- make.names(names(wdat))
wdat$ex.rate <- wdat$U.S..dollars/wdat$National.currency

ccs = fread("currency_codes.csv")
ccs = subset(ccs, ISO!="KOS") # Duplicate

setdiff(ccs$ISO,wdat$ISO)
setdiff(wdat$ISO,ccs$ISO)

ccs = subset(ccs,is.na(duplicate))
keep = c("ISO","cc")
ccs = ccs[,keep,with=F]

wdat = merge(wdat,ccs,by="ISO")

# Bitcoin transaction in 2015
xbt_df = data.frame(cc="XBT",year=2015,ex.rate=230.54)
wdat = rbindlist(list(wdat,xbt_df),fill=T)

# XDR
xdr = fread("xdr.csv")
setnames(xdr,"XDR","ex.rate")
xdr$ex.rate = 1/xdr$ex.rate
xdr$ex.rate[which(!is.finite(xdr$ex.rate))] = 0
xdr$cc = "XDR"
wdat = rbind(wdat,xdr,fill=T)

wdat = wdat[order(wdat$cc,wdat$year),]

ex_list = list()

for(this.cc in unique(wdat$cc)){
  sub_list = list()
  wdat_sub = subset(wdat,cc==this.cc)
  for(i in 1:nrow(wdat_sub)){
    sub_list[as.character(wdat_sub[i,"year"])] = wdat_sub[i,"ex.rate"]
  }
  ex_list[[this.cc]] = sub_list
}

fwrite(wdat[,c("year","cc","ex.rate"),with=F],"ex_rates.csv")
