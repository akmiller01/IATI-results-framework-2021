list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-results-framework-2021/output")

# dat2019 = fread("total_spend_2019_by_publisher.csv")
# 
# dat2019$bin = "<=1M"
# dat2019$bin[which(dat2019$total.spend<=1000000)] = "<=1M"
# dat2019$bin[which(dat2019$total.spend>1000000 & dat2019$total.spend<=10000000)] = "> 1M & <= 10M"
# dat2019$bin[which(dat2019$total.spend>10000000 & dat2019$total.spend<=100000000)] = "> 10M & <= 100M"
# dat2019$bin[which(dat2019$total.spend>100000000 & dat2019$total.spend<=1000000000)] = "> 100M & <= 1B"
# dat2019$bin[which(dat2019$total.spend>1000000000)] = "> 1B"
# 
# dat2019$bin = factor(
#   dat2019$bin,
#   levels=c(
#     "<=1M",
#     "> 1M & <= 10M",
#     "> 10M & <= 100M",
#     "> 100M & <= 1B",
#     "> 1B"
#   )
# )
# 
# ggplot(dat2019,aes(x=bin)) + geom_bar()
# 
# xml_files = list.files(path="~/git/IATI-Better-Refresher/data",full.names=T)
# dat_list = list()
# dat_index = 1
# pb = txtProgressBar(max=length(xml_files),style=3)
# for(idx in 1:length(xml_files)){
#   file_dat_list = list()
#   file_dat_index = 1
#   setTxtProgressBar(pb,idx)
#   xml_file = xml_files[idx]
#   xml_dat <- tryCatch(
#     xmlParse(xml_file),
#     error=function(e) e
#   )
#   
#   if(inherits(xml_dat, "error")){next}
#   activities = getNodeSet(xml_dat, "//iati-activity")
#   for(activity in activities){
#     iati_identifier = sapply(getNodeSet(activity,"./iati-identifier"),xmlValue)
#     if(length(iati_identifier)==0){
#       iati_identifier = NA
#     }
#     default_currency = xmlGetAttr(activity,"default-currency")
#     if(length(default_currency)==0){
#       default_currency = NA
#     }
#     reporting_org_ref = NA
#     reporting_org_elem = getNodeSet(activity,"./reporting-org")
#     if(length(reporting_org_elem)>0){
#       reporting_org_attrs = xmlAttrs(reporting_org_elem[[1]])
#       if("ref" %in% names(reporting_org_attrs)){
#         reporting_org_ref = reporting_org_attrs[["ref"]]
#       }
#     }
#     transactions = getNodeSet(activity,"./transaction")
#     if(length(transactions)>0){
#       for(transaction in transactions){
#         if(length(getNodeSet(transaction,"./transaction-type/@code"))==0){
#           t_type = NA
#         }else{
#           t_type = getNodeSet(transaction,"./transaction-type/@code")[[1]][["code"]]
#         }
#         
#         if(length(getNodeSet(transaction,"./transaction-date/@iso-date"))==0){
#           t_date = NULL
#         }else{
#           t_date = getNodeSet(transaction,"./transaction-date/@iso-date")[[1]][["iso-date"]]
#         }
#         t_value_elem = getNodeSet(transaction,"./value")
#         if(length(t_value_elem)>0){
#           t_value = as.numeric(gsub(",","",sapply(t_value_elem,xmlValue)))
#           t_currency = sapply(t_value_elem,xmlGetAttr,"currency")[[1]]
#           if(length(t_currency)==0){
#             t_currency = default_currency
#           }
#           t_value_date = sapply(t_value_elem,xmlGetAttr,"value-date")[[1]]
#           if(length(t_date)==0){
#             t_date = t_value_date
#           }
#           if(length(t_date)==0){
#             t_date = NA
#           }
#           if(!is.na(t_date) & !is.na(t_value) & !is.na(t_currency) & !is.na(reporting_org_ref) &
#              t_date!="" & t_value!="" & t_currency!="" & reporting_org_ref!=""){
#             file_dat_list[[file_dat_index]] = data.frame(iati_identifier,reporting_org_ref,type=t_type,date=t_date,value=t_value,currency=t_currency)
#             file_dat_index = file_dat_index + 1
#           }
#           rm(t_type,t_date,t_value_elem,t_value,t_currency)
#         }
#       }
#       rm(iati_identifier,default_currency,reporting_org_elem,reporting_org_attrs,reporting_org_ref,transactions)
#     }
#   }
#   rm(xml_dat)
#   file_dat = rbindlist(file_dat_list)
#   if(nrow(file_dat)>0){
#     file_dat$year = as.numeric(substr(file_dat$date,1,4))
#     file_tab = file_dat[,.(value=sum(value,na.rm=T)),by=.(reporting_org_ref,type,year,currency)]
#     dat_list[[dat_index]] = file_tab
#     dat_index = dat_index + 1
#   }
# }
# close(pb)
# 
# dat = rbindlist(dat_list)
# dat_tab = dat[,.(value=sum(value,na.rm=T)),by=.(reporting_org_ref,type,year,currency)]
# save(dat_tab,file="results_framework_transactions.RData")
load("results_framework_transactions.RData")
dat_tab$currency = gsub(" ","",toupper(dat_tab$currency))
dat_tab$currency[which(dat_tab$currency=="BEF")] = "EUR"
dat_tab$currency[which(dat_tab$currency=="GIP")] = "GBP"
dat_tab$currency[which(dat_tab$currency=="AON")] = "AOA"
dat_tab$currency[which(dat_tab$currency=="USS")] = "USD"
dat_tab$currency[which(dat_tab$currency=="FKP")] = "GBP"
dat_tab$currency[which(dat_tab$currency=="ZMK")] = "ZMW"
dat_tab$currency[which(dat_tab$currency=="USN")] = "USD"
dat_tab$currency[which(dat_tab$currency=="FIM")] = "EUR"
dat_tab$currency[which(dat_tab$currency=="EURO")] = "EUR"
dat_tab$currency[which(dat_tab$currency=="GHC")] = "GHS"

ex_rates = fread("../ex_rates.csv")
setnames(ex_rates,"cc","currency")
setdiff(unique(dat_tab$currency),unique(ex_rates$currency))
dat_tab = merge(dat_tab,ex_rates,by=c("year","currency"))
dat_tab = subset(dat_tab,ex.rate>0 & type %in% c(
  "3",
  "4",
  "D",
  "E",
  "Disbursement",
  "Expenditure"
))
dat_tab$value_usd = dat_tab$value / dat_tab$ex.rate
publisher_tab = dat_tab[,.(value_usd=sum(value_usd,na.rm=T)),by=.(year,reporting_org_ref)]
publisher_tab = publisher_tab[order(publisher_tab$reporting_org_ref,-publisher_tab$year),]
fwrite(publisher_tab,"IATI_publishers_by_spend_10012021.csv")