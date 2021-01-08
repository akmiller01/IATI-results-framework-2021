list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-results-framework-2020/output")

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

xml_files = list.files(path="~/git/IATI-Better-Refresher/data",full.names=T)
dat_list = list()
dat_index = 1
pb = txtProgressBar(max=length(xml_files),style=3)
for(idx in 1:length(xml_files)){
  setTxtProgressBar(pb,idx)
  xml_file = xml_files[idx]
  xml_dat <- tryCatch(
    xmlParse(xml_file),
    error=function(e) e
  )
  
  if(inherits(xml_dat, "error")){next}
  activities = getNodeSet(xml_dat, "//iati-activity")
  for(activity in activities){
    iati_identifier = sapply(getNodeSet(activity,"./iati-identifier"),xmlValue)
    if(length(iati_identifier)==0){
      iati_identifier = NA
    }
    default_currency = xmlGetAttr(activity,"default-currency")
    if(length(default_currency)==0){
      default_currency = NA
    }
    reporting_org_elem = getNodeSet(activity,"./reporting-org")
    reporting_org_attrs = sapply(reporting_org_elem,xmlAttrs)
    if(length(reporting_org_attrs)>0){
      if("ref" %in% names(reporting_org_attrs)){
        reporting_org_ref = reporting_org_attrs[["ref"]]
      }else{
        reporting_org_ref = sapply(reporting_org_elem,xmlValue)
      }
    }else{
      reporting_org_ref = sapply(reporting_org_elem,xmlValue)
    }
    if(length(reporting_org_ref)==0){
      reporting_org_ref = NA
    }
    transactions = getNodeSet(activity,"./transaction")
    if(length(transactions)>0){
      for(transaction in transactions){
        if(length(getNodeSet(transaction,"./transaction-type/@code"))==0){
          t_type = NA
        }else{
          t_type = getNodeSet(transaction,"./transaction-type/@code")[[1]][["code"]]
        }
        
        if(length(getNodeSet(transaction,"./transaction-date/@iso-date"))==0){
          t_date = NULL
        }else{
          t_date = getNodeSet(transaction,"./transaction-date/@iso-date")[[1]][["iso-date"]]
        }
        t_value_elem = getNodeSet(transaction,"./value")
        if(length(t_value_elem)>0){
          t_value = as.numeric(gsub(",","",sapply(t_value_elem,xmlValue)))
          t_currency = sapply(t_value_elem,xmlGetAttr,"currency")[[1]]
          if(length(t_currency)==0){
            t_currency = default_currency
          }
          t_value_date = sapply(t_value_elem,xmlGetAttr,"value-date")[[1]]
          if(length(t_date)==0){
            t_date = t_value_date
          }
          if(length(t_date)==0){
            t_date = NA
          }
          dat_list[[dat_index]] = data.frame(iati_identifier,type=t_type,date=t_date,value=t_value,currency=t_currency)
          dat_index = dat_index + 1
          rm(t_type,t_date,t_value_elem,t_value,t_currency)
        }
      }
      rm(iati_identifier,default_currency,reporting_org_elem,reporting_org_attrs,reporting_org_ref,transactions)
    }
  }
  rm(xml_dat)
}
close(pb)

dat = rbindlist(dat_list)
save(dat,file="results_framework_transactions.RData")
