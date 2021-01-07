list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-results-framework-2020/output")

dat2019 = fread("total_spend_2019_by_publisher.csv")

dat2019$bin = "<=1M"
dat2019$bin[which(dat2019$total.spend<=1000000)] = "<=1M"
dat2019$bin[which(dat2019$total.spend>1000000 & dat2019$total.spend<=10000000)] = "> 1M & <= 10M"
dat2019$bin[which(dat2019$total.spend>10000000 & dat2019$total.spend<=100000000)] = "> 10M & <= 100M"
dat2019$bin[which(dat2019$total.spend>100000000 & dat2019$total.spend<=1000000000)] = "> 100M & <= 1B"
dat2019$bin[which(dat2019$total.spend>1000000000)] = "> 1B"

dat2019$bin = factor(
  dat2019$bin,
  levels=c(
    "<=1M",
    "> 1M & <= 10M",
    "> 10M & <= 100M",
    "> 100M & <= 1B",
    "> 1B"
  )
)

ggplot(dat2019,aes(x=bin)) + geom_bar()

t_types = c("C","c","2","D","d","3")

xml_files = list.files(path="~/git/IATI-Better-Refresher/data",full.names=T)
dat_list = list()
dat_index = 1
pb = txtProgressBar(max=length(xml_files),style=3)
for(idx in 1:length(xml_files)){
  setTxtProgressBar(pb,idx)
  xml_file = xml_files[idx]
  
  try(
    {
      xml_dat = xmlParse(xml_file)
      activities = getNodeSet(xml_dat, "//iati-activity")
      for(activity in activities){
        iati_identifier = sapply(getNodeSet(activity,"./iati-identifier"),xmlValue)
        if(length(iati_identifier)==0){
          iati_identifier = NA
        }
        reporting_org_ref	= getNodeSet(activity,"./reporting-org/@ref")[[1]][["ref"]]
        transactions = getNodeSet(activity,"./transaction")
        if(length(transactions)>0){
          for(transaction in transactions){
            t_type = getNodeSet(transaction,"./transaction-type/@code")[[1]][["code"]]
            t_date = getNodeSet(transaction,"./transaction-date/@iso-date")[[1]][["iso-date"]]
            t_value = sapply(getNodeSet(transaction,"./value"),xmlValue)
            dat_list[[dat_index]] = data.frame(iati_identifier,transaction="Transaction",type=t_type,date=t_date,value=t_value)
            dat_index = dat_index + 1
          }
        }
        budgets = getNodeSet(activity,"./budget")
        if(length(budgets)>0){
          for(budget in budgets){
            b_type = xmlGetAttr(budget,"type")
            b_date = getNodeSet(budget,"./period-start/@iso-date")[[1]][["iso-date"]]
            b_value = sapply(getNodeSet(budget,"./value"),xmlValue)
            dat_list[[dat_index]] = data.frame(iati_identifier,transaction="Budget",type=b_type,date=b_date,value=b_value)
            dat_index = dat_index + 1
          }
        }
      }
    }
  )
  
}
close(pb)

sum_dat = rbindlist(dat_list)
