list.of.packages <- c("data.table", "jsonlite","reshape2","XML","httr","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/IATI-results-framework-2021"

setwd(wd)

# Download duplicate reporting_org_ref in data table, split
# table_url = "http://dashboard.iatistandard.org/reporting_orgs.html"
# table_url_src = content(GET(table_url))
# table_doc = htmlParse(table_url_src)
# pub_tab = readHTMLTable(table_doc)[[1]]
# names(pub_tab) = make.names(names(pub_tab))
# pub_tab$Reporting.Org.on.Registry = as.character(pub_tab$Reporting.Org.on.Registry)
# pub_tab$Reporting.Org.on.Registry[which(pub_tab$Reporting.Org.on.Registry=="NL-KVK-34200988 (deprecated)")] = "NL-KVK-34200988"
# pub_tab$Reporting.Org.on.Registry[which(pub_tab$Reporting.Org.on.Registry=="US-EIN 81-2339233")] = "US-EIN-81-2339233"
# pub_tab = pub_tab[,c("Reporting.Org.on.Registry","Reporting.Orgs.in.Data")]
# pub_tab_split = cSplit(pub_tab,"Reporting.Orgs.in.Data",sep=" ")
# new_names = names(pub_tab_split)[which(names(pub_tab_split)!="Reporting.Org.on.Registry")]
# pub_tab_split_long = reshape(pub_tab_split, varying=new_names, direction="long", sep="_")
# pub_tab_split_long[,c("time","id")] = NULL
# pub_tab_split_long=subset(pub_tab_split_long, !is.na(Reporting.Orgs.in.Data) & Reporting.Orgs.in.Data!="null")
# fwrite(pub_tab_split_long,"publisher_data_names.csv")
pub_tab_split_long = fread("publisher_data_names.csv")

# Load publisher table from registry, use to create full publisher reporting_org_ref table
org_ids = fread("iati-publishers.csv")
org_ids = unique(org_ids[,c("IATI Organisation Identifier","publisher")])
setnames(org_ids,"IATI Organisation Identifier","Reporting.Org.on.Registry")
single_orgs = setdiff(org_ids$Reporting.Org.on.Registry,pub_tab_split_long$Reporting.Org.on.Registry)
single_org_df = data.frame(Reporting.Org.on.Registry=single_orgs, Reporting.Orgs.in.Data=single_orgs)
pub_tab_split_long = rbind(pub_tab_split_long, single_org_df)
setnames(pub_tab_split_long, "Reporting.Orgs.in.Data", "reporting_org_ref")

# Fix duplicate publishers claiming same reporting_org_ref
pub_tab_split_long = subset(
  pub_tab_split_long,
  !(Reporting.Org.on.Registry=="BJ-IFU-6201408024805" & reporting_org_ref=="BJ-IFU-4201710012551") &
    !(Reporting.Org.on.Registry=="US-EIN-23-1352010" & reporting_org_ref=="NL-KVK-41149831") &
    !(Reporting.Org.on.Registry=="NL-KVK-27327087" & reporting_org_ref=="NL-KVK-41150939") &
    !(Reporting.Org.on.Registry=="NL-KVK-70292361" & reporting_org_ref=="NL-KVK-53744993")
)

# Load total spend, recalculate categories, merge publishers
org_categories = fread("output/IATI_publishers_by_spend_10012021.csv")
org_categories = merge(org_categories, pub_tab_split_long, by="reporting_org_ref", all.x=T)
org_categories$Reporting.Org.on.Registry = as.character(org_categories$Reporting.Org.on.Registry)
org_categories$reporting_org_ref = as.character(org_categories$reporting_org_ref)
org_categories$Reporting.Org.on.Registry[which(is.na(org_categories$Reporting.Org.on.Registry))] = org_categories$reporting_org_ref[which(is.na(org_categories$Reporting.Org.on.Registry))]
org_categories = org_categories[,.(value_usd=sum(value_usd,na.rm=T)),by=.(Reporting.Org.on.Registry, year)]
org_categories$category = "<=1M"
org_categories$category[which(org_categories$value_usd<=1000000)] = "<=1M"
org_categories$category[which(org_categories$value_usd>1000000 & org_categories$value_usd<=10000000)] = "> 1M & <= 10M"
org_categories$category[which(org_categories$value_usd>10000000 & org_categories$value_usd<=100000000)] = "> 10M & <= 100M"
org_categories$category[which(org_categories$value_usd>100000000 & org_categories$value_usd<=1000000000)] = "> 100M & <= 1B"
org_categories$category[which(org_categories$value_usd>1000000000)] = "> 1B"

org_categories = subset(org_categories,year %in% c(2018,2019,2020))
org_cat_max = data.table(org_categories)[,.(category=category[which.max(.SD$value_usd)],year=year[which.max(.SD$value_usd)]),by=.(Reporting.Org.on.Registry)]
org_cat_max = merge(org_cat_max, org_ids, by="Reporting.Org.on.Registry",all=T)
org_cat_max$category[which(is.na(org_cat_max$category))] = "<=1M"


# test_url = "https://iativalidator.iatistandard.org/api/v1/stats?date=2021-01-11"
test_url = "http://stage.iativalidator.iatistandard.org/api/v1/stats?date=2020-12-31"

test_json = fromJSON(test_url)
publishers = test_json$publisher
meta.mess.list = list()
meta.sum.list = list()
meta.index = 1
pb = txtProgressBar(max=length(publishers),style=3)
for(test_publisher in publishers){
  setTxtProgressBar(pb, meta.index)
  row = subset(test_json,publisher==test_publisher)
  messStats = row$messageStats
  messCodes = names(messStats)
  messList = list()
  messIndex = 1
  for(messCode in messCodes){
    tmp.df = data.frame(code=messCode,text=messStats[,messCode]$text,count=messStats[,messCode]$count)
    messList[[messIndex]] = tmp.df
    messIndex = messIndex + 1
  }
  messDf = rbindlist(messList)
  messDf = subset(messDf,!is.na(count))
  messDf$publisher = test_publisher
  sumStats = row$summaryStats
  sumDf = data.frame(names(sumStats),unlist(t(sumStats)))
  names(sumDf) = c("name","count")
  sumDf$publisher = test_publisher
  meta.mess.list[[meta.index]] = messDf
  meta.sum.list[[meta.index]] = sumDf
  meta.index = meta.index + 1
}
close(pb)
all.messages = rbindlist(meta.mess.list)
all.messages = all.messages[order(all.messages$publisher),]
all.summaries = rbindlist(meta.sum.list)
setnames(all.summaries,"count","value")
all.summaries.wide = dcast(all.summaries,publisher~name)
fwrite(all.messages,"output/all_messages.csv")
fwrite(all.summaries.wide,"output/all_summaries.csv")
