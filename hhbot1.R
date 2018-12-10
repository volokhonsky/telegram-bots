
setwd("~/hhbot")
library(data.table)
library(jsonlite)
library(lubridate)
print(now())
library(telegram.bot)


dates_delete<-seq.Date(today()-14,today(),by=1)

files_to_delete<-lapply(dates_delete,function(day) {
if (file.exists(paste0(day,"-vac.Rdata"))) {
load(paste0(day,"-vac.Rdata")) 
return(vacancies)  
} else return (data.table())
} )

published_vacancies<-rbindlist(files_to_delete,fill=T)
# published_vacancies<-published_vacancies[1:10]
# vacancy_search_string<-"&period=5&per_page=100"

search_vacancies<-function(vacancy_search_string) parse_vacancies_list(search_vacancies_full(vacancy_search_string))


search_vacancies_full<-function(vacancy_search_string,period=5) {
  
  vacancy_search_string<-paste0("https://api.hh.ru/vacancies?text=",gsub(" ","+",vacancy_search_string),"&per_page=100&period=",period)
  vacancy_search_string<-URLencode(iconv(vacancy_search_string,from="CP1251",to="UTF-8"))
  
  results<-list()
  while (length(results)<ifelse(length(results), results[[1]][['pages']],1)){
    results[[length(results)+1]]<-fromJSON(paste0(vacancy_search_string,"&page=",length(results)))
  }
  results
}

parse_vacancies_list <- function(vacancies_list) rbindlist(lapply(vacancies_list, function(x) data.table(id=x$items$id,name=x$items$name,published=x$items$created_at,place=x$items$area$name,salary_from=x$items$salary$from,salary_to=x$items$salary$to,salary_currency=x$items$salary$currency,employer=x$items$employer$name,url=x$items$alternate_url))) 

parse_vacancies_list <- function(vacancies_list) rbindlist(lapply(vacancies_list, function(x) data.table(id=x$items$id,name=x$items$name,published=x$items$created_at,place=x$items$area$name,salary_from=x$items$salary['from'],salary_to=x$items$salary['to'],salary_currency=x$items$salary['currency'],employer=x$items$employer$name,url=x$items$alternate_url))) 


get_vacancy<-function(vacancy_id)
{
  vacancy<-fromJSON(paste0("https://api.hh.ru/vacancies/",vacancy_id))  
  data.table(id=vacancy_id,name=vacancy$name,description=vacancy$description,place=vacancy$area$name,url=vacancy$alternate_url,created=vacancy$created_at)
}

vacancies_list<-search_vacancies_full(vacancy_search_string="R AND (data OR данны* OR машинно* OR+статистик*)",2)
vacancies<-parse_vacancies_list(vacancies_list)
vacancies<-vacancies[!published_vacancies, on=c("name","employer")]


vacancies[salary_currency.currency=="RUR",c("salary_from.from","salary_to.to"):=list(as.integer(round(salary_from.from/1000)),as.integer(round(salary_to.to/1000)))]
vacancies[salary_currency.currency=="RUR",salary_currency.currency:=" т.р."]
vacancies[salary_currency.currency=="USD",salary_currency.currency:="$"]
vacancies[salary_currency.currency=="EUR",salary_currency.currency:="€"]
# vacancies[,c("salary_from","salary_to","salary_currency"):=lapply(.SD, function(x) ifelse(is.na(x),"",x)),.SDcol=c("salary_from","salary_to","salary_currency")]
# vacancies[,salary:=paste0(salary_from,"-",salary_to,salary_currency)]
vacancies[,salary:="?"]
vacancies[!is.na(salary_from.from) & is.na(salary_to.to),salary:=paste0("от ",salary_from.from,salary_currency.currency)]
vacancies[is.na(salary_from.from) & !is.na(salary_to.to),salary:=paste0("до ",salary_to.to,salary_currency.currency)]
vacancies[!is.na(salary_from.from) & !is.na(salary_to.to),salary:=paste0(salary_from.from, " — ",salary_to.to,salary_currency.currency)]

full_vacancies_new<-rbindlist(lapply(vacancies$id,get_vacancy))
 
# save(full_vacancies,file="full_vacancies.Rdata")
fv_separate<-full_vacancies_new[grep("[^-&]\\br\\b[^-&/]|^\\br\\b[^-&/]|[^-&]\\br\\b$",description,ignore.case = T)]
vacancies<-merge(vacancies,fv_separate[,.(id)])

vacancies[,print_string:=paste0(name," | ",employer," | ",salary," | ", url,"\n")]
vacancies[,place_group:=ifelse(place %in% c("Москва","Санкт-Петербург", "Нижний Новгород", "Минск", "Киев", "Екатеринбург"),place, "Other")]
vacancies[place_group=="Other", print_string:=paste0("#",place," | ",print_string)]
vacancies[place_group=="Санкт-Петербург", place_group:="СПб"]




sent_texts<-sapply(unique(vacancies$place_group), function(p) paste0(
"#",p,"\n",paste(vacancies[place_group==p,print_string],collapse="\n")) )


updater <- Updater(token=fread("bots.csv")[botname=="vacanceRbot",token])

for(t in sent_texts) updater$bot$sendMessage(chat_id = -1001196736023, text = t)

save(vacancies,file=paste0(today(),"-vac.Rdata"))









