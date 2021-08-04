library(data.table)
library(telegram.bot)
library(rio)

expired_passports<-fread("list_of_expired_passports.csv.bz2")
setkey(expired_passports,PASSP_NUMBER,PASSP_SERIES)

podpisi<-data.table(import("Подписи.xlsx",skip=1))
setnames(podpisi,c("Серия","Номер"),c("PASSP_SERIES","PASSP_NUMBER"))

expired_data<-merge(expired_passports,podpisi)


test_pasp<-c("4005","482961")

check_pasp<-function(test_pasp) {
  res<-expired_passports[PASSP_SERIES==test_pasp[1] & PASSP_NUMBER==test_pasp[2]]
  if (nrow(res)>0) "ПАСПОРТ НЕДЕЙСТВИТЕЛЕН" else "в списке недействительных не обнаружено"
}

check_pasp(c("0398","165060"))


## tg bot


token<-"1949109927:AAER0llsFX4M1I-idbHyKbc7fPa03pg-Hd0"


updater <- Updater(token=token)




start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text = "Hello Creator!")
}


dispatcher <- updater$dispatcher

start_handler <- CommandHandler('start', start)

dispatcher$add_handler(start_handler)

send_pasp<-function(bot, update){
  test_pasp<-update$message$text
    fname<-paste0(update$message$chat_id,".txt")
    if(!file.exists(fname)) file.create(fname)
    # f<-file(fname,"a")
    write(test_pasp,file=fname,append=TRUE)
    pasp<-strsplit(test_pasp," ")[[1]]
    write(pasp,file=fname,append=TRUE)
    reply<-check_pasp(pasp)
    if(!is.null(reply))  bot$sendMessage(chat_id = update$message$chat_id, text = reply)
}

# test_pasp<-"0399 165060"

pasp_handler <- MessageHandler(send_pasp, MessageFilters$text)
dispatcher$add_handler(pasp_handler)
updater$start_polling()

