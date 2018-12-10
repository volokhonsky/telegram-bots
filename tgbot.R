# devtools::install_github("ebeneditos/telegram.bot")
setwd("/home/rstudio/tgbot")
library(data.table)
library(telegram.bot)
library(udpipe)
# udpipe_download_model("russian-syntagrus")
model<-udpipe_load_model("/home/rstudio/russian-syntagrus-ud-2.0-170801.udpipe")

bot_replies<-c("От бота слышу", "КТО ЗДЕСЬ БОТ???", "@InYourPantsBot, похоже, тебя вычислили", "Ты что-то имеешь против ботов?", "В штанах у себя ботов поищи", "Нелепые подозрения, разве не очевидно, что я живой", "Да, я бот. Чего же боле?")


spasibo_replies<-c("Спасибо не булькает!", "Мне кажется, одним спасибо тут не отделаешься...")

dplyr_replies<-c("Ну какой же отвратительный код!", "Если бы вы выучили data.table, ваша жизнь была бы иной", "Этот код... ну... как будто его писали на кальмаРе!", "Какая гадость этот ваш dplyr!")
ggplot_replies<-c("Поправьте легенду, попробуйте её передвинуть на 50 пикселей выше!", "Может, цвета ещё немного поменять?", "БОЖЕ! НА ЭТОМ ГРАФИКЕ КАЛЬМАР! ОГРОМНЫЙ СТРАШНЫЙ КАЛЬМАР!", "Вы тоже видите кальмара на этом графике?")
dt_replies<-c("О-о, код на data.table! Хорошая попытка!","Может, попробовать перевести данные в узкий формат? Используйте melt", "Добавьте ещё calmaR:='beautiful'")
r_replies<-c("О, вы опять кодите в чате?", "Где-то в этом коде ошибка", "Надо бы переписать этот код", "R - отличный язык")


updater <- Updater(token=fread("bots.csv")[botname=="andan1_bot",token])




start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text = "Hello Creator!")
}


dispatcher <- updater$dispatcher

start_handler <- CommandHandler('start', start)

dispatcher$add_handler(start_handler)

swim_replies<-c("Утонете, на мастерскую не возвращайтесь", "Опять эти предсказуемые кожаные ублюдки сейчас попрутся на реку...", "У нас что, мастерская по плаванию?", "За буйки не заплывать!", "Хочу ваш фотосет с пляжа","Берегите свои кальмары и морские огурцы!","Посмотрите, нет ли там на Волге острогрудых челнов...","Чтобы не было раздора\n
Между вольными людьми,\n
Волга, Волга, мать родная,\n
На, красавицу прими!")

foods<-fread("food.csv")
foods[,new:=tolower(food)]

food_replies<-c("есть мнение - на вкус как кальмар", "кожаные ублюдки опять собрались пожрать",
"Сколько можно жрать!", "мне тоже возьмите!", "в штанах своих поищи" )


an_dan<-function(text)
{
  text<-tolower(iconv(text,to="UTF8"))
  udpipe_text<-as.data.table(udpipe_annotate(model,text))
  if (sum(foods$new %in% udpipe_text$lemma)>0) return(paste0(foods$new[foods$new %in% udpipe_text$lemma][1],
                                                             "?\n",sample(food_replies,1)))
    
  
  if (sum(c("плавать","река","купаться","волга","волгу","купаваться") %in% udpipe_text$lemma)>0) return(sample(swim_replies,1))
  if ("бот" %in% udpipe_text$lemma) return(sample(bot_replies,1))
  
  if(grepl("%>%", text,fixed=T)) return(sample(dplyr_replies,1))  
  if(grepl("filter", text,fixed=T)) return(sample(dplyr_replies,1))  
  if(grepl("dplyr", text,fixed=T)) return(sample(dplyr_replies,1))  
  
  if(grepl("[,.(", text,fixed=T)) return(sample(dt_replies,1))  
  if(grepl("data.table", text,fixed=T)) return(sample(dt_replies,1))  
  if(grepl(":=", text,fixed=T)) return(sample(dt_replies,1))  
  if(grepl(".SD", text,fixed=T)) return(sample(dt_replies,1))    
  if(grepl("ggplot", text,fixed=T)) return(sample(ggplot_replies,1))    
  if(grepl("qplot", text,fixed=T)) return(sample(ggplot_replies,1))    
  if(grepl("<-", text,fixed=T)) return(sample(r_replies,1))    
  
  
  if (sample(2,1)==1){
    if ("спасибо" %in% udpipe_text$lemma) return(sample(spasibo_replies,1))
    }
  if (sample(4,1)==1){
    if(grepl("АН", text)) return(paste0(gsub("АН","ан",text),"\nан!\nдан!"))  
    if(grepl("Ан|ан", text)) return(paste0(gsub("Ан|ан","АН",text),"\nАН!\nДАН!"))   
  }
}

send_an_dan<-function(bot, update){
  reply<-an_dan(update$message$text)
  if(!is.null(reply))  bot$sendMessage(chat_id = update$message$chat_id, text = reply)
}



picture_replies<-c("О БОЖЕ, НА ЭТОМ ФОТО КАЛЬМАР! ОГРОМНЫЙ СТРАШНЫЙ КАЛЬМАР!","Ну и рожи","Выглядит великолепно!","Картинка супер!", "Ты на свете всех милее, всех румяней и белее!", "Вы уверены, что такое можно выкладывать в общий доступ?", "Порнуха какая-то...")

send_an_dan_photo<-function(bot, update){

  if(sample(2,1)==1){
   reply<-sample(picture_replies,1)
     bot$sendMessage(chat_id = update$message$chat_id, text = reply)
  }
}




andan_handler <- MessageHandler(send_an_dan, Filters$text)
andan_handler2 <- MessageHandler(send_an_dan_photo, Filters$photo)

dispatcher$add_handler(andan_handler)
dispatcher$add_handler(andan_handler2)
updater$start_polling()