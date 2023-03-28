# psychAssist script ---- 
# current_path = rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path))
setwd("D:/PRIN2020/qualtrics/psycAssist/Adulti")
library(ggplot2)
library(dplyr)
library(lavaan)
library(mokken)
# restituisce la dipendenza locale ----
dip.local = function(model, cut = .20) {
  temp_local = model$Q3.matr
  index = which( upper.tri(temp_local,diag=F) , arr.ind = TRUE )
  local = data.frame( col = dimnames(temp_local)[[2]][index[,2]] ,
                      row = dimnames(temp_local)[[1]][index[,1]] ,
                      val = temp_local[ index ] )
  summary.local = local[abs(local$val) > cut, ]
  return(summary.local)
}
# ridà i parametri degli item ordinati per proporzione di risposte corrette ----
item.par = function(model) {
  est = model$item 
  est = est[order(est$M), ]
  return(est)
}
# item fit basata sul chi quadro ----
item.fit = function(model) {
  temp.fit = tam.modelfit(model)
  chi = temp.fit$chisquare.itemfit
  chi = chi[chi$p.holm < .05, ]
  return(chi)
}

guide = data.frame(id_question = 1:52, 
                   tipo = rep(c("due.celle", "tre.celle"), c(3, 49)))

# csv.files = list.files(pattern = ".csv")
# 
# dates = gsub("matdata_adults_", "", csv.files)
# dates = gsub(".csv", "", dates)
# dates = gsub("_", "-", dates)
# dates = as.Date(dates)
# latest = max(dates)
# latest = gsub("-", "_", latest)
# 

data = read.csv("mat50_2023_03_17.csv", 
                header = T, sep = ",")

head(data)
table(data$external_code)
data = data[!data$external_code %in% c("AB", "Pietro", 
                                       "UTENTE01", "PROVA", "Prova"), ]
data = data[!data$date_end %in% "NULL", ]
# ATTENZIONE: FARE CORRERE SOLO UNA VOLTA ------
data$id_question = data$id_question + 1

data$response_vector = gsub('[\"]', "", data$response_vector)
data$distractors = gsub(".*_", "", data$response_vector)
data$distractors = gsub(".svg]", "", data$distractors)
# macro_distractors contiene solo le macrocategorie di distrattori 
data$macro_distractors = numeric(nrow(data))

for (i in 1:nrow(data)) {
  if(data[i, "distractors"] == "d.union" | data[i, "distractors"] == "diff1" | data[i, "distractors"] == "diff2" | data[i, "distractors"] == "diff") {
    data[i, "macro_distractors"] = "D"
  } else if (grepl("ic", data[i, "distractors"])  ==T) {
    data[i, "macro_distractors"] = "IC"
  } else if (grepl("wp", data[i, "distractors"])  ==T) {
    data[i, "macro_distractors"] = "WP"
  } else if (grepl("correct", data[i, "distractors"])  ==T){
    data[i, "macro_distractors"] = "correct"
  } else if (data[i, "distractors"] == "r.left" | data[i, "distractors"] == "r.top" | data[i, "distractors"] == "r.diag"){
    data[i, "macro_distractors"] = "R"
  }
}


# RICODIFICA SCOLARITà -----
# ricodifico in: 
# fino a 8 anni inclusi: primaria 
# 9-13 anni (incluso): secondaria
# maggior di 13 superiore 
data$anni_scolarita = as.integer(data$anni_scolarita)
data$scol = ifelse(data$anni_scolarita <=8, "primaria", 
                   ifelse(data$anni_scolarita > 8 & data$anni_scolarita <= 13, 
                          "secondaria", 
                          "superiore"))

# fa il check per vedere che  non ci siano proev
#table(data$external_code)
#table(data$id)


# primo check per vedere la percentuale di risposte corrette 

temp = data[, c("id", "id_question", "task_success")]


temp$task_success = ifelse(temp$task_success == "True", 
                           1, 0)
wide.temp = reshape(temp, 
                    idvar = "id", 
                    timevar = "id_question", 
                    direction = "wide")

wide.temp$total_score = rowSums(wide.temp[,-1], na.rm = T)



## Distribuzione grezza punteggi accuratezza -----


wide.temp = wide.temp[order(wide.temp$total_score), ]
wide.temp$new.id = 1:nrow(wide.temp)

ggplot(wide.temp, 
       aes(x=as.factor(new.id), y = total_score)) + geom_point() +
  geom_hline(yintercept = mean(wide.temp$total_score), 
             col = "red") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[3], 
             col = "blue") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[2], 
             col = "green") + 
  geom_hline(yintercept = length(unique(data$id_question))*0.20, 
             col = "purple") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  annotate("text", color = "red", 
           x = length(wide.temp$id)-5, 
           y = mean(wide.temp$total_score), label = "Mean") + 
  annotate("text", color = "blue", 
           x = length(wide.temp$id)-5, 
           y = median(wide.temp$total_score), label = "Median") + 
  annotate("text", color = "green", 
           x = length(wide.temp$id)-5, 
           y = quantile(wide.temp$total_score)[2], label = "Q1") + ylab("Score accuratezza") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank())

wide.temp = wide.temp[order(wide.temp$total_score), ]
wide.temp$new.id = 1:nrow(wide.temp)
# dettagli soggetti -----
small_sbj = data[, c("id", "anni_scolarita", 
                     "gender", "diagnostic", "scol")]
small_sbj = small_sbj %>% distinct()
# recode disagi persone ------ 
small_sbj$diagnostic = tolower(small_sbj$diagnostic)
small_sbj$new.diag = ifelse(small_sbj$diagnostic == "", 
                            "ok", 
                            ifelse(grepl("disort", small_sbj$diagnostic), 
                                   "DSA", 
                                   ifelse(grepl("disless", small_sbj$diagnostic), 
                                          "DSA", "altro")))

wide.temp = merge(wide.temp, small_sbj)


## Distrbuzione grezza punteggi accuratezza (considerando diagnosi)


ggplot(wide.temp, 
       aes(x=as.factor(new.id), 
           y = total_score,  
           color = as.factor(scol))) + geom_point(size =3, alpha = .8) +
  geom_hline(yintercept = mean(wide.temp$total_score), 
             col = "red") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[3], 
             col = "blue") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[2], 
             col = "green") + 
  geom_hline(yintercept = length(unique(data$id_question))*0.20, 
             col = "purple") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  annotate("text", color = "red", 
           x = length(wide.temp$id)-5, 
           y = mean(wide.temp$total_score), label = "Mean") + 
  annotate("text", color = "blue", 
           x = length(wide.temp$id)-5, 
           y = median(wide.temp$total_score), label = "Median") + 
  annotate("text", color = "green", 
           x = length(wide.temp$id)-5, 
           y = quantile(wide.temp$total_score)[2], label = "Q1") + ylab("Score accuratezza") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        legend.position = "bottom") 


# diagnosi -----
ggplot(wide.temp, 
       aes(x=as.factor(new.id), 
           y = total_score,  
           color = scol, shape = new.diag)) + geom_point(size = 2) +
  geom_hline(yintercept = mean(wide.temp$total_score), 
             col = "red") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[3], 
             col = "blue") + 
  geom_hline(yintercept = quantile(wide.temp$total_score)[2], 
             col = "green") + 
  geom_hline(yintercept = length(unique(data$id_question))*0.20, 
             col = "purple") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  annotate("text", color = "red", 
           x = length(wide.temp$id)-5, 
           y = mean(wide.temp$total_score), label = "Mean") + 
  annotate("text", color = "blue", 
           x = length(wide.temp$id)-5, 
           y = median(wide.temp$total_score), label = "Median") + 
  annotate("text", color = "green", 
           x = length(wide.temp$id)-5, 
           y = quantile(wide.temp$total_score)[2], label = "Q1") + ylab("Score accuratezza") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        legend.position = "bottom") # + 
#scale_color_manual(values = c(RColorBrewer::brewer.pal(11, "Dark2"))) 
# calcolo delle latenze ---- 

data$time.diff.item = difftime(data$end_time, 
                               data$start_time, 
                               units = "secs")
data$time.diff.item = as.numeric(data$time.diff.item)
sum.time =aggregate(time.diff.item ~ id, 
                    data, sum)
colnames(sum.time)[2] = "sum.time"
sum.time$sum.time = as.numeric(sum.time$sum.time)

# # unisco il dettaglio sui tempi al dataset ----
data = merge(data, sum.time, by="id")
# unisco anche i punteggi totali e il filter 

wide.small = wide.temp[, c("id", "total_score")]

data = merge(data, wide.small, by = "id")


# genere ----- 
table(wide.temp$scol, wide.temp$gender)
ggplot(data, 
       aes(x = scol, y = total_score, 
           color = gender)) + geom_boxplot()

# faccio lo stesso grafico ma con la media e la sd 

library(dplyr)
# GRAFICO GENERE-PUNTEGGIO-SCOLARITà -----
score_sbj_gender = data %>% 
  group_by(scol, gender) %>% 
  summarise(mean = mean(total_score), sd = sd(total_score)) 

ggplot(score_sbj_gender, 
       aes(x = scol, y = mean, color = gender, group = gender)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(x = scol, 
                    ymin = mean-sd, 
                    ymax = mean+sd),width=0.2, size=1) +
  geom_line(aes(y = mean))

# voglio vedere uomini e donne ordinati per punteggio, 
# sulla y mi metto il loro tempo 


ggplot(data, 
       aes(x= reorder(id, total_score), 
           y = sum.time, color = gender)) + 
  geom_point()

# a questo punto mi creo anche i risponditori più bravi ---- 


cut.sbj = quantile(wide.temp$total_score)

data$ab.sbj = ifelse(data$total_score <= cut.sbj[2], 
                     "pessimi", 
                     ifelse(data$total_score > cut.sbj[2] & data$total_score <= cut.sbj[3], 
                            "scarsi", 
                            ifelse(data$total_score > cut.sbj[3] & data$total_score <= cut.sbj[4], 
                                   "bravi", "top")))

wide.temp$ab.sbj = ifelse(wide.temp$total_score <= cut.sbj[2], 
                          "pessimi", 
                          ifelse(wide.temp$total_score > cut.sbj[2] & wide.temp$total_score <= cut.sbj[3], 
                                 "scarsi", 
                                 ifelse(wide.temp$total_score > cut.sbj[3] & wide.temp$total_score <= cut.sbj[4], 
                                        "bravi", "top")))

table(wide.temp$ab.sbj)
table(wide.temp$ab.sbj, wide.temp$gender)

data$ab.sbj = factor(data$ab.sbj, 
                     levels = c("pessimi", "scarsi", "bravi", "top"))


ggplot(data, 
       aes(x = ab.sbj, y = sum.time, color = gender)) + 
  geom_boxplot()

# GRAFICO GENERE-ABILITà (tempo) -----

sbj_time_gender = data %>% 
  group_by(ab.sbj, gender) %>% 
  summarise(mean = mean(sum.time), sd = sd(sum.time))
sbj_time_gender$ab.sbj = factor(sbj_time_gender$ab.sbj, 
                                levels = c("pessimi", "scarsi", 
                                           "bravi", "top"))

ggplot(sbj_time_gender, 
       aes(x = ab.sbj, y = mean, color = gender, group = gender)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(x = ab.sbj, 
                    ymin = mean-sd, 
                    ymax = mean+sd),width=0.2, size=1) +
  geom_line(aes(y = mean))


# GRAFICO ABILITà - SCOLARITà ------

ab_scol = data %>% 
  group_by(ab.sbj, scol) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
ab_scol$scol = factor(ab_scol$scol, 
                      levels = c("superiore", "secondaria", "primaria"))
ab_scol$ab.sbj = factor(ab_scol$ab.sbj, 
                        levels = c("pessimi", "scarsi" ,
                                  "bravi", "top"))

ggplot(ab_scol, 
       aes(x= ab.sbj, 
           fill = scol, 
           y = prop)) + geom_bar(stat = "identity")

# provo a considerare genere, abilitòà e scolarità -----

sbj_time_gender_scol = data %>% 
  group_by(ab.sbj, gender, scol) %>% 
  summarise(mean = mean(sum.time), sd = sd(sum.time))
sbj_time_gender$ab.sbj = factor(sbj_time_gender$ab.sbj, 
                                levels = c("pessimi", "scarsi", 
                                           "bravi", "top"))

ggplot(sbj_time_gender_scol, 
       aes(x = ab.sbj, y = mean, color = gender, group = gender)) + 
  geom_point(size = 3)  +
  geom_line(aes(y = mean)) + facet_wrap(~scol)

data$city = tolower(data$city)
data$anni_scolarita = factor(data$anni_scolarita)



# item probability ----
data$correct = ifelse(data$task_success == "True", 1, 0)

item_prob = aggregate(correct ~ id_question, data = data, mean)

# GRAFICO ITEM PROBABILITY -----
ggplot(item_prob[order(item_prob$correct), ], 
       aes( x = reorder(id_question, correct), y = correct)) + geom_point()

# ora considero tutto attraverso le scolarità 
item_time = aggregate(time.diff.item ~ id_question, data = data, mean)

item_char = merge(item_prob, item_time)

# GRAFICO ITEM PROBABILITY CON TEMPO DI RISPOSTA ----
ggplot(item_char, aes(x=reorder(id_question, correct), 
                      y = time.diff.item)) + geom_point()


table(data$anni_scolarita)
# tolgo i NULL 

data = data[!data$anni_scolarita %in% "NULL", ]
data$anni_scolarita = as.character(data$anni_scolarita)
data$anni_scolarita = as.integer(data$anni_scolarita)

table(data$anni_scolarita)

# ora faccio quello che ho fatto prima però considero questa nuova suddivisione della 
# scolarità 

item_prob_scol = aggregate(correct ~ id_question + scol, data = data, mean)

# GRAFICO IEM PROBABILITY PER SCOLAIRTà ----

ggplot(item_prob_scol, 
       aes( x = reorder(id_question, correct), 
            y = correct, color = scol, 
            shape = scol, group = scol, 
            linetype = scol)) + geom_point(size = 2) +
  geom_line(aes(y=correct))

# ora considero tutto attraverso le scolarità 
item_time_scol = aggregate(time.diff.item ~ id_question + scol, data = data, mean)

# GRAFICO TEMPO DI RISPOSTA E SCOLAIRTà -----
ggplot(item_time_scol, 
       aes( x = reorder(id_question, time.diff.item), 
            y = time.diff.item, color = scol, shape = scol, group = scol)) + geom_point() +
  geom_line(aes(y=time.diff.item))



item_char_scol = merge(item_prob_scol, 
                       item_time_scol)

# GRAFICO ITEM PROBABILITY TEMPO DI RISPOSTA SCOLARITà -----
ggplot(item_char_scol, 
       aes(x=reorder(id_question, correct), 
                      y = time.diff.item, 
                      color = scol, shape = scol, group = scol)) + geom_point() +
  geom_line(aes(y=time.diff.item))


ggplot(item_char_scol, aes(x=scol, 
                           y = time.diff.item, 
                           color = scol, shape = scol, group = scol)) + geom_boxplot()


# analisi sui quartili ------

prop_item = aggregate(correct ~ id_question, 
                      data, mean)


cutoff = quantile(prop_item$correct)
prop_item$item.diff = ifelse(item_prob$correct <= cutoff[2], 
                             "molto.difficile", 
                             ifelse(item_prob$correct > cutoff[2] &  item_prob$correct <= cutoff[3],
                             "difficile", 
                             ifelse(item_prob$correct > cutoff[3] &  item_prob$correct <= cutoff[4], 
                                    "facili", "molto.facile")))


table(prop_item$item.diff)
# quindi a questo punto dovrei unire questo dettaglio sugli item 
# al dataset genereale e rifare le analisi come prima ma considerando 
# la divisione quartillica e non il singolo item 
# provo ad unirlo al datset dove ho già la scolarità e i 
# tempi medi sperando che per qualche miracolo mi calcoli 
# da solo la media odio la mia vita 

colnames(prop_item)[2] = "prop.tot"

item_char_scol = merge(item_char_scol, 
                       prop_item)

item_char_scol$item.diff = factor(item_char_scol$item.diff, 
                                  levels = c("molto.facile", 
                                             "facili", 
                                             "difficile", "molto.difficile"))
# GRAFICO CON LIVELLI DI DIFFICOLTà e TR (distribuzione) ------

ggplot(item_char_scol, 
       aes(x = item.diff, 
           y = time.diff.item, color = scol)) + 
  geom_boxplot()





# devo fare le medie dei tempi di risposta nelle scolarità per 
# i diversi livelli di difficoltà degli item 

# GRAFICO CON LIVELLI DI DIFFICOLTà e TR (MEDIA SD) ------
time_details = item_char_scol %>%  
  group_by(scol, item.diff) %>% 
  summarise(mean_time = mean(time.diff.item), sd = sd(time.diff.item))

ggplot(time_details, 
       aes(x = item.diff, y = mean_time, 
           color = scol, group = scol, 
           linetype = scol)) + geom_point(size = 2) +
  geom_line(aes(y=mean_time), size = 1) + 
  geom_errorbar(aes(x = item.diff, 
                    ymin = mean_time - sd, 
                    ymax = mean_time + sd), 
                width = .2, size = 1)

prova = item_char_scol[!item_char_scol$scol %in% "secondaria", ]
prova$difference = 0 

for (i in 2:nrow(prova)) {
  prova[i-1, "difference"] = prova[i, "time.diff.item"] - prova[i-1, "time.diff.item"]
}

# prendo solo le righe della primaria perché per ora mi servono solo quelle 

prova.1 = prova[prova$scol %in% "primaria", ]

ggplot(prova.1, 
       aes(x = reorder(id_question, correct), 
           y = difference, color = item.diff)) + geom_point()


# lo faccio anche per i livelli di difficoltà medi 

prova.details = prova.1 %>% 
  group_by(item.diff) %>% 
  summarise(mean_difference = mean(difference), sd = sd(difference))

prova.details$item.diff = factor(prova.details$item.diff, 
                                 levels = c("molto.difficile", 
                                            "difficile", 
                                            "facili", 
                                            "molto.facile"))

ggplot(prova.details, 
       aes(x = item.diff, y = mean_difference)) + 
  geom_point() + geom_errorbar(aes(x = item.diff,
                                   ymin=mean_difference-sd, 
                                   ymax = mean_difference + sd), 
                               width= .2)


# differenza di genere nella differenza di tempo sugli item nei quartili ---- 

gender_difference = data %>% 
  group_by(scol, item.diff, gender) %>% 
  summarise(mean_time = mean(time.diff.item), 
            sd = sd(time.diff.item))
# tiro fuori la secondaria

gender_difference = gender_difference[!gender_difference$scol %in% "secondaria", ]

for (i in 9:nrow(gender_difference)) {
  gender_difference[i, "difference"] = gender_difference[i, "mean_time"] - 
    gender_difference[i-8, "mean_time"]
}

# ora seleziono solo le righe con superiore 

gender_difference = gender_difference[gender_difference$scol %in% "superiore", ]

ggplot(gender_difference, 
       aes(x = item.diff, y = difference, 
           color = gender)) + geom_point() 


# voglio fare risposte giuste vs errate -----
# GRAFICI TEMPI DI RISPOSTA ERRATE VS. CORRETTE PER ITEM DIFFICULTY E TIME -----

data = merge(data, prop_item)

data$correct = factor(data$correct, 
                      levels = c("1", "0"))

data$item.diff = factor(data$item.diff, 
                        levels = c("molto.facile", 
                                   "facili", 
                                   "difficile", 
                                   "molto.difficile"))
ggplot(data, 
       aes(x = item.diff, y = time.diff.item, 
           color =correct)) + geom_boxplot() + 
  facet_wrap(~scol)

item_ce_time = aggregate(time.diff.item ~ scol + correct + item.diff, 
          data, mean)

item_ce_time$correct = ifelse(item_ce_time$correct == 1, 
                              "correct", "error")
item_ce_time$correct = factor(item_ce_time$correct, 
                              levels = c("error", "correct"))
item_ce_time$item.diff = factor(item_ce_time$item.diff, 
                                levels = c("molto.facile", 
                                           "facili", 
                                           "difficile", 
                                           "molto.difficile"))

ggplot(item_ce_time, 
       aes(x = item.diff, y = time.diff.item, 
           color = correct, group = correct)) + 
  geom_point(size = 2) +
  geom_line(aes(y=time.diff.item)) + facet_wrap(~scol)



# provo a fare qualcosa sui distrattori ----
# prima cosa, cerco di vedere i distrattori più scelti in generale
# disrtrattori più scelti in generale ----- 
data = merge(data, guide)


distr_prob = data.frame(table(data$id_question, 
                 data$macro_distractors)/length(unique(data$id)))

colnames(distr_prob) = c("id_question", "response", "prop")
distr_prob$response = factor(distr_prob$response, 
                             levels = c( "0","D", "WP", "R", "IC", "correct"))
item_prob = item_prob[order(item_prob$correct), ]
item_prob$new_id = 1:nrow(item_prob)

distr_prob = merge(distr_prob, item_prob)


# GRAFICO proporzione distrattori -----
ggplot(distr_prob, 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light()


ggplot(distr_prob, 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light()

# il problema del graico visto così è che non è molto utile per la diagnostica 
# degli item (e infatti la prima volta non me ne ero accorta)

# quindi adesso faccio la stessa cosa ma considero anche la scolarità 
# (nelle classificazioni che ho creato prima)

temp = data[, c("id", "scol")]

scol = temp %>% distinct()


freq_scol = data.frame(table(scol$scol))

colnames(freq_scol) = c("scol", "tot.scol")


distr_scol_prob = data.frame(table(data$id_question, 
                              data$macro_distractors, 
                              data$scol))

colnames(distr_scol_prob) = c("id_question", "response", "scol", 
                              "tot.freq")

distr_scol_prob$response = factor(distr_scol_prob$response, 
                             levels = c( "0","D", "WP", "R", "IC", "correct"))

distr_scol_prob = merge(distr_scol_prob, freq_scol)
distr_scol_prob$prop = distr_scol_prob$tot.freq/distr_scol_prob$tot.scol

distr_scol_prob = merge(distr_scol_prob, item_prob)

# in realtà questi grafici non aiutano come aiutava guardare le proporzioni 
# per i singoli anni di scolarità
ggplot(distr_scol_prob[distr_scol_prob$scol %in% "primaria", ], 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light() + 
  facet_wrap(~scol)

ggplot(distr_scol_prob[distr_scol_prob$scol %in% "secondaria", ], 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light() + 
  facet_wrap(~scol)


ggplot(distr_scol_prob[distr_scol_prob$scol %in% "superiore", ], 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light() + 
  facet_wrap(~scol)


# distrattori e anni di scolarità dettgalio ------

temp = data[, c("id", "anni_scolarita")]

scol_det = temp %>% distinct()


freq_scol_det = data.frame(table(scol_det$anni_scolarita))

colnames(freq_scol_det) = c("scol_det", "tot.scol_det")


distr_scol_det_prob = data.frame(table(data$id_question, 
                                       data$macro_distractors, 
                                       data$anni_scolarita))

colnames(distr_scol_det_prob) = c("id_question", "response", 
                                  "scol_det", 
                                  "tot.freq")

distr_scol_det_prob$response = factor(distr_scol_det_prob$response, 
                                      levels = c( "0","D", "WP", "R", "IC", "correct"))

distr_scol_det_prob = merge(distr_scol_det_prob, freq_scol_det)
distr_scol_det_prob$prop = distr_scol_det_prob$tot.freq/distr_scol_det_prob$tot.scol_det

distr_scol_det_prob = merge(distr_scol_det_prob, item_prob)

# così non ha senso 
ggplot(distr_scol_det_prob, 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light() + 
  facet_wrap(~scol_det)

# io enandrea abbiamo guardato questo 
ggplot(distr_scol_det_prob[distr_scol_det_prob$scol_det %in% 6, ], 
       aes(x = as.factor(new_id), y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(labels = item_prob$id_question) + theme_light() + 
  facet_wrap(~scol_det)


# mi piacerebbe fare un discorso sui tempi di risposta dei distrattori 
# qui ho le probabilità di ogni distrattore 
distr_prob
# intanto potrei ottenere la distribuzione dei tmepi di risposta per ogni distrattore 
data$new_var = paste(data$tipo, data$macro_distractors, sep ="." )
data$new_var = factor(data$new_var, 
                      levels = c("due.celle.correct", 
                                 "tre.celle.correct", 
                                 "due.celle.IC", 
                                 "tre.celle.IC", 
                                 "due.celle.R", 
                                 "tre.celle.R", 
                                 "due.celle.WP", 
                                 "tre.celle.WP", 
                                 "due.celle.D", 
                                 "tre.celle.D"))
ggplot(data[!data$macro_distractors %in% 0, ], 
       aes(x=new_var, 
           y = time.diff.item)) + 
  geom_violin(trim = F) + stat_summary(fun.data=mean_sdl, 
                                       geom="pointrange", size=1, 
                                       color="red")
# comunque questa cosa non è chiarissima 
# forse quello che voglio vedere è la probabilità di ogni distrattore 
# plottata contro il tempo medio?
# ????? 

# MODEL TIME-----
# creao un dataframe solo con gli item 
item = wide.temp[, c(grep("id", colnames(wide.temp)), 
                     grep("task", colnames(wide.temp)))]
# tolgo newid 
item = item[, -2]
colnames(item) = gsub("task_success", "item", colnames(item))

iout = c("item.5", 
         "item.31", 
         "item.12", 
         "item.18", 
         "item.37")
for (i in 1:length(iout)) {
  item = item[, names(item)[!colnames(item) == iout[i]]]
}

item = item[, names(item)[!colnames(item) == iout[1]]]

# monotonicyt ----- 


mono = check.monotonicity(item[,-1])
plot(mono)
mono = as.data.frame(summary(mono))
mono[mono$ItemH <= .30, ]

# c'è qualcosa di strano sulla monotonicità 
# per ora non la considero perché è un risultato davvero strano 
# cfa -----

form.mod = paste(colnames(item)[-1], collapse = " + ")
form.mod = paste("latent =~ ", form.mod)


cfa.1 = cfa(form.mod, data = item[, -1], ordered = colnames(item)[-1])

summary(cfa.1, fit.measures = T)
# overfit 
# le cose non stanno andando benissimo 
# provo comunque con Rasch 
# 1pl ------
library(TAM)
m.1pl = tam.mml(item[,-1], verbose = F, pid = item$id)
m.1pl.fit = tam.modelfit(m.1pl)
# le stime non hanno senso 

item.par(m.1pl)
dip.local(m.1pl.fit)
# è ancora troppo instabile 



# DA QUI IN POI è ROTTO  ---- 
# prima cosa: check dfferene di genere nel punteggio totale 

# effetto sperimentatore----

ggplot(data, 
       aes(x = as.factor(clinic_id), y = sum.time, 
           color = anni_scolarita)) + geom_boxplot(size = 1.2) + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("Codice sperimentatore") +
  ylab("Tempo di esecuzione del compito in secondi") + 
  ggtitle("Distribuzione tempi di risposta per sperimentatore") 


ggplot(data, 
       aes(x = as.factor(clinic_id), y = total_score, 
           color = anni_scolarita)) + geom_boxplot(size =1.2)  + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("Codice sperimentatore") +
  ylab("Score totale") + 
  ggtitle("Distribuzione score accuratezze totali per sperimentatore")


ggplot(data, 
       aes(x = as.factor(city), y = total_score, 
           color = anni_scolarita)) + geom_boxplot(size = 1.1) + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("città") +
  ylab("Score accuratezze") + 
  ggtitle("Distribuzione score accuratezze per città") + 
  theme(axis.text.x = element_text(angle = 90))


ggplot(data, 
       aes(x = as.factor(province), y = total_score, 
           color = anni_scolarita)) + geom_boxplot(size = 1.1) + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("città") +
  ylab("Score accuratezze") + 
  ggtitle("Distribuzione score accuratezze per città") + 
  theme(axis.text.x = element_text(angle = 90))


# Analisi tempi di risposta ------ 
d = data
# per il momento tolgo i null perché mi fanno ansia ----- 

d = d[!d$anni_scolarita %in% "NULL", ]
d$anni_scolarita = as.character(d$anni_scolarita)

# PREPARAZIONE DISTRATTORI -----
d$response_vector = gsub('[\"]', "", d$response_vector)
d$distractors = gsub(".*_", "", d$response_vector)
# d distractors contiene i dettagli su ogni distrattore -----
d$distractors = gsub(".svg]", "", d$distractors)
# macro_distractors contiene solo le macrocategorie di distrattori 
d$macro_distractors = numeric(nrow(d))

for (i in 1:nrow(d)) {
  if(d[i, "distractors"] == "d.union" | d[i, "distractors"] == "diff1" | d[i, "distractors"] == "diff2" | d[i, "distractors"] == "diff") {
    d[i, "macro_distractors"] = "D"
  } else if (grepl("ic", d[i, "distractors"])  ==T) {
    d[i, "macro_distractors"] = "IC"
  } else if (grepl("wp", d[i, "distractors"])  ==T) {
    d[i, "macro_distractors"] = "WP"
  } else if (grepl("correct", d[i, "distractors"])  ==T){
    d[i, "macro_distractors"] = "correct"
  } else if (d[i, "distractors"] == "r.left" | d[i, "distractors"] == "r.top" | d[i, "distractors"] == "r.diag"){
    d[i, "macro_distractors"] = "R"
  }
}
# controlla che tutti siano stati ricodificati correttamente 
table(d$macro_distractors)
guide$id_question = 1:52


# UNISCE I DETTAGLI DEGLI ITEM AL DATASET (CREA D1) ----
d1  =merge(d, guide, by = "id_question")
# ricodifica le risposte in corrette ed errate 
d1$task_success = ifelse(d1$task_success == "True",
                         "correct", "wrong")

# calcola il numero di osservazioni per ogni soggetto -----
freq_p = data.frame(table(d1$id))
colnames(freq_p) = c("id", "freq_p")
# lo unisce a d1 --- 
d1 = merge(d1, freq_p, by = "id")


# mi prendo la scolarità e calcolo il numero di bambini per ongi anno di scolarità 
scol = d1[, c("id", "anni_scolarita")]
scol = scol %>% 
  distinct()

freq_scol_p = data.frame(table(scol$anni_scolarita))
colnames(freq_scol_p) = c("anni_scolarita", "num_scol")
# aggiungo il numero di osservazioni per ogni anno di scolarità al dataset 
# generale 

d1 = merge(d1, freq_scol_p, by = "anni_scolarita")



# calcola il numero di osservazioni su ogni item ATTRAVERSO LA SCOLARITà------
freq_i = data.frame(table(d1$id_question))
colnames(freq_i) = c("id_question", "freq_i")
# unissce il totale di obs sugli item al dataset -----
d1 = merge(d1, freq_i, by = "id_question")

# crea il numero di risposte corrette per ogni soggetto considerando anche scolarità
correct_p = data.frame(table(d1$id, d1$task_success))
colnames(correct_p) = c("id", "response",  "freq")
correct_p = merge(correct_p, freq_p, by = "id")
# calcola la proporzione di risp corrette dividendo il numero di risp 
# corrette per il numeor di osservazioni sul soggetto 
correct_p$prop = correct_p$freq/correct_p$freq_p
# prendo solo le risposte corrette ---
only_correct_p = correct_p[correct_p$response %in% "correct", ]
# riordino le proporsioni 
only_correct_p = only_correct_p[order(only_correct_p$prop), ]
only_correct_p$p_cres <- 1:nrow(only_correct_p)
only_correct_p$p_cres <- as.factor(only_correct_p$p_cres)

# unisco scolarità al dataset ----

only_correct_p = merge(only_correct_p, scol, by = "id")


# calcolo le proporzioni di risposte corrette per gli item ---- 
correct_i = data.frame(table(d1$id_question, d1$task_success))
colnames(correct_i) = c("id_question", "response", "Freq")
# unisco il totale degli item ----

correct_i = merge(correct_i, freq_i, by = "id_question")
correct_i$prop = correct_i$Freq/correct_i$freq_i

correct_i = correct_i[correct_i$response %in% "correct", ]

cutoff = quantile(correct_i$prop)

# aggiungo una divisione di item in base alla loro distribuzione quartilica
# < q1 difficili
# q1 q2 incluso medio diff
# q2 q3 incluso gnec
# maggiori q3 molto facili

for (i in 1:nrow(correct_i)) {
  if (correct_i[i, "prop"] <= cutoff[2]) {
    correct_i[i, "item.diff"] = "diff"
  } else if (correct_i[i, "prop"] > cutoff[2] & correct_i[i, "prop"] <= cutoff[3]) {
    correct_i[i, "item.diff"] = "medio.diff"
  } else if ( correct_i[i, "prop"] > cutoff[3] & correct_i[i, "prop"] <= cutoff[4]) {
    correct_i[i, "item.diff"] = "facili"
  } else if (correct_i[i, "prop"] > cutoff[4]) {
    correct_i[i, "item.diff"] = "molto.facili"
  }
}

colnames(correct_i) = c("id_question", "response", "freq.correct", "freq_i", "prop_c_item", 
                        "item.diff")
mapping_item = correct_i[, c("id_question", "item.diff")]
# unicsco al dataset grande il mapping degli item (difficili, facili ecc)


d1 = merge(d1, correct_i, by = "id_question")


item.quartili.mean = aggregate(time.diff.item ~ anni_scolarita + item.diff, 
                               d1, 
                               mean)
colnames(item.quartili.mean)[ncol(item.quartili.mean)] = "mean.time.item"

item.quartili.sd = aggregate(time.diff.item ~ anni_scolarita + item.diff, 
                             d1, 
                             sd)
colnames(item.quartili.sd)[ncol(item.quartili.sd)] = "sd.time.item"



item.quartili = merge(item.quartili.mean, 
                      item.quartili.sd, by = c("anni_scolarita", "item.diff"))

item.quartili$group = 1
table(correct_i$item.diff)

item.quartili.mean.c = aggregate(time.diff.item ~ anni_scolarita + item.diff + task_success, 
                                 d1, 
                                 mean)
colnames(item.quartili.mean.c)[ncol(item.quartili.mean.c)] = "mean.time.item"

item.quartili.sd.c = aggregate(time.diff.item ~ anni_scolarita + item.diff + task_success, 
                               d1, 
                               sd)
colnames(item.quartili.sd.c)[ncol(item.quartili.sd.c)] = "sd.time.item"



item.quartili.c = merge(item.quartili.mean.c, 
                        item.quartili.sd.c, by = c("anni_scolarita", "item.diff", "task_success"))
item.quartili.c$task_success = factor(item.quartili.c$task_success, 
                                      levels = c("wrong", "correct"))

item.quartili.c$item.diff = factor(item.quartili.c$item.diff, 
                                   levels = c("molto.facili", 
                                              "facili", 
                                              "medio.diff", 
                                              "diff"))
# scolarità, difficoltà degli item e tempi di risposta 
ggplot(item.quartili.c, 
       aes(x = item.diff, y = mean.time.item, 
           group=task_success, color = task_success)) + 
  geom_point(size = 3)  + geom_line(aes(y = mean.time.item), 
                                    size = 1) +
  facet_wrap(~anni_scolarita) + theme_light() + 
  ggtitle("Tempi di risposta per difficoltà dell'item e scolarità")

# scolarità, difficoltà degli item e tempi di risposta (singole linee) -----
d1$correct = ifelse(d1$task_success == "correct", 1, 0)

prop_item = aggregate(correct ~ id_question + anni_scolarita, d1, mean)
time_item = aggregate(time.diff.item ~ id_question + anni_scolarita, d1, mean)

data_item = merge(prop_item, time_item, 
                  by = c("id_question", "anni_scolarita"))


prop_tota = aggregate(correct  ~ id_question , data_item, mean)
colnames(prop_tota)[ncol(prop_tota)] = "prop"

prop_tota = prop_tota[order(prop_tota$prop), ]
prop_tota$new_id = 1:nrow(prop_tota)
prop_tota$labl = prop_tota$id_question


data_item = merge(data_item, 
                  prop_tota)

ggplot(data_item[!data_item$anni_scolarita %in% 5, ], 
       aes(x = correct, y = time.diff.item, 
           color = as.factor(id_question), 
           group = anni_scolarita)) + geom_point()+ theme_light() +
  geom_smooth(se=F) + facet_wrap(~ anni_scolarita)

ggplot(data_item, 
       aes(x = as.factor(new_id), y = time.diff.item, 
           color = anni_scolarita, 
           group = anni_scolarita)) + geom_point(aes(size = correct)) + geom_line(aes(y=time.diff.item)) + 
  scale_x_discrete(labels = prop_tota$labl) + theme_light() 


# data_item$anni_scolarita = factor(data_item$anni_scolarita, 
#                                  levels =  as.character(data_item$anni_scolarita)[order(as.character(data_item$anni_scolarita), decreasing = T)])
# difficioltà latenteze scolarità ------
ggplot(data_item, 
       aes(x = as.factor(new_id), y = time.diff.item, 
           color = anni_scolarita, 
           group = anni_scolarita)) + geom_point() + geom_line(aes(y=time.diff.item)) + 
  scale_x_discrete(labels = prop_tota$labl) + theme_light() + facet_wrap(~anni_scolarita) + ggtitle("Media proporzione risposte corrette * Media tempo di risposta per ogni item") + theme(axis.text.x = element_text(angle = 90))


item.quartili$item.diff = factor(item.quartili$item.diff, 
                                 levels = c("molto.facili", 
                                            "facili", "medio.diff", "diff"))
# tr per difficoltà unico grafico ------

ggplot(item.quartili, 
       aes(x = item.diff, y = mean.time.item, 
           group=anni_scolarita, color = anni_scolarita, 
           linetype = anni_scolarita)) + 
  geom_point(size = 5) + geom_line(aes(y = mean.time.item), 
                                   size= 1.2)  + theme_light() + ggtitle("Tempi di risposta (solo corrette) per scolarita")


# accuratezza generale delle persone 


d1$correct = ifelse(d1$task_success == "correct", 
                    1, 0)
d1$freq_i.x = NULL
names(d1)[colnames(d1) == "freq_i.y"] = "freq_i"

correct_p = data.frame(table(d1$id, d1$task_success))
colnames(correct_p) = c("id", "response", "freq")
correct_p = merge(correct_p, freq_p, by = "id")
# calcola la proporzione di risp corrette dividendo il numero di risp 
# corrette per il numeor di osservazioni sul soggetto 
correct_p$prop = correct_p$freq/correct_p$freq_p
# prendo solo le risposte corrette ---
only_correct_p = correct_p[correct_p$response %in% "correct", ]
# riordino le proporsioni 
only_correct_p = only_correct_p[order(only_correct_p$prop), ]
only_correct_p$p_cres <- 1:nrow(only_correct_p)
only_correct_p$p_cres <- as.factor(only_correct_p$p_cres)

# unisco scolarità al dataset ----

only_correct_p = merge(only_correct_p, scol, by = "id")

av_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, mean)
colnames(av_p_prop)[2] = "mean.prop"

sd_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, sd)
colnames(sd_p_prop)[2] = "sd.prop"


sbj_prop_summary = merge(av_p_prop, sd_p_prop, 
                         by = "anni_scolarita")
sbj_prop_summary$group = 1

sbj_prop_summary$anni_scolarita = factor(sbj_prop_summary$anni_scolarita, 
                                         levels = as.character(sbj_prop_summary$anni_scolarita)[order(as.character(sbj_prop_summary$anni_scolarita), decreasing = T)])


sbj_prop_summary$anni_scolarita = as.character(sbj_prop_summary$anni_scolarita)


# grafico score medio per anni di scolarità -------

sbj_prop_summary$anni_scolarita = factor(sbj_prop_summary$anni_scolarita, 
                                         levels = c(3, 5,6,7,9, 11:13, 16, 17, 19))
ggplot(sbj_prop_summary, 
       aes(x = anni_scolarita, y = mean.prop)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.prop-sd.prop, 
                    ymax=mean.prop+sd.prop), 
                width = .2) + 
  ylab("Proporzione risposte corrette") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.prop, 
                                group=group)) + ylim(0,1 ) + ggtitle("Score accuratezze delle persone")


# item accuratezze generali ------ 

freq_i_as = data.frame(table(d1$id_question, d1$anni_scolarita))
colnames(freq_i_as) = c("id_question", "anni_scolarita", "freq_i_as")


d1 = merge(d1, freq_i_as, by = c("id_question", "anni_scolarita"))


item_correct_scol = data.frame(table(d1$id_question, 
                                     d1$task_success, d1$anni_scolarita))
colnames(item_correct_scol) = c("id_question", "response", "anni_scolarita", "freq")

item_correct = data.frame(table(d1$id_question, 
                                d1$task_success))
colnames(item_correct) = c("id_question", "response", "freq")



item_correct = merge(item_correct, freq_i, 
                     by = "id_question")
# dividi in quartili sulla proporzione 
# poi plotta sulle scolarità 
# tutto sugli ITEM 

#item_correct_scol=item_correct_scol[item_correct_scol$response %in% "correct", ]


item_correct_scol = merge(item_correct_scol, 
                          freq_i_as, 
                          by = c("id_question", "anni_scolarita"))

item_correct_scol = merge(item_correct_scol, 
                          guide, by = "id_question")

item_correct_scol$prop = item_correct_scol$freq/item_correct_scol$freq_i_as
item_correct_scol$response = factor(item_correct_scol$response, 
                                    levels = c("wrong", "correct"))
item_correct_scol = item_correct_scol[order(item_correct_scol$prop), ]
# Proporizione risposte corrette per scolarità -------
ggplot(item_correct_scol[item_correct_scol$anni_scolarita %in% c(11, 13,5,7,9), ], 
       aes(x = id_question, y = prop, fill = response)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~anni_scolarita) + 
  ggtitle("Proporzione risposte corrette per ogni item") + theme_light() + 
  theme(axis.text.x = element_text(angle = 90))


cor.scol =  aggregate(prop ~ response + anni_scolarita + tipo, item_correct_scol, mean)

cor.scol$response = factor(cor.scol$response, 
                           c("wrong", "correct"))

cor.scol$tipo = factor(cor.scol$tipo, 
                       c("una.cella", "due.celle", "tre.celle"))
ggplot(cor.scol, 
       aes(x = tipo, y = prop, fill = response)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~anni_scolarita) + 
  ggtitle("Proporzione risposte corrette per tipologie di matrice") + 
  theme_light() + 
  geom_hline(yintercept = .25, linetype = 2) + 
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2)


# cose .... 

dist.scol.item = data.frame(table(d1$macro_distractors, d1$anni_scolarita, d1$id_question))
colnames(dist.scol.item) = c("dist", "scol", "id_question", "freq")


tot.dist.scol.item = aggregate(freq ~ scol + id_question, dist.scol.item, sum)
colnames(tot.dist.scol.item)[ncol(tot.dist.scol.item)] = "freq.tot" 

dist.scol.item = merge(dist.scol.item, tot.dist.scol.item, 
                       by = c("scol", "id_question"))

dist.scol.item$prop = dist.scol.item$freq/dist.scol.item$freq.tot

dist.scol.item = merge(dist.scol.item, 
                       guide, by = "id_question")

ggplot(dist.scol.item[dist.scol.item$tipo %in% "due.celle", ], 
       aes(x = id_question, y = prop, fill = dist)) + 
  geom_bar(stat="identity", position = position_dodge(), color = "black") + 
  facet_wrap(~scol) + 
  ggtitle("Proporzione per diversi distrattori: Due cella") + theme_light() +
  xlab("Matrice") + ylab("Proporzione") + 
  geom_hline(yintercept = .25, linetype = 2) + 
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2)

graphs = list()
i=1
for (i in unique(dist.scol.item$scol)) {
  graphs[[i]] = ggplot(dist.scol.item[dist.scol.item$tipo %in% "tre.celle" &  dist.scol.item$scol %in% i, ], 
                       aes(x = id_question, y = prop, fill = dist)) + 
    geom_bar(stat="identity", position = position_dodge(), color = "black")   + theme_light() +
    xlab("Matrice") + ylab("Proporzione") + 
    geom_hline(yintercept = .25, linetype = 2) + 
    geom_hline(yintercept = .5, linetype = 2) +
    geom_hline(yintercept = .75, linetype = 2) + ggtitle(paste("anni scolarita = ", i))
}


graphs[[4]]

ggplot(dist.scol.item[dist.scol.item$tipo %in% "tre.celle" & dist.scol.item$scol %in% 6, ], 
       aes(x = id_question, y = prop, fill = dist)) + 
  geom_bar(stat="identity", position = position_dodge(), color = "black") + 
  facet_wrap(~scol)  + theme_light() +
  xlab("Matrice") + ylab("Proporzione") + 
  geom_hline(yintercept = .25, linetype = 2) + 
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2)

p = d1[d1$id_question %in% 47, ]


table(p$id_question, p$distractors)