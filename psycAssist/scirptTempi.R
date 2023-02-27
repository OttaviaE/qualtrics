# psychAssist script ---- 
# setta la directpry attiva, quella dove è locato il documento
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
library(ggplot2)
library(dplyr)
# importa il data set con le caratteriscctihe degli item -----
guide = read.csv("D:/PRIN2020/qualtrics/skill_young.csv", 
                 header = T, sep = ",")
csv.files = list.files(pattern = ".csv")

dates = gsub("matdata_", "", csv.files)
dates = gsub(".csv", "", dates)
dates = gsub("_", "-", dates)
dates = as.Date(dates)
latest = max(dates)
latest = gsub("-", "_", latest)

# importa il csv con la data più recente -----
data = read.csv(paste0("matdata_", latest, ".csv"), 
                header = T, sep = ",")
head(data)
# fa il check per vedere che  non ci siano proev
table(data$external_code)
table(data$id)


# viene tolto l'utente di prova
data = data[!data$external_code %in% "UTENTE01", ]
#data = data[!data$external_code %in% "PEDY08", ]

pey08 = data[data$external_code %in% "PEDY08", ]
head(pey08)
table(pey08$task_success)

# tolgo i soggetti de clinic_id numero 9 -------
# ATTENZIONE-------
data = data[!data$clinic_id %in% 9,]

# controlla che ogni soggetto abbia solo al massimo 40 osservazioni
table(data$id)




# prendo le variabili (per il momento più utili)
# e toglie tutte le osservazioni che non hanno finito la compilazione
d = data[!data$date_end %in% "NULL", c("id", "gender", "birth_date", 
                                       "anni_scolarita", 
                                       "response_vector", "task_success", "id_question", 
                                       "external_code", "clinic_id",
                                       "created_at.3", "updated_at.3", 
                                       "start_time", "end_time"
                                       )]
# check sul numero di osservazioni per ogni partecipante
table(d$id)

# preparazione tempi di risposta ----
d$time.diff.item = difftime(d$end_time, 
                            d$start_time, 
                            units = "secs")
d$time.diff.item = as.numeric(d$time.diff.item)
sum.time =aggregate(time.diff.item ~ id, 
                    d, sum)
colnames(sum.time)[2] = "sum.time"
# forzo sum.yime ad essere numeric, sia mai che sia il problema 

sum.time$sum.time = as.numeric(sum.time$sum.time)


# faccio il check che sum time contenga effettivamente il totale dei secondi per ogni soggetto 

# temp = NULL 
# check = data.frame(id =  numeric(nrow(sum.time)), 
#                    sum.check = numeric(nrow(sum.time)))
# for (i in 1:nrow(sum.time)) {
#   temp = d[d$id %in% unique(d$id)[i], "time.diff.item"]
#   check[i, "sum.check"] = sum(temp)
#   check[i, "id"] = unique(d$id)[i]
# }
# 
# 
# sum1 = merge(sum.time, check, by = "id")
# # si funziona

# # unisco il dettaglio sui tempi al dataset ----
d = merge(d, sum.time, by="id")


ggplot(d, 
       aes(x = as.factor(clinic_id), y = sum.time, col = anni_scolarita)) + geom_boxplot()


temp = d[d$anni_scolarita %in% 0, ]

max(aggregate(sum.time ~ id, temp, mean)[2]/60)
min(aggregate(sum.time ~ id, temp, mean)[2]/60)

a = aggregate(task_success ~ id, temp, table)
media.tempi.0 = aggregate(sum.time ~ id, temp, mean)
aggregate(time.diff.item ~ id, temp, max) - aggregate(time.diff.item ~ id, temp, min) 
media.tempi.0$prop = a$task_success[,2]/40

media.tempi.0[order(media.tempi.0$sum.time), ]



media.tempi.0$minuti = media.tempi.0$sum.time/60
media.tempi.0[order(media.tempi.0$minuti), ]
media.tempi.0[order(media.tempi.0$prop), ]

ggplot(media.tempi.0, 
       aes(x = prop, y = minuti)) + geom_point(size = 3)
mean(media.tempi.0$minuti)
min(media.tempi.0$minuti)
media.tempi.0[which(media.tempi.0$minuti == min(media.tempi.0$minuti)), ]
media.tempi.0[which(media.tempi.0$minuti == max(media.tempi.0$minuti)), ]


mean((d[which(d$sum.time == min(d$sum.time)), "sum.time" ]))
d[which(d$sum.time == max(d$sum.time)), ]
table(d[which(d$sum.time == min(d$sum.time)), "task_success"])


# scol 2 


temp2 = d[d$anni_scolarita %in% 2, ]

max(aggregate(sum.time ~ id, temp2, mean)[2]/60)
min(aggregate(sum.time ~ id, temp2, mean)[2]/60)

a = data.frame(table(temp2$id, temp2$task_success))
a = a[a$Var2 %in% "True", ]
a$prop = a$Freq/40

media.tempi.2 = aggregate(sum.time ~ id, temp, mean)
aggregate(time.diff.item ~ id, temp, max) - aggregate(time.diff.item ~ id, temp, min) 
media.tempi.2$prop  =a$prop

media.tempi.2[order(media.tempi.2$sum.time), ]

ggplot(media.tempi.2, 
       aes(x = prop, y = minuti)) + geom_point(size = 3)
mean(media.tempi.2$minuti)

media.tempi.2$minuti = media.tempi.2$sum.time/60
media.tempi.2[order(media.tempi.2$minuti), ]
media.tempi.2[order(media.tempi.2$prop), ]



# PREPARAZIONE DISTRATTORI -----
d$response_vector = gsub('[\"]', "", d$response_vector)
d$distractors = gsub(".*_", "", d$response_vector)
table(d$distractors)
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
# ATTENZIONE: FARE CORRERE SOLO UNA VOLTA ------
d$id_question = d$id_question + 1
guide$id_question = 1:40

# UNISCE I DETTAGLI DEGLI ITEM AL DATASET (CREA D1) ----
d1  =merge(d, guide, by = "id_question")
# ricodifica le risposte in corrette ed errate 
d1$task_success = ifelse(d1$task_success == "False",
                         "wrong", "correct")

# calcola il numero di osservazioni per ogni soggetto -----
freq_p = data.frame(table(d1$id))
colnames(freq_p) = c("id", "freq_p")
# lo unisce a d1 --- 
d1 = merge(d1, freq_p, by = "id")

# mi prendo la scolarità e calcolo il numero di bambini per ongi anno di scolarità 
scol = d[, c("id", "anni_scolarita")]
scol = scol %>% 
  distinct()
scol

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

head(correct_i)
colnames(correct_i) = c("id_question", "response", "freq.correct", "freq_i", "prop_c_item", 
                        "item.diff")

table(correct_i$item.diff)
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
ggplot(item.quartili, 
       aes(x = anni_scolarita, y = mean.time.item, group=1)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean.time.item - sd.time.item, 
                    ymax = mean.time.item + sd.time.item), 
                width= .3) + geom_line(aes(y = mean.time.item)) +
  facet_wrap(~item.diff) + theme_light()

# lo faccio distinguendo corrette ed erratte


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

ggplot(item.quartili.c, 
       aes(x = anni_scolarita, y = mean.time.item, group=task_success, color = task_success)) + 
  geom_point(size = 3)  + geom_line(aes(y = mean.time.item), 
                                       size = 1) +
  facet_wrap(~item.diff) + theme_light()


# ora invece che distinguere tra corrette ed errate lo faccio direttamente 
# solo sulle risposte corrette 
# e metto insieme le difficoltà degli item 

d.correct = d1[d1$task_success %in% "correct", ]



item.quartili.mean = aggregate(time.diff.item ~ anni_scolarita + item.diff, 
                               d.correct, 
                               mean)
colnames(item.quartili.mean)[ncol(item.quartili.mean)] = "mean.time.item"

item.quartili.sd = aggregate(time.diff.item ~ anni_scolarita + item.diff, 
                             d.correct, 
                             sd)
colnames(item.quartili.sd)[ncol(item.quartili.sd)] = "sd.time.item"



item.quartili = merge(item.quartili.mean, 
                      item.quartili.sd, by = c("anni_scolarita", "item.diff"))

item.quartili$group = 1
ggplot(item.quartili, 
       aes(x = anni_scolarita, y = mean.time.item, 
           group=item.diff, color = item.diff)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean.time.item - sd.time.item, 
                    ymax = mean.time.item + sd.time.item), 
                width= .3) + geom_line(aes(y = mean.time.item))  + theme_light()


ggplot(item.quartili, 
       aes(x = anni_scolarita, y = mean.time.item, 
           group=item.diff, color = item.diff)) + 
  geom_point(size = 5) + geom_line(aes(y = mean.time.item), 
                                   size= 1.2)  + theme_light()


# lo faccio anche per i singoli item -----


ggplot(d1, 
       aes(x = anni_scolarita, 
           y = sum.time)) + geom_boxplot() + 
  ggtitle("Distribuzione totale risposte per scolarità") + 
  ylab("Tempo totale in secondi")


av_p_time = aggregate(sum.time ~ anni_scolarita, d1, mean)
colnames(av_p_time)[2] = "mean.time"

sd_p_time = aggregate(sum.time ~ anni_scolarita, d1, sd)
colnames(sd_p_time)[2] = "sd.time"


sbj_time_summary = merge(av_p_time, sd_p_time, 
                         by = "anni_scolarita")
sbj_time_summary$group = 1
# grafico tempo di risposta medio per anni di scolarità -------
ggplot(sbj_time_summary, 
       aes(x = anni_scolarita, y = mean.time)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.time-sd.time, 
                    ymax=mean.time+sd.time), 
                width = .2) + 
  ylab("Media Tempo totale in secondi") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.time, 
                                group=group)) + ylim(50, 550)


# faccio la stessa cosa per le proporzioni delle risposte corrette 


av_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, mean)
colnames(av_p_prop)[2] = "mean.prop"

sd_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, sd)
colnames(sd_p_prop)[2] = "sd.prop"


sbj_prop_summary = merge(av_p_prop, sd_p_prop, 
                         by = "anni_scolarita")
sbj_prop_summary$group = 1
# grafico tempo di risposta medio per anni di scolarità -------
ggplot(sbj_prop_summary, 
       aes(x = anni_scolarita, y = mean.prop)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.prop-sd.prop, 
                    ymax=mean.prop+sd.prop), 
                width = .2) + 
  ylab("Proporzione risposte corrette") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.prop, 
                                group=group)) + ylim(0,1 )
library(gridExtra)


grid.arrange(ggplot(sbj_prop_summary, 
                    aes(x = anni_scolarita, y = mean.prop)) + geom_point(size = 4) +
               geom_errorbar(aes(ymin = mean.prop-sd.prop, 
                                 ymax=mean.prop+sd.prop), 
                             width = .2) + 
               ylab("Proporzione risposte corrette") + xlab("Anni di scolarità") +
               theme_light() + geom_line(aes(x=anni_scolarita, y = mean.prop, 
                                             group=group)) + ylim(0,1 ), 
             ggplot(sbj_time_summary, 
                    aes(x = anni_scolarita, y = mean.time)) + geom_point(size = 4) +
               geom_errorbar(aes(ymin = mean.time-sd.time, 
                                 ymax=mean.time+sd.time), 
                             width = .2) + 
               ylab("Media Tempo totale in secondi") + xlab("Anni di scolarità") +
               theme_light() + geom_line(aes(x=anni_scolarita, y = mean.time, 
                                             group=group)) + ylim(50, 550))

# grafico tempo di risposta medio per anni di scolarità -------
ggplot(sbj_time_summary, 
       aes(x = anni_scolarita, y = mean.time)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.time-sd.time, 
                    ymax=mean.time+sd.time), 
                width = .2) + 
  ylab("Media Tempo totale in secondi") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.time, 
                                group=group)) + ylim(50, 550)

# item e scolarità ------

# calcolo la proporzione di risposte corrette nei diversi gruppi di scolarità -----
# prima cosa devo sapere quante volte viene presentato ogni item in ogni anno 
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


ggplot(item_correct_scol, 
       aes(x = id_question, y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~anni_scolarita) + ggtitle("Proporzione risposte corrette per ogni item")


ggplot(item_correct_scol, 
       aes(x = tipo, y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~anni_scolarita) + 
  ggtitle("Proporzione risposte corrette per tipologie di matrice")

# calcolo il tempo di risposta medio per ogni item ----
d_correct = d1[d1$task_success %in% "correct", ]

av_i_time = aggregate(time.diff.item ~ anni_scolarita + id_question, 
                      d_correct, mean)
colnames(av_i_time)[3] = "mean.time"

sd_i_time = aggregate(time.diff.item ~ anni_scolarita + id_question, 
                      d_correct, sd)
colnames(sd_i_time)[3] = "sd.time"


i_time_summary = merge(av_i_time, sd_i_time, 
                         by = c("anni_scolarita", "id_question"))

i_time_summary$group = 1
# in i_time_summary ho le medie dei tempi di risposta per ogni item a sedonda dell'anno
# di scolarita 
#
ggplot(i_time_summary, 
       aes(x = (anni_scolarita), 
           y = mean.time)) + 
  geom_point() + facet_wrap(~id_question) + ylim(0, 10)

ggplot(i_time_summary[i_time_summary$id_question %in% c(7, 11, 21, 22, 14, 17, 18, 24, 27, 34, 39), ], 
       aes(x = (anni_scolarita), 
           y = mean.time)) + 
  geom_point() + facet_wrap(~id_question) 


# sto sbagliando qualcosa ---- 
# provo comunque a calcolare le medie per anni di scolarità sugli item 

av_i_time_scol = aggregate(mean.time ~ anni_scolarita, i_time_summary, mean)
colnames(av_i_time_scol)[2] = "av_time"
sd_i_time_scol = aggregate(mean.time ~ anni_scolarita, i_time_summary, sd)
colnames(sd_i_time_scol)[2] = "sd_time"


summary_item_time = merge(av_i_time_scol, 
                          sd_i_time_scol, 
                          by = "anni_scolarita")
summary_item_time$group =1 

ggplot(summary_item_time, 
       aes(x = anni_scolarita, y = av_time)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = av_time-sd_time, 
                    ymax=av_time+sd_time), 
                width = .2) + 
  ylab("Medie risposte agli item") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = av_time, 
                                group=group))

# ora mi piacerebbe fare i grafici delle medie agli item per i diversi anni di scolarità

av_i_time_scol_cw = aggregate(time.diff.item ~ anni_scolarita + id_question + task_success, 
                              d1, mean)
colnames(av_i_time_scol_cw)[ncol(av_i_time_scol_cw)] = "av_time"
sd_i_time_scol_cw = aggregate(time.diff.item ~ anni_scolarita + id_question + task_success, 
                              d1, sd)
colnames(sd_i_time_scol_cw)[ncol(sd_i_time_scol_cw)] = "sd_time"


summary_item_time_cw = merge(av_i_time_scol_cw, 
                             sd_i_time_scol_cw, 
                             by = c("anni_scolarita", "id_question", "task_success"))
summary_item_time_cw$group =1 

av_i_time_scol_cw_s = aggregate(av_time ~ anni_scolarita + task_success, 
                              av_i_time_scol_cw, mean)
colnames(av_i_time_scol_cw_s)[ncol(av_i_time_scol_cw_s)] = "av_time"
sd_i_time_scol_cw_s = aggregate(av_time ~ anni_scolarita + task_success, 
                                av_i_time_scol_cw, sd)
colnames(sd_i_time_scol_cw_s)[ncol(sd_i_time_scol_cw_s)] = "sd_time"


summary_item_time_cw_s = merge(av_i_time_scol_cw_s, 
                             sd_i_time_scol_cw_s, 
                             by = c("anni_scolarita",  "task_success"))


# devo ricalcolare la media sulle average e 

ggplot(summary_item_time_cw_s, 
       aes(x = anni_scolarita, y = av_time, 
           color = task_success)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = av_time-sd_time, 
                    ymax=av_time+sd_time, 
                    group = task_success, 
                    color = task_success), 
                width = .2) + 
  ylab("Medie risposte agli item") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = av_time, 
                                group=task_success))



# ho sbagliato qualcosa 

# separo gli item per anno di scolarita direttamente dal dataset
scol0 = d1[d1$anni_scolarita %in% 0, ]
scol2 = d1[d1$anni_scolarita %in% 2, ]
scol3 = d1[d1$anni_scolarita %in% 3, ]
scol5 = d1[d1$anni_scolarita %in% 5, ]

# prima mi calcolo le proprozioni corrette degli item per ogni anno di scolarita 
# poi calcolo i temopi e metto tutto insieme 
# scol 0 calcola totale per ogni item 

freq_i_scol0 = data.frame(table(scol0$id_question))
colnames(freq_i_scol0) = c("id_question", "freq_i_scol")

# calcolo la proporzione di risposte corrette per ogni item 
correct_i_scol0 = data.frame(table(scol0$id_question, scol0$task_success))
colnames(correct_i_scol0) = c("id_question", "response", "freq")
correct_i_scol0 = correct_i_scol0[correct_i_scol0$response %in% "correct", ]
correct_i_scol0 = merge(correct_i_scol0, freq_i_scol0, by = "id_question")
correct_i_scol0$prop = correct_i_scol0$freq/correct_i_scol0$freq_i_scol

# metto subito il tempo di reazione medio per ogni stimolo 

av_i_sol0 = aggregate(time.diff.item ~ id_question, scol0, mean)
colnames(av_i_sol0)[2] = "av_time"


correct_i_scol0 = merge(correct_i_scol0,av_i_sol0, by = "id_question")
correct_i_scol0 = correct_i_scol0[order(correct_i_scol0$prop), ]
correct_i_scol0$new_id_q = as.factor(1:nrow(corrcet_i_scol0))

ggplot(correct_i_scol0, 
       aes(x = av_time, y = prop)) + geom_point()

## ??? non capisco ma non funziona ---- 

av_i_prop = aggregate(prop ~ anni_scolarita, item_correct_scol, mean)
colnames(av_i_prop)[2] = "mean.prop"

sd_i_prop = aggregate(prop ~ anni_scolarita, item_correct_scol, sd)
colnames(sd_i_prop)[2] = "sd.prop"


item_prop_summary = merge(av_i_prop, sd_i_prop, 
                         by = "anni_scolarita")
item_prop_summary$group = 1

ggplot(item_prop_summary, 
       aes(x = anni_scolarita, y = mean.prop)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.prop-sd.prop, 
                    ymax=mean.prop+sd.prop), 
                width = .2) + 
  ylab("Proporzione risposte corrette") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.prop, 
                                group=group)) + ylim(0,1 )
