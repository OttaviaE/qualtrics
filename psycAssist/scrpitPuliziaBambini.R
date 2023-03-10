# facciamo le cose ordinate--- 
rm(list = ls())
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

# primo check per vedere la percentuale di risposte corrette 

temp = data[, c("id", "id_question", "task_success")]


temp$task_success = ifelse(temp$task_success == "True", 
                           1, 0)
wide.temp = reshape(temp, 
                    idvar = "id", 
                    timevar = "id_question", 
                    direction = "wide")
wide.temp$total_score = rowSums(wide.temp[,-1], na.rm = T)

ggplot(wide.temp, 
       aes(x=as.factor(id), y = total_score)) + geom_point() +
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
           y = quantile(wide.temp$total_score)[2], label = "Q1")

# prendo quelli che hanno fatto almeno il 20% del punteggio totale ottenibile, gli altri per il 
# momento li escludo 

wide.temp$filter = ifelse(wide.temp$total_score <= length(unique(data$id_question))*0.20, 
                          "out", "in")

# prendo i nomi di quelli che hanno un punetggio sotto il 20% 

sbj.out = wide.temp[wide.temp$filter %in% "out", "id"]


# prima di toglierli dal dataset totale, guardo l'efftto dello sperimentatore sul tempo 
# e sul punteggio totale 
# prima cosa: calcolo il tempo 
# tolgo quelli che non hanno finito 
data = data[!data$date_end %in% "NULL", c("id", "gender", "birth_date", 
                                       "anni_scolarita", "city",
                                       "response_vector", "task_success", "id_question", 
                                       "external_code", "clinic_id",
                                       "start_time", "end_time"
)]
# check sul numero di osservazioni per ogni partecipante

table(data$id)

# preparazione tempi di risposta ----
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

wide.small = wide.temp[, c("id", "total_score", "filter")]

data = merge(data, wide.small, by = "id")

# guardo l'effetto dello sperimentatore sui tempi e sulle proporzioni 
# grafici effetto sperimentatore -------
data$city = tolower(data$city)
ggplot(data, 
       aes(x = as.factor(clinic_id), y = sum.time, 
           color = anni_scolarita)) + geom_boxplot() + 
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
           color = anni_scolarita)) + geom_boxplot() + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("città") +
  ylab("Score accuratezze") + 
  ggtitle("Distribuzione score accuratezze per città") + 
  theme(axis.text.x = element_text(angle = 90))


# ora tolto i bambini che hanno un punteggio inferiore al 20% -----

d = data[data$filter %in% "in", ]


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

# SOLO PER ADESSO RICODIFICO NULL IN 0 ------
d1[d1$anni_scolarita %in% "NULL", "anni_scolarita"] = 0

# mi prendo la scolarità e calcolo il numero di bambini per ongi anno di scolarità 
scol = d1[, c("id", "anni_scolarita")]
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
mapping_item = correct_i[, c("id_question", "item.diff")]
# unicsco al dataset grande il mapping degli item (difficili, facili ecc)


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
# tempi di risposta per scolarita e per difficoltà degli item -----
ggplot(item.quartili, 
       aes(x = anni_scolarita, 
           y = mean.time.item, 
           group=item.diff, color = item.diff)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.time.item - sd.time.item, 
                    ymax = mean.time.item + sd.time.item), 
                width= .3, size = .8) + geom_line(aes(y = mean.time.item), 
                                       size = 1.3) + 
  theme_light() + ggtitle("Tempi di risposta medi per scolarita e difficoltà degli item")


# stesso grafico ma senza error bar
ggplot(item.quartili, 
       aes(x = anni_scolarita, 
           y = mean.time.item, 
           group=item.diff, color = item.diff)) + 
  geom_point(size = 4) + geom_line(aes(y = mean.time.item), 
                                                  size = 1.3) + 
  theme_light() + ggtitle("Tempi di risposta medi per scolarita e difficoltà degli item")

# grafico di prima ma distinguendo corrette ed errate ------
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
  facet_wrap(~item.diff) + theme_light() + ggtitle("Tempi di risposta per difficiotlà dell'item e scolairtà")

# aggiungo le error bar 
ggplot(item.quartili.c, 
       aes(x = anni_scolarita, y = mean.time.item, group=task_success, color = task_success)) + 
  geom_point(size = 3)  + geom_line(aes(y = mean.time.item), 
                                    size = 1) +
  facet_wrap(~item.diff) + theme_light() + 
  geom_errorbar(aes(ymin = mean.time.item - sd.time.item, 
                    ymax = mean.time.item + sd.time.item), 
                width= .3, size = .8) + ggtitle("Tempi di risposta per difficiotlà dell'item e scolairtà")

# stessi grafici ma questa volta SOLO SULLE RISPOSTE CORRETTE -----

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
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.time.item - sd.time.item, 
                    ymax = mean.time.item + sd.time.item), 
                width= .3) + 
  geom_line(aes(y = mean.time.item), 
            size = 1.2)  + theme_light() + ggtitle("Tempi di risposta (solo corrette) per scolarita")
# Tempi di risposta (solo corrette) per scolarita grafico -----

ggplot(item.quartili, 
       aes(x = anni_scolarita, y = mean.time.item, 
           group=item.diff, color = item.diff)) + 
  geom_point(size = 5) + geom_line(aes(y = mean.time.item), 
                                   size= 1.2)  + theme_light() + ggtitle("Tempi di risposta (solo corrette) per scolarita")



# passo alle accuratezze .----- 

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

# grafico score corretti per ogni soggetto per scolairtà -----
ggplot(only_correct_p, 
       aes(x = p_cres, y = prop, 
           col = anni_scolarita, 
           shape = anni_scolarita)) + 
  geom_point(size = 2) + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank(), 
        legend.position = "bottom") + 
  ggtitle("Proporzione risposte corrette per scolarità") +
  geom_hline(yintercept = 0.25, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2)

av_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, mean)
colnames(av_p_prop)[2] = "mean.prop"

sd_p_prop = aggregate(prop ~ anni_scolarita, only_correct_p, sd)
colnames(sd_p_prop)[2] = "sd.prop"


sbj_prop_summary = merge(av_p_prop, sd_p_prop, 
                         by = "anni_scolarita")
sbj_prop_summary$group = 1
# grafico score medio per anni di scolarità -------
ggplot(sbj_prop_summary, 
       aes(x = anni_scolarita, y = mean.prop)) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.prop-sd.prop, 
                    ymax=mean.prop+sd.prop), 
                width = .2) + 
  ylab("Proporzione risposte corrette") + xlab("Anni di scolarità") +
  theme_light() + geom_line(aes(x=anni_scolarita, y = mean.prop, 
                                group=group)) + ylim(0,1 )


# item -----
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

# Proporizione risposte corrette per scolarità -------
ggplot(item_correct_scol, 
       aes(x = id_question, y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~anni_scolarita) + 
  ggtitle("Proporzione risposte corrette per ogni item") + theme_light()

# Proporizione risposte corrette per scolarità (tipo di item)-------

ggplot(item_correct_scol, 
       aes(x = tipo, y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~anni_scolarita) + 
  ggtitle("Proporzione risposte corrette per tipologie di matrice") + 
  theme_light()


ggplot(item_correct_scol, 
       aes(x = anni_scolarita, y = prop, fill = response)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~tipo*colore) + 
  ggtitle("Proporzione risposte corrette per tipologie di matrice e colore") + 
  theme_light()

dist.scol = data.frame(table(d1$macro_distractors, d1$anni_scolarita, d1$tipo, d1$colore))
colnames(dist.scol) = c("dist", "scol", "tipo", "colore", "freq")


tot.dist.scol = aggregate(freq ~ scol + tipo + colore, dist.scol, sum)
colnames(tot.dist.scol)[4] = "freq.tot"


dist.scol = merge(dist.scol, tot.dist.scol, by = c("scol", "tipo", "colore"))
dist.scol$prop = dist.scol$freq/dist.scol$freq.tot

dist.scol = dist.scol[!is.nan(dist.scol$prop), ]


ggplot(dist.scol, 
       aes(x = scol, y = prop, fill = dist)) + 
  geom_bar(stat="identity", position = position_dodge(), color = "black") + 
  facet_wrap(~tipo*colore) + 
  ggtitle("Proporzione per diversi distrattori") + theme_light()


# provo a isolare i bambini di don bosco 
# hanno external code pedy 
# codifica scuola ------
small = d1


for (i in 1:nrow(small)){
  if (grepl("PEDY", small[i, "external_code"]) == T) {
    small[i, "school"] = "don.bosco"
  } else if (grepl("PEDX", small[i, "external_code"])) {
    small[i, "school"] = "don.milani"
  } else {
    small[i, "school"] = paste("città", small[i,"city"], sep = " ")
  }
}
table(small$school)
small[small$school %in% "scuola.bosco", "external_code"]

ggplot(small, 
       aes(x = as.factor(school), y = total_score, 
           color = anni_scolarita)) + geom_boxplot(size = 1.1) + 
  theme_light() + 
  theme(legend.position = "bottom") + xlab("") +
  ylab("Score accuratezze") + 
  ggtitle("Distribuzione score accuratezze per città") + 
  theme(axis.text.x = element_text(angle = 90))
