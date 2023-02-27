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

data[data$id %in% 154, "birth_date"]

# viene tolto l'utente di prova
data = data[!data$external_code %in% "UTENTE01", ]

# controlla che ogni soggetto abbia solo al massimo 40 osservazioni
table(data$id)




# prendo le variabili (per il momento più utili)
# e toglie tutte le osservazioni che non hanno finito la compilazione
d = data[!data$date_end %in% "NULL", c("id", "gender", "birth_date", 
                                       "anni_scolarita", 
             "response_vector", "task_success", "id_question", 
             "date_begin", "date_end", 
             "external_code", 
             "created_at.3", "updated_at.3", "start_time", "end_time")]
# check sul numero di osservazioni per ogni partecipante
table(d$id)

# preparazione tempi di risposta ----
d$time.diff = difftime(d$date_end, d$date_begin, units = "mins")
d$time.diff.item = difftime(d$updated_at.3, d$created_at.3, units = "mins")
sum.time =aggregate(time.diff.item ~ id, d, sum)
colnames(sum.time)[2] = "sum.time"
d$time.diff.item1 = difftime(d$end_time, d$start_time, units = "mins")

# unisco il dettaglio sui tempi al dataset ----
d = merge(d, sum.time, by="id") %>% 

# PREPARAZIONE DISTRATTORI ----
# creo una nuova colonna per i distrattori
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
# ATTENZIONE: FARE CORRERE SOLO UNA VOLTA 
d$id_question = d$id_question + 1
guide$id_question = 1:40

# UNISCE I DETTAGLI DEGLI ITEM AL DATASET 
d1  =merge(d, guide, by = "id_question")
# ricodifica le risposte in corrette ed errate 
d1$task_success = ifelse(d1$task_success == "False",
                         "wrong", "correct")

# calcola il numero di osservazioni per ogni soggetto -----
freq_p = data.frame(table(d1$id))
colnames(freq_p) = c("id", "freq_p")

# mi prendo la scolarità 
scol = d[, c("id", "anni_scolarita")]
scol = scol %>% 
  distinct()
scol

d1 = merge(d1, scol, by = "id")


# unissce il totale di obs sulle persone al dataset -----
d1 = merge(d1, freq_p, by = "id")

# calcola il numero di osservazioni su ogni item ------
freq_i = data.frame(table(d1$id_question))
colnames(freq_i) = c("id_question", "freq_i")



# unissce il totale di obs sugli item al dataset -----
d1 = merge(d1, freq_i, by = "id_question")

# crea il numero di risposte corrette per ogni soggetto 
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


# item -----
# numero risposte corrette/errate per ogni item
correct_i = data.frame(table(d1$id_question, d1$task_success))
colnames(correct_i) = c("id_question", "response", "freq")
# unisco con la frequenza di risposta su ogni item ----
correct_i = merge(correct_i, freq_i, by = "id_question")
# calcolo la proporzione di risposte corrette su ogni item 
correct_i$prop = correct_i$freq/correct_i$freq_i
# prendo solo le risposte corrette 
only_correct_i = correct_i[correct_i$response %in% "correct", ]
# isolo i dettagli sull'item
i.type = d1[, c("id_question", "tipo", "colore")]
i.type = i.type %>% 
  distinct()

#li unisco alle proporzioni di item 
only_correct_i = merge(only_correct_i, 
                       i.type, 
                       by = "id_question")

only_correct_i = only_correct_i[order(only_correct_i$prop), ]


only_correct_i$item_cres = 1:nrow(only_correct_i)
only_correct_i$item_cres = as.factor(only_correct_i$item_cres)



ggplot(only_correct_i, 
       aes(x = item_cres, y = prop)) + 
  geom_point() + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank()) + ylim(0, 1) + 
  geom_hline(yintercept = .25, linetype = 2) +
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2) + ggtitle("Proporzione di risposte corrette per ogni item")


ggplot(only_correct_i, 
       aes(x = item_cres, y = prop, fill = tipo)) + 
  geom_bar(stat = "identity") + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank()) + ylim(0, 1) + 
  theme(legend.position = "bottom") + 
  ggtitle("Proporzione di risposte corrette per ogni item per tipo")



ggplot(only_correct_i, 
       aes(x = item_cres, y = prop, fill = colore)) + 
  geom_bar(stat = "identity") + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank()) + ylim(0, 1) + 
  theme(legend.position = "bottom")  + 
  ggtitle("Proporzione di risposte corrette per ogni item per colore")  + 
  geom_hline(yintercept = .25, linetype = 2) +
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2) 

only_correct_i$tipo.colore = paste(only_correct_i$tipo, 
                                   only_correct_i$colore, 
                                   sep = ".") +  
  geom_hline(yintercept = .25, linetype = 2) +
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2) 

ggplot(only_correct_i, 
       aes(x = item_cres, y = prop, fill = colore)) + 
  geom_bar(stat = "identity") + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank()) + ylim(0, 1) + 
  theme(legend.position = "bottom") + facet_grid(~tipo) +
  geom_hline(yintercept = 0.25, linetype = 2) + 
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2) + 
  ggtitle("Proporzione di risposte corrette per ogni item per colore e tipo") 


# distrattori ----
# crea la frequenza con cui è stato scelto ogni distrattore per ogni item
responses = data.frame(table(d1$id_question, d1$macro_distractors))
colnames(responses) = c("id_question", "response", "freq")
# lo unisco al numero di volte in cui è stato presentato ogni item 
responses = merge(responses, 
                  freq_i, by = "id_question")
responses$prop = responses$freq/responses$freq_i
# lo unisco alle caratteristiche degli item ----
responses = merge(responses, i.type, 
                  by = "id_question")
# cerco di appaiare gli item colorati e non ----
responses$new.id = responses$id_question
responses[responses$id_question %in% c(13:19), "new.id"] = responses[responses$id_question %in% c(6:12), "new.id"] 
responses[responses$id_question %in% c(23:25), "new.id"] = responses[responses$id_question %in% c(20:22), "new.id"] 
responses[responses$id_question %in% c(33:36), "new.id"] = responses[responses$id_question %in% c(29:32), "new.id"] 


ggplot(responses, 
       aes(x = response, y = prop, fill = tipo)) + geom_bar(stat = "identity") + 
  facet_wrap(~id_question) + 
  geom_hline(yintercept = 0.25, linetype = 2) + 
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2) + theme_light() +
  theme(legend.position = "bottom") + 
  ggtitle("Proporzione di risposte corrette, item,distrattore,colore,tipo") 


# comviene isolare gli item per cui ho l'appaiamento colore e non colore 

item.small = responses[responses$new.id %in% c(6:12, 20:22, 29:32), ]


ggplot(item.small[item.small$tipo %in% "due.celle", ], 
       aes(x = response, y = prop, fill = colore)) + facet_wrap(~new.id*tipo) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_hline(yintercept = 0.25, linetype = 2) + 
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2) + theme_light() +
  theme(legend.position = "bottom") +
  ggtitle("Proporzione di risposte corrette, item,distrattore,colore, DUE celle") 



ggplot(item.small[item.small$tipo %in% "tre.celle", ], 
       aes(x = response, y = prop, fill = colore)) + facet_wrap(~new.id*tipo) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_hline(yintercept = 0.25, linetype = 2) + 
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2) + theme_light() +
  theme(legend.position = "bottom") +
  ggtitle("Proporzione di risposte corrette, item,distrattore,colore, TRE celle") 



# check sul time -----

a = data.frame(table(d$id, d$sum.time))
a = a[a$Freq !=0, ]

colnames(a) = c("id", "minuti", "freq_min")

p = merge(a, only_correct_p, by = "id")

plot(p$minuti, p$freq)
p$minuti = as.character(p$minuti)
p$minuti = as.numeric(p$minuti)
# time soggeti per frequenze corrette ----
ggplot(p, 
       aes(x = as.numeric(minuti), y = freq)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Tempo*Risposte corrette")

# distribuzioni item ----- 

d.item =d1[, c("id_question", "sum.time", "colore", "tipo", "task_success", 
               "macro_distractors", "distractors", "time.diff.item")]
d.item$sum.time = as.numeric(d.item$sum.time)
d.item$time.diff.item = as.numeric(d.item$time.diff.item)

ggplot(d.item, 
       aes(x =task_success, y = time.diff.item)) + 
  geom_violin(trim = F) + 
  facet_wrap(~id_question) + ylim(0,.25) + 
  ggtitle("Distribuzioni corrette errate per item")

aggregate(time.diff.item ~ id_question + task_success, d.item,  mean)

ggplot(d.item, 
       aes(x =task_success, y = time.diff.item)) + 
  geom_boxplot() + 
  facet_wrap(~id_question) + ylim(0,.25) + 
  ggtitle("Distribuzioni corrette errate per item")


ggplot(d.item, 
       aes(x =macro_distractors, y = time.diff.item)) + geom_violin(trim = F) + 
  facet_wrap(~id_question) + ylim(0,.25) + 
  ggtitle("Distribuzioni corrette errate per item e DISTRATTORE (MACRO)")

ggplot(d.item, 
       aes(x =macro_distractors, y = time.diff.item)) + geom_boxplot() + 
  facet_wrap(~id_question) + ylim(0,.25)  + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Distribuzioni corrette errate per item e DISTRATTORE")


ggplot(d.item, 
       aes(x =macro_distractors, y = time.diff.item)) + geom_boxplot() + 
  facet_wrap(~tipo) + ylim(0,.25)  + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Distribuzioni corrette errate per item e DISTRATTORE")


ggplot(d.item[d.item$id_question %in% c(21, 11, 22, 24, 34), ], 
       aes(x =macro_distractors, y = time.diff.item)) + geom_boxplot() + 
  facet_wrap(~id_question)  + 
  theme(axis.text.x = element_text(angle = 90))


resp = data.frame(table( d1$id_question, d1$distractors))
colnames(resp) = c("id_question", "response", "freq")
resp = merge(resp, 
             freq_i, by = "id_question")
resp$prop = resp$freq/resp$freq_i
resp = merge(resp, i.type, 
             by = "id_question")


d.item1 = merge(d.item, 
                resp, 
                by = "id_question")

ggplot(d.item1[d.item1$id_question %in% 26, ], 
       aes(x =distractors, y = prop)) + geom_bar(stat="identity") + 
  facet_wrap(~id_question) 


