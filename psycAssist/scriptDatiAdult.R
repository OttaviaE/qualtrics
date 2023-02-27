# psychAssist script ---- 
# current_path = rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path))
setwd("D:/PRIN2020/qualtrics/psycAssist/Adulti")
library(ggplot2)
library(dplyr)
guide = data.frame(id_question = 1:52, 
                   tipo = rep(c("due.celle", "tre.celle"), c(3, 49)))
csv.files = list.files(pattern = ".csv")

dates = gsub("matdata_adults_", "", csv.files)
dates = gsub(".csv", "", dates)
dates = gsub("_", "-", dates)
dates = as.Date(dates)
latest = max(dates)
latest = gsub("-", "_", latest)


data = read.csv(paste0("matdata_adults_", latest, ".csv"), 
                header = T, sep = ",")

head(data)
# prendo le variabili (per il momento più utili)
data = data[!data$external_code %in% "PROVA", ]
d = data[!data$date_end %in% "NULL", c("id", "gender", "birth_date", "anni_scolarita", 
                                       "response_vector", "task_success", "id_question", "date_begin", "date_end", 
                                       "external_code", 
                                       "created_at.3", "updated_at.3")]
table(d$id)
# creo una nuova colonna per i distrattori
d$response_vector = gsub('[\"]', "", d$response_vector)

d$time.diff = difftime(d$date_end, d$date_begin, units = "mins")
d$time.diff.item = difftime(d$updated_at.3, d$created_at.3, units = "mins")
sum.time =aggregate(time.diff.item ~ id, d, sum)
colnames(sum.time)[2] = "sum.time"

d$time.diff = difftime(d$date_end, d$date_begin, units = "mins")
d = merge(d, sum.time, by="id")




# creo una nuova colonna per i distrattori
d$response_vector = gsub('[\"]', "", d$response_vector)



d$distractors = gsub(".*_", "", d$response_vector)
table(d$distractors)
d$distractors = gsub(".svg]", "", d$distractors)
d$macro_distractors = numeric(nrow(d))

for (i in 1:nrow(d)) {
  if(d[i, "distractors"] == "d.union" | d[i, "distractors"] == "diff1" | d[i, "distractors"] == "diff2") {
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
table(d$macro_distractors)

d$id_question = d$id_question + 1
guide

d1  =merge(d, guide, by = "id_question")



d1$task_success = ifelse(d1$task_success == "False",
                         "wrong", "correct")

freq_p = data.frame(table(d1$id))
colnames(freq_p) = c("id", "freq_p")

freq_i = data.frame(table(d1$id_question))
colnames(freq_i) = c("id_question", "freq_i")

d1 = merge(d1, freq_p, by = "id")

d1 = merge(d1, freq_i, by = "id_question")
data.frame(table(d1$tipo))


data.frame(table(d1$tipo, d1$macro_distractors, d1$id_question))
freq_p
d1
correct_p = data.frame(table(d1$id, d1$task_success))
colnames(correct_p) = c("id", "response", "freq")

correct_p = merge(correct_p, freq_p, by = "id")
correct_p$prop = correct_p$freq/correct_p$freq_p

only_correct_p = correct_p[correct_p$response %in% "correct", ]


only_correct_p = only_correct_p[order(only_correct_p$prop), ]
only_correct_p$p_cres <- 1:nrow(only_correct_p)
only_correct_p$p_cres <- as.factor(only_correct_p$p_cres)
scol = d[, c("id", "anni_scolarita")]
scol = scol %>% 
  distinct()


only_correct_p = merge(only_correct_p, scol, by = "id")

ggplot(only_correct_p, 
       aes(x = p_cres, y = prop, col = anni_scolarita, 
           shape = anni_scolarita)) + 
  geom_point(size = 4) + theme_light() +
  theme(axis.title.x = element_blank(), 
        axis.text.x =element_blank()) + ylim(0,1) + 
  ggtitle("Proporzione risposte corrette per scolarità") +
  geom_hline(yintercept = 0.25, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2)


# item ----- 

correct_i = data.frame(table(d1$id_question, d1$task_success))
colnames(correct_i) = c("id_question", "response", "freq")

correct_i = merge(correct_i, freq_i, by = "id_question")
correct_i$prop = correct_i$freq/correct_i$freq_i



only_correct_i = correct_i[correct_i$response %in% "correct", ]
i.type = d1[, c("id_question", "tipo")]

i.type = i.type %>% 
  distinct()


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
  ggtitle("Proporzione risposte corrette per ogni item") +
  geom_hline(yintercept = 0.25, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2)




# distrattori ----

responses = data.frame(table(d1$id_question, d1$macro_distractors))
colnames(responses) = c("id_question", "response", "freq")
responses = merge(responses, 
                  freq_i, by = "id_question")
responses$prop = responses$freq/responses$freq_i
responses = merge(responses, i.type, 
                  by = "id_question")
ggplot(responses, 
       aes(x = response, y = prop, fill = tipo)) + geom_bar(stat = "identity") + 
  facet_wrap(~id_question) +
  geom_hline(yintercept = .25, linetype=2) +
  geom_hline(yintercept = .5, linetype=2) +
  geom_hline(yintercept = .75, linetype=2) + 
  ggtitle("Proporzione risposte per distrattore") +
  geom_hline(yintercept = 0.25, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.75, linetype = 2) + 
  theme(legend.position = "bottom")

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
  ggtitle("Relazione tempo * risposte corrette")

# distribuzioni item ----- 

d1
d.item =d1[, c("id_question", "sum.time",  "tipo", "task_success", 
               "macro_distractors", "distractors", "time.diff.item")]
d.item$sum.time = as.numeric(d.item$sum.time)
d.item$time.diff.item = as.numeric(d.item$time.diff.item)

ggplot(d.item, 
       aes(x =task_success, y = time.diff.item)) + 
  geom_violin(trim = F) + 
  facet_wrap(~id_question) + 
  ggtitle("Distribuzione tempi di risposta giuste/sbagliate")

ggplot(d.item, 
       aes(x =task_success, y = time.diff.item)) + geom_boxplot() + 
  facet_wrap(~id_question) + 
  ggtitle("Distribuzione tempi di risposta giuste/sbagliate")

ggplot(d.item, 
       aes(x =macro_distractors, y = time.diff.item)) + geom_violin(trim = F) + 
  facet_wrap(~id_question)  + 
  ggtitle("Distribuzioni corrette errate per item e DISTRATTORE (MACRO)")


ggplot(d.item, 
       aes(x =distractors, y = time.diff.item)) + geom_boxplot() + 
  facet_wrap(~id_question)  + 
  ggtitle("Distribuzioni corrette errate per item e DISTRATTORE (micro)") + 
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

ggplot(d.item1, 
       aes(x =distractors, y = prop)) + geom_bar(stat="identity") + 
  facet_wrap(~id_question) 


