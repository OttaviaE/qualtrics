# mi concentro sull'analisi delle coppie equibvalenti di stimoli 
# provo anche a vedre se c'è dif nei gruppi di giovanissimi e non giovanissimi 

library(psych)
library(corrplot)
library(ggplot2)
library(car)
library(dplyr)
library(xtable)
library(mokken)
library(lavaan)
library(TAM)
library(lavaanPlot)
library(difR)
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}


irt.icc = function(model) {
  item_par = model$item
  est_theta = seq(-4,4, length.out=1000)
  item_prob = list()
  if (any(grep("guess", colnames(item_par))) == F) {
    for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta, 
                                b = item_par[i, "xsi.item"], 
                                a = item_par[i, "B.Cat1.Dim1"])
      item_prob[[i]]$item = item_par[i, "item"]
    }
  } else {
    for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta, 
                                b = item_par[i, "AXsi_.Cat1"], 
                                a = item_par[i, "B.Cat1.Dim1"], 
                                c = item_par[i, "guess"])
      item_prob[[i]]$item = item_par[i, "item"]
    }
  }
  p = do.call("rbind", item_prob)
  gp = ggplot(p, 
              aes(x = theta, y = it_p, group = item, col =
                    item)) + geom_line(lwd = 1)
  object = list(prob.data = p, 
                icc.graph = gp)
  return(object)
}
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
data = read.csv("D:/PRIN2020/qualtrics/psycAssist/mat49_2023_03_26.csv", 
                header = T, sep = ",")
data = data[!data$external_code %in% "UTENTE01", ]
data = data[!data$date_end %in% "NULL", ]

table(data$external_code)

guide = read.csv("D:/PRIN2020/qualtrics/skill_young.csv",
                 header = T, sep = ",")
guide$id_question = 1:nrow(guide)
data$id_question = data$id_question + 1
problems = data[data$id_question %in% c(3, 7), ]
data = data[!data$id_question %in% c(3), ]
data = data[!data$anni_scolarita %in% "NULL", ]
temp = data[, c("id", "id_question", "task_success")]

data = merge(data, guide, by = "id_question")

# faccio subito la ricodifica dei distrattori -----

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
temp$task_success = ifelse(temp$task_success == "True", 
                           1, 0)


wide.temp = reshape(temp, 
                    idvar = "id", 
                    timevar = "id_question", 
                    direction = "wide")
wide.temp$total_score = rowSums(wide.temp[,-1], na.rm = T)
wide.temp = wide.temp[order(wide.temp$total_score), ]
wide.temp$new.id = 1:nrow(wide.temp)

small_sbj = data[, c("id", "anni_scolarita", 
                     "gender", "diagnostic")]
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


wide.temp$new.scol = ifelse(wide.temp$anni_scolarita == 0, 
                            "yy", "y")

data$new.scol = ifelse(data$anni_scolarita == 0, 
                            "yy", "y")
table(wide.temp$anni_scolarita)
# tempi di risposta degli item 
couple_data = data[data$id_question %in% c(6:10,12:17, 19), ]
couple_data$id_question = factor(couple_data$id_question,
                                 levels = c("6","13","7","14","8","15",
                                            "9","16","10","17","12","19"))

couple_data$couple = character(nrow(couple_data))

for(i in 1:nrow(couple_data)) {
  if (couple_data[i, "id_question"] == 6 | couple_data[i, "id_question"] == 13) {
    couple_data[i, "couple"] = "6.13"
  } else if (couple_data[i, "id_question"] == 7 | couple_data[i, "id_question"] == 14) {
    couple_data[i, "couple"] = "7.14"
  } else if (couple_data[i, "id_question"] == 8 | couple_data[i, "id_question"] == 15) {
    couple_data[i, "couple"] = "8.15"
  } else if (couple_data[i, "id_question"] == 9 | couple_data[i, "id_question"] == 16) {
    couple_data[i, "couple"] = "9.16"
  } else if (couple_data[i, "id_question"] == 10 | couple_data[i, "id_question"] == 17) {
    couple_data[i, "couple"] = "10.17"
  } else if (couple_data[i, "id_question"] == 12 | couple_data[i, "id_question"] == 19) {
    couple_data[i, "couple"] = "12.19"
  }
}

couple_data$couple = factor(couple_data$couple, 
                            levels = c("6.13","7.14", "8.15", 
                                       "9.16", "10.17", "12.19"))
couple_data$new.scol = factor(couple_data$new.scol, 
                              levels = c("yy", "y"))
ggplot(couple_data, 
       aes(x = new.scol, y = time.diff.item, color = colore)) + geom_boxplot() + 
  facet_wrap(~couple) + ylim(0, 50)

# faccio il grafcio con le medie colorate vs non colarete e anni scol -----

mean_time_couple = couple_data %>%  
  group_by(new.scol, colore, couple) %>% 
  summarise(mean_time = mean(time.diff.item), sd_time = sd(time.diff.item))

ggplot(mean_time_couple, 
       aes(x = new.scol, y = mean_time, color = colore)) + geom_point(size = 4) + 
  geom_errorbar(aes(x=new.scol, 
                    ymin = mean_time-sd_time, 
                    ymax = mean_time+sd_time), width = .2) + 
  facet_wrap(~couple) + theme_light()

# grafico solo per vedere se c'è diffeenza nei tmepi delle colorate e delle nn colorate -----
mean_time_color = couple_data %>%  
  group_by(colore, couple) %>% 
  summarise(mean_time = mean(time.diff.item), sd_time = sd(time.diff.item))



# faccio il grafcio con le medie ACCURATEZZE colorate vs non colarete e anni scol -----
couple_data$correct = ifelse(couple_data$task_success == "True", 1, 0)
mean_acc_couple = couple_data %>%  
  group_by(new.scol, colore, couple) %>% 
  summarise(prop = mean(correct))

ggplot(mean_acc_couple, 
       aes(x=new.scol, y = prop, fill = colore)) +
  geom_bar(stat = "identity", position = position_dodge()) + facet_wrap(~couple)

ggplot(mean_time_couple, 
       aes(x = new.scol, y = mean_time, color = colore)) + geom_point(size = 4) + 
  geom_errorbar(aes(x=new.scol, 
                    ymin = mean_time-sd_time, 
                    ymax = mean_time+sd_time)) + facet_wrap(~couple)

# grafico solo per vedere se c'è diffeenza nei tmepi delle colorate e delle nn colorate -----
mean_time_color = couple_data %>%  
  group_by(colore, couple) %>% 
  summarise(mean_time = mean(time.diff.item), sd_time = sd(time.diff.item))


ggplot(mean_time_color, 
       aes(x = colore, y = mean_time, color = colore)) + geom_point(size = 4) + 
  geom_errorbar(aes(x=colore, 
                    ymin = mean_time-sd_time, 
                    ymax = mean_time+sd_time)) + facet_wrap(~couple)


# provo a fare la dif anlaysis ---- 
colnames(wide.temp) = gsub("task_success.", "items.", colnames(wide.temp))
item = wide.temp[, c("id","new.scol", "items.6", "items.13","items.7", "items.14", 
                     "items.8", "items.15", "items.9", "items.16", "items.10", "items.17", 
                     "items.12", "items.19")]

m.1pl = tam.mml(item[, -c(1,2)], pid = item$id)
fit.m1pl = tam.modelfit(m.1pl)
fit.m1pl$chi2.stat
fit.m1pl$statlist
fit.m1pl$modelfit.test



# non fitta molto bene 
dip.local(fit.m1pl)

x = item[,-c(1:2)]
# ci sono deu coppie con dipendenza lcoale. per ora le tengo 
data.ref <- item[item$new.scol %in% "yy", -c(1,2)]
data.focal <- item[!item$new.scol %in% "yy", -c(1,2)]
# Pre-estimation of the item parameters (1PL model)
mR <- itemParEst(data.ref, model = "1PL")
mF <- itemParEst(data.focal, model = "1PL")
mF <- itemRescale(mR,mF)
dif_parameters = rbind(mR, mF)


dif.1 = difLord(irtParam = dif_parameters,model = "1PL", engine = "ltm",
                same.scale = FALSE)

dif.1 = difLord(irtParam = x,model = "1PL", engine = "ltm",
                same.scale = FALSE)

# raju 

nF <- sum(Gender)
nR <- nrow(verbal)-nF
data.ref <- verbal[,1:24][order(Gender),][1:nR,]
data.focal <- verbal[,1:24][order(Gender),][(nR+1):(nR+nF),]
# Pre-estimation of the item parameters (1PL model)
mR <- itemParEst(data.ref,model = "1PL")
mF <- itemParEst(data.focal,model = "1PL")
mF <- itemRescale(mR, mF)
# Signed and unsigned Raju statistics
RajuZ(mR, mF)
RajuZ(mR, mF, signed = TRUE)
dif_parameters.r = rbind(mR, mF)
dif.Ra = difRaju(irtParam = dif_parameters.r,
                 model = "1PL", engine = "ltm",
                 same.scale = FALSE)
dif.Ra
par_yy = data.frame(mR)
par_yy$group = "yy"
par_yy$id_question = rownames(par_yy)
par_y = data.frame(mF)
par_y$group = "y"
colnames(par_y) = gsub("new.", "", colnames(par_y))
par_y$id_question = rownames(par_yy)

par_dif = rbind(par_yy, par_y) 

id_couples = couple_data[, c("id_question", "couple")]
id_couples = id_couples %>% 
  distinct()
id_couples$id_question = paste0("items.", id_couples$id_question)


par_dif = merge(par_dif, id_couples)
small_guide  = guide[, c("id_question", "colore")]
small_guide$id_question =  paste0("items.", small_guide$id_question)

par_dif = merge(par_dif, small_guide)


temp = cbind(par_yy, par_y)
temp$difference = temp[,1] - temp[,5]
temp = temp[,c("id_question", "difference")]

par_dif =merge(par_dif, temp)

par_dif$couple = factor(par_dif$couple, 
                        levels = levels(couple_data$couple))
ggplot(par_dif, 
       aes(x =b, y = reorder(couple, difference), 
           color = group)) + geom_point(size = 3) + facet_wrap(~colore) + 
  geom_errorbar(aes(y = couple, 
                     xmin = b - par_dif$se.b, 
                     xmax = b + par_dif$se.b),width=0.2, size=1) + 
  theme_light()
 
data.frame(mR, mF)
summary(dif.1)


# analisi (descrittiva) sui distrattori 
# devo calcoalre la proporzione di scelta di ogni distrattore 

# proporzione di distrattori scelti ATTRAVERSO LE SCOLARITà ----
freq_dist = couple_data %>% 
  group_by(distractors, id_question) %>% 
  summarise(n = n())


freq_item_couple = couple_data %>%  group_by(id_question) %>% 
  summarise(n_item = n()) 

freq_dist = merge(freq_dist, freq_item_couple)

freq_dist$prop = freq_dist$n/freq_dist$n_item

id_couples_temp = id_couples
id_couples_temp$id_question = gsub("items.", "", id_couples_temp$id_question)


freq_dist = merge(freq_dist, id_couples_temp)
# GRAFICO FREQUENZE DISTRATTORI -----
# devo metterli in ordine di coppia 


freq_dist$id_question = factor(freq_dist$id_question,
       levels = c("6","13","7","14","8","15",
                  "9","16","10","17","12","19"))

freq_dist$distractors = factor(freq_dist$distractors, 
                               levels = c("d.union", 
                                         "wp.matrix", 
                                          "r.top", "r.left", 
                                          "ic.flip", "ic.neg", "correct"))
ggplot(freq_dist, 
       aes(x = id_question, y = prop, fill = distractors)) + 
  geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")

# stessa cosa ma ora considero anche i livelli di scolarità-----

freq_dist_scol = couple_data %>% 
  group_by(distractors, id_question, new.scol) %>% 
  summarise(n = n())


freq_item_couple_scol = couple_data %>%  
  group_by(id_question, new.scol) %>% 
  summarise(n_item = n()) 

freq_dist_scol = merge(freq_dist_scol, freq_item_couple_scol)

freq_dist_scol$prop = freq_dist_scol$n/freq_dist_scol$n_item


freq_dist_scol = merge(freq_dist_scol, id_couples_temp)

freq_dist_scol$new.id = paste(freq_dist_scol$id_question, 
                               freq_dist_scol$new.scol, 
                               sep = ".")
freq_dist_scol$new.id = factor(freq_dist_scol$new.id, 
                               levels = c("6.yy", "6.y", 
                                          "13.yy", "13.y", 
                                          "7.yy", "7.y", 
                                          "14.yy", "14.y", 
                                          "8.yy", "8.y",
                                          "15.yy", "15.y", 
                                          "9.yy", "9.y", 
                                          "16.yy", "16.y", 
                                          "10.yy", "10.y", 
                                          "17.yy", "17.y", 
                                          "12.yy", "12.y", 
                                          "19.yy", "19.y"))

ggplot(freq_dist_scol[freq_dist_scol$couple %in% c("6.13", "7.14", "8.15"), ], 
       aes(x = new.id, y = prop, fill = distractors)) + 
  geom_bar(stat = "identity", position = position_dodge() )  + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")

ggplot(freq_dist_scol[!freq_dist_scol$couple %in% c("6.13", "7.14", "8.15"), ], 
       aes(x = new.id, y = prop, fill = distractors)) + 
  geom_bar(stat = "identity", position = position_dodge() ) + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")

# stessi grafici ma considerando la macro categoria di distrattori ----

# grafici tempi di risposta-accuratezza
# fare grafico tempi/accuratezze per gruppi di età e coppie di matrici

freq_dist_macro = couple_data %>% 
  group_by(macro_distractors, id_question) %>% 
  summarise(n = n())


freq_dist_macro = merge(freq_dist_macro, freq_item_couple)

freq_dist_macro$prop = freq_dist_macro$n/freq_dist_macro$n_item


freq_dist_macro = merge(freq_dist_macro, id_couples_temp)
# GRAFICO FREQUENZE DISTRATTORI -----
# devo metterli in ordine di coppia 


freq_dist_macro$id_question = factor(freq_dist_macro$id_question,
                               levels = c("6","13","7","14","8","15",
                                          "9","16","10","17","12","19"))

freq_dist_macro$distractors = factor(freq_dist_macro$macro_distractors, 
                               levels = c("correct", 
                                          "WP", 
                                          "R",  
                                          "IC", "D"))
ggplot(freq_dist_macro, 
       aes(x = id_question, y = prop, fill = macro_distractors)) + 
  geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")

# stessa cosa ma ora considero anche i livelli di scolarità-----

freq_dist_scol_macro = couple_data %>% 
  group_by(macro_distractors, id_question, new.scol) %>% 
  summarise(n = n())



freq_dist_scol_macro = merge(freq_dist_scol_macro, freq_item_couple_scol)

freq_dist_scol_macro$prop = freq_dist_scol_macro$n/freq_dist_scol_macro$n_item


freq_dist_scol_macro = merge(freq_dist_scol_macro, id_couples_temp)

freq_dist_scol_macro$new.id = paste(freq_dist_scol_macro$id_question, 
                                    freq_dist_scol_macro$new.scol, 
                              sep = ".")
freq_dist_scol_macro$new.id = factor(freq_dist_scol_macro$new.id, 
                               levels = c("6.yy", "6.y", 
                                          "13.yy", "13.y", 
                                          "7.yy", "7.y", 
                                          "14.yy", "14.y", 
                                          "8.yy", "8.y",
                                          "15.yy", "15.y", 
                                          "9.yy", "9.y", 
                                          "16.yy", "16.y", 
                                          "10.yy", "10.y", 
                                          "17.yy", "17.y", 
                                          "12.yy", "12.y", 
                                          "19.yy", "19.y"))

ggplot(freq_dist_scol_macro[freq_dist_scol_macro$couple %in% c("6.13", "7.14", "8.15"), ], 
       aes(x = new.id, y = prop, fill = macro_distractors)) + 
  geom_bar(stat = "identity", position = position_dodge() )  + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")


ggplot(freq_dist_scol_macro[!freq_dist_scol_macro$couple %in% c("6.13", "7.14", "8.15"), ], 
       aes(x = new.id, y = prop, fill = macro_distractors)) + 
  geom_bar(stat = "identity", position = position_dodge() )  + 
  geom_hline(aes(yintercept = .25), linetype = 2) + 
  geom_hline(aes(yintercept = .5), linetype = 2) + 
  geom_hline(aes(yintercept = .75), linetype = 2) + theme_light() + 
  scale_fill_brewer(palette = "Dark2")


# tempi di risposta e accuratezze item 2 x 2  ------

item_prob_scol = couple_data %>%  
  group_by(id_question, new.scol) %>% 
  summarise(prop = mean(correct))

item_time_scol = couple_data %>%  
  group_by(id_question, new.scol) %>% 
  summarise(mean_time = mean(time.diff.item), sd = sd(time.diff.item))



item_char = merge(item_prob_scol, item_time_scol)
item_char$id_question = factor(item_char$id_question, 
                               levels = c("6","13","7","14","8","15",
                                                   "9","16","10","17","12","19"))
item_char = merge(item_char, id_couples_temp)
item_char$new.id = ifelse(as.integer(as.character(item_char$id_question)) <= 12, 
                          "bold", 
                          "italic")


item_char = item_char[order(item_char$prop), ]
x.guide = item_char[, c("id_question", "new.id")]
x.guide = x.guide %>% distinct()

ggplot(item_char, 
       aes(x = reorder(id_question, prop), y=mean_time, 
           col = new.scol, shape = couple)) + geom_point(size = 3) + 
  theme(axis.text.x = element_text(face = x.guide$new.id)) + 
  geom_errorbar(aes(x = id_question, 
                    ymin = mean_time-sd, 
                    ymax = mean_time+sd), width =.2) + theme_light()




item.6.13 = item[, c("id", "items.6", "items.13")]
item.7.14 = item[, c("id", "items.7", "items.14")] # questi so già che hanno DP
item.8.15 = item[, c("id", "items.8", "items.15")]
item.9.16 = item[, c("id", "items.9", "items.16")]
item.10.17 = item[, c("id", "items.10", "items.17")]
item.12.19 = item[, c("id", "items.12", "items.19")]
