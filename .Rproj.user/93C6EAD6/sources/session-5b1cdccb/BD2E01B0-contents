library(ggplot2)
library(patchwork)

data = read.csv("data/data_recode.csv", 
                header =T, 
                sep = ",")
head(data)
# facciamo pulizia 
data = data[data$data.accetto.non.accetto %in% 1, ]
data = data[!data$data.DistributionChannel %in% "preview", ]
data = data[!grepl("13/12", data$data.StartDate), ]

# prendo solo i dati completi 

d = data[data$data.Finished %in% 1, ]

# rinomino i distrattori
d.r = d[, -c(1:4)]

for (i in 1:nrow(d.r)) {
  for (j in 1:ncol(d.r)) {
    if (is.na(d.r[i,j]) == T) {
      d.r[i,j] = d.r[i,j]
    } else if (d.r[i,j] == "correct") {
      d.r[i,j] = d.r[i,j]
    } else if (d.r[i, j] == "wp.copy.ic.flip") {
      d.r[i, j] = "wp.copy.ic.flip"
    } else if (grepl("ic.", d.r[i,j]) ==T) {
      d.r[i,j] = "ic"
    } else if ( grepl("r.", d.r[i,j])) {
      d.r[i,j] = "r"
    } else if (grepl("wp.", d.r[i,j])) {
      d.r[i,j] = "wp"
    }
  }
}

# write.table(d.r,
#             "data/recode_small.csv", sep = ",", row.names = F)

# mi separo di nuovo i set 

a = d.r[, c(grep("a_", colnames(d.r)), 
            grep("a1_", colnames(d.r)))]

a.vis = a[, !grepl("logic", colnames(a))]
a.log = a[, grepl("logic", colnames(a))]

a.vis.long = stack(a.vis)
a.vis.long$ind = factor(a.vis.long$ind, 
                         levels = c("a_1", "a1_1", 
                                    "a_2", "a1_2", 
                                    "a_3", "a1_3")  )
g.a.vis = list()

for (i in 1:length(levels(a.vis.long$ind))){
  g.a.vis[[i]] <- ggplot(a.vis.long[a.vis.long$ind %in% levels(a.vis.long$ind)[i], ], 
                         aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(a.vis.long$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.a.vis, ncol = 2)
# provo a fare quel calcolo del chi quadro 
# ricodifco la risposta corretta come NA ma mi prendo solo gli item più 
# "problematici" qui mi prendo solo a1_3 

a1_3 = a.vis$a1_3
a1_3[a1_3 == "correct"] = NA

sqrt(chisq.test(table(a1_3))$statistic/sum(table(a1_3)))

# qui sarebbe interessante verderlo sull'item a1_3 

a.log.long = stack(a.log)
a.log.long$ind = factor(a.log.long$ind, 
                        levels = c("a_logic1", "a1_logic1", 
                                   "a_logic2", "a1_logic2", 
                                   "a_logic3", "a1_logic3")  )
g.a.log = list()

for (i in 1:length(levels(a.log.long$ind))){
  g.a.log[[i]] <- ggplot(a.log.long[a.log.long$ind %in% levels(a.log.long$ind)[i], ], 
                         aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(a.log.long$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.a.log, ncol = 2)

# qi è ineteressante su a_logic1 e a1_logi1, a_logic2 e a1_logic2
a.log.test = a.log[, c("a_logic1", "a1_logic1", "a1_logic2")]

temp = NULL
summary.log.a = list()

for (i in 1:ncol(a.log.test)) {
  temp = a.log.test[, i]
  temp[temp == "correct"] = NA
  summary.log.a[[i]]  = sqrt(chisq.test(table(temp))$statistic/sum(table(temp)))
  names(summary.log.a)[i] = colnames(a.log.test)[i]
}
sum.log.a = data.frame(stim = names(summary.log.a), 
                       val = unlist(summary.log.a))

# set b -----

b = d.r[, c(grep("b_", colnames(d.r)), 
            grep("b1_", colnames(d.r)))]

b.vis = b[, !grepl("logic", colnames(b))]
b.log = b[, grepl("logic", colnames(b))]

b.vis.long = stack(b.vis)
b.vis.long$ind = factor(b.vis.long$ind, 
                        levels = c("b_visuo1", "b1_visuo1", 
                                   "b_visuo2", "b1_visuo2", 
                                   "b_visuo3", "b1_visuo3")  )
g.b.vis = list()

for (i in 1:length(levels(b.vis.long$ind))){
  g.b.vis[[i]] <- ggplot(b.vis.long[b.vis.long$ind %in% levels(b.vis.long$ind)[i], ], 
                         aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(b.vis.long$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.b.vis, ncol = 2)


# qui solo B1 

b.vis.test = b.vis[, "b_visuo1"]

b.vis.test[b.vis.test == "correct"] = NA

sqrt(chisq.test(table(b.vis.test))$statistic/sum(table(b.vis.test)))

# temp = NULL
# summary.vis.b = list()
# 
# for (i in 1:ncol(b.vis.test)) {
#   temp = b.vis.test[, i]
#   temp[temp == "correct"] = NA
#   summary.vis.b[[i]]  = sqrt(chisq.test(table(temp))$statistic/sum(table(temp)))
#   names(summary.vis.b)[i] = colnames(b.vis.test)[i]
# }
# sum.log.b = data.frame(stim = names(summary.vis.b), 
#                        val = unlist(summary.vis.b))

# b_visuo1 e b1_visuo1


b.log.long = stack(b.log)
b.log.long$ind = factor(b.log.long$ind, 
                        levels = c("b_logic1", "b1_logic1", 
                                   "b_logic2", "b1_logic2", 
                                   "b_logic3", "b1_logic3")  )
g.b.log = list()

for (i in 1:length(levels(b.log.long$ind))){
  g.b.log[[i]] <- ggplot(b.log.long[b.log.long$ind %in% levels(b.log.long$ind)[i], ], 
                         aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(b.log.long$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.b.log, ncol = 2)

# blogic e b1_logic1

b.log.test = b.log[, "b1_logic1"]

b.log.test[b.log.test == "correct"] = NA

sqrt(chisq.test(table(b.log.test))$statistic/sum(table(b.log.test)))

# confronti AB ----
ab.vis.0 = d.r[, c(grep("a_", colnames(d.r)), 
                   grep("b_visuo", colnames(d.r)))]
vis.0.l =stack(ab.vis.0) 
vis.0.l$ind = as.character(vis.0.l$ind)
vis.0.l = vis.0.l[!grepl("a_logic", vis.0.l$ind), ]

vis.0.l$ind = factor(vis.0.l$ind, 
                     levels = c("a_1", "b_visuo1", 
                                "a_2", "b_visuo2", 
                                "a_3", "b_visuo3"))

g.ab.vis = list()

for (i in 1:length(levels(vis.0.l$ind))){
  g.ab.vis[[i]] <- ggplot(vis.0.l[vis.0.l$ind %in% levels(vis.0.l$ind)[i], ], 
                          aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(vis.0.l$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab.vis, ncol = 2)


ab.log.0 = d.r[, c(grep("a_logic", colnames(d.r)), 
                   grep("b_logic", colnames(d.r)))]
log.0.l =stack(ab.log.0) 
log.0.l$ind = factor(log.0.l$ind, 
                     levels = c("a_logic1", "b_logic1", 
                                "a_logic2", "b_logic2", 
                                "a_logic3", "b_logic3"))

g.ab.log = list()

for (i in 1:length(levels(log.0.l$ind))){
  g.ab.log[[i]] <- ggplot(log.0.l[log.0.l$ind %in% levels(log.0.l$ind)[i], ], 
                          aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(log.0.l$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab.log, ncol = 2)

# Confronti A1-B1 -----
ab.vis.0 = d.r[, c(grep("a1_", colnames(d.r)), 
                   grep("b1_visuo", colnames(d.r)))]
vis.0.l =stack(ab.vis.0) 
vis.0.l$ind = as.character(vis.0.l$ind)
vis.0.l = vis.0.l[!grepl("a_logic", vis.0.l$ind), ]

vis.0.l$ind = factor(vis.0.l$ind, 
                     levels = c("a_1", "b_visuo1", 
                                "a_2", "b_visuo2", 
                                "a_3", "b_visuo3"))

g.ab.vis = list()

for (i in 1:length(levels(vis.0.l$ind))){
  g.ab.vis[[i]] <- ggplot(vis.0.l[vis.0.l$ind %in% levels(vis.0.l$ind)[i], ], 
                          aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(vis.0.l$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab.vis, ncol = 2)


ab.log.0 = d.r[, c(grep("a_logic", colnames(d.r)), 
                   grep("b_logic", colnames(d.r)))]
log.0.l =stack(ab.log.0) 
log.0.l$ind = factor(log.0.l$ind, 
                     levels = c("a_logic1", "b_logic1", 
                                "a_logic2", "b_logic2", 
                                "a_logic3", "b_logic3"))

g.ab.log = list()

for (i in 1:length(levels(log.0.l$ind))){
  g.ab.log[[i]] <- ggplot(log.0.l[log.0.l$ind %in% levels(log.0.l$ind)[i], ], 
                          aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(log.0.l$ind)[i],
         y = "Proporzione", 
         x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab.log, ncol = 2)

# Confronto A1-B1 -----

ab1.vis.1 = d.r[, c(grep("a1_", colnames(d.r)), 
                    grep("b1_visuo", colnames(d.r)))]
vis.1.l =stack(ab1.vis.1) 
vis.1.l$ind = as.character(vis.1.l$ind)
vis.1.l = vis.1.l[!grepl("a1_logic", vis.1.l$ind), ]

vis.1.l$ind = factor(vis.1.l$ind, 
                     levels = c("a1_1", "b1_visuo1", 
                                "a1_2", "b1_visuo2", 
                                "a1_3", "b1_visuo3"))

g.ab1.vis = list()

for (i in 1:length(levels(vis.1.l$ind))){
  g.ab1.vis[[i]] <- ggplot(vis.1.l[vis.1.l$ind %in% levels(vis.1.l$ind)[i], ], 
                           aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), lab1el = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(vis.1.l$ind)[i],
          y = "Proporzione", 
          x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab1.vis, ncol = 2)


ab1.log.1 = d.r[, c(grep("a1_logic", colnames(d.r)), 
                    grep("b1_logic", colnames(d.r)))]
log.1.l =stack(ab1.log.1) 
log.1.l$ind = factor(log.1.l$ind, 
                     levels = c("a1_logic1", "b1_logic1", 
                                "a1_logic2", "b1_logic2", 
                                "a1_logic3", "b1_logic3"))

g.ab1.log = list()

for (i in 1:length(levels(log.1.l$ind))){
  g.ab1.log[[i]] <- ggplot(log.1.l[log.1.l$ind %in% levels(log.1.l$ind)[i], ], 
                           aes(values)) +
    geom_bar(aes(y = (..count..)/sum(..count..)))  +
    scale_x_discrete(drop = FALSE) +
    # geom_text(aes(y = ((..count..)/sum(..count..)), lab1el = scales::percent(accuracy = .01, (..count..)/sum(..count..))), stat = "count", vjust = -0.25, size = 4) +
    scale_y_continuous(breaks=c(0,.25,.50,.75,1.00),
                       limits = c(0,1)) +
    labs(title = levels(log.1.l$ind)[i],
          y = "Proporzione", 
          x = "Risposte")+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text.y = element_text(size=12, color="black"),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, color="black"),
          axis.title.x = element_text(size=12),
          title = element_text(size=12))+
    geom_hline(yintercept=.25, linetype="dashed")+
    geom_hline(yintercept=.50, linetype="dashed")+
    geom_hline(yintercept=.75, linetype="dashed")
}

wrap_plots(g.ab1.log, ncol = 2)


# Non vale la pena fare i modelli è tutto sballato. 
# vai a studiare il modello di Bock e scegli gli item da accoppiare per fare 
# l'analisi
# set A -----
# qui sarebbe interessante verderlo sull'item a1_3 
# qi è ineteressante su a_logic1 e a1_logi1, a_logic2 e a1_logic2
library(mirt)

# dveo rifare la codifica 
# corret = 0
# r = 1
# wp = 2
# d.union = 3
# ic = 4

# mi cambio già prima la wp copy.ic flip e la consdiero come IC flip (quindi ic)

a.vis.bock = a.vis[, c("a_3", "a1_3", "a_2", "a1_2")]
a.vis.bock$a1_3 = with(a.vis.bock, 
                       ifelse(a1_3 == "wp.copy.ic.flip", 
                              "ic", a1_3))

for (i in 1:ncol(a.vis.bock)) {
  a.vis.bock[,i] = ifelse(a.vis.bock[,i] == "correct", 
                          0, 
                          ifelse(a.vis.bock[,i] == "r", 
                                 1, 
                                 ifelse(a.vis.bock[,i] == "d.union", 
                                        3, 4)))
}

a.bock1 = mirt(a.vis.bock, 
               1, 
               "nominal")

coef(a.bock1, IRTpars = F, simplify = TRUE)


# kappa di cohen ------
library(psych)

a.vis.1 = a.vis[, c("a_1", "a1_1", 
                  "a_2", "a1_2", 
                  "a_3", "a1_3")]
# CHE DUE COGLIONI DC NON FACCIO ALTRO CHE CODIFiCARE STI CAZZO DI COSI MA BASTA 

for (i in 1:ncol(a.vis.1)) {
  a.vis.1[, i] = ifelse(a.vis.1[,i] == "correct", 
                        1, 0)
}
cont.vis.a = list()
kappa.vis.a = list()
wrap_plots(g.a.vis, ncol = 2)


for (i in seq(1, ncol(a.vis.1), by = 2)) {
  cont.vis.a[[i]] = table(a.vis.1[,i], 
                          a.vis.1[,i+1])
  names(cont.vis.a)[i] = paste0("Visuo", i)
  kappa.vis.a[[i]] = cohen.kappa(cbind(a.vis.1[,i], a.vis.1[,i+1]))
  names(kappa.vis.a)[i] = paste0("Visuo", i)
}
cohen.kappa(cbind(a.vis.1[,5], a.vis.1[,6]))

# a logic ----
a.log.1 = a.log[, c("a_logic1", "a1_logic1", 
                    "a_logic2", "a1_logic2", 
                    "a_logic3", "a1_logic3", 
                    "a_logic4", "a1_logic4")]


for (i in 1:ncol(a.log.1)) {
  a.log.1[, i] = ifelse(a.log.1[,i] == "correct", 
                        1, 0)
}
cont.log.a = list()
kappa.log.a = list()
wrap_plots(g.a.log, ncol = 2)


for (i in seq(1, ncol(a.log.1), by = 2)) {
  cont.log.a[[i]] = table(a.log.1[,i], 
                          a.log.1[,i+1])
  names(cont.log.a)[i] = paste0("logic", i)
  kappa.log.a[[i]] = cohen.kappa(cbind(a.log.1[,i], a.log.1[,i+1]))
  names(kappa.log.a)[i] = paste0("logic", i)
}

b.vis.1 = b.vis[, c("b_visuo1", "b1_visuo1", 
                    "b_visuo2", "b1_visuo2", 
                    "b_visuo3", "b1_visuo3")]


for (i in 1:ncol(b.vis.1)) {
  b.vis.1[, i] = ifelse(b.vis.1[,i] == "correct", 
                        1, 0)
}
cont.vis.b = list()
kappa.vis.b = list()
wrap_plots(g.b.vis, ncol = 2)


for (i in seq(1, ncol(b.vis.1), by = 2)) {
  cont.vis.b[[i]] = table(b.vis.1[,i], 
                          b.vis.1[,i+1])
  names(cont.vis.b)[i] = colnames(b.vis.1)[i]
  kappa.vis.b[[i]] = cohen.kappa(cbind(b.vis.1[,i], b.vis.1[,i+1]))
  names(kappa.vis.b)[i] = colnames(b.vis.1)[i]
}


# a logic ----
b.log.1 = b.log[, c("b_logic1", "b1_logic1", 
                    "b_logic2", "b1_logic2", 
                    "b_logic3", "b1_logic3", 
                    "b_logic4", "b1_logic4")]


for (i in 1:ncol(b.log.1)) {
  b.log.1[, i] = ifelse(b.log.1[,i] == "correct", 
                        1, 0)
}
cont.log.b = list()
kappa.log.b = list()
wrap_plots(g.b.log, ncol = 2)


for (i in seq(1, ncol(b.log.1), by = 2)) {
  cont.log.b[[i]] = table(b.log.1[,i], 
                          b.log.1[,i+1])
  names(cont.log.b)[i] = colnames(b.log.1)[i]
  kappa.log.b[[i]] = cohen.kappa(cbind(b.log.1[,i], b.log.1[,i+1]))
  names(kappa.log.b)[i] = colnames(b.log.1)[i]
}


kappa.log.b = kappa.log.b[grep("logic", names(kappa.log.b))]
kappa.log.b$b_logic1$kappa
