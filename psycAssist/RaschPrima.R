library(lavaan)
library(lme4)
library(difR)
library(ggplot2)
library(dplyr)
library(TAM)
library(mokken)
# guarda i distrattori scelti in base 
# alla scolarita

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

# comincio mettendo il datatset in wide 
# (dopo scrivo lo script per imporatre i dati, ora mi prendo solo i dati)

dl = d1[, c("id", "matrice", "tipo", "colore", 
            "anni_scolarita", "gender", 
            "task_success", "distractors", "macro_distractors",
            "time.diff.item")]

# isolo le variabili demografiche che ora non mi serbono 

demo = dl[, c("id", "tipo", "colore", "matrice",
              "anni_scolarita", "gender", 
              "task_success", "distractors", "macro_distractors",
              "time.diff.item")]
demo = demo %>% 
  distinct()

d.temp = dl[, c("id", "task_success", "matrice")]

d.temp$task_success = ifelse(d.temp$task_success == "correct", 
                             1,
                             0)

wide = reshape(d.temp, 
               direction = "wide", 
               idvar = "id", 
               timevar="matrice")
colnames(wide) = gsub("task_success.", "", colnames(wide))

# partiamo con la cfa
# crea il modello 
names = paste(colnames(wide)[-1], collapse = " + ")

form.mod = paste("latent =~", names)

latent.model = cfa(form.mod, ordered = colnames(wide)[-1], 
                    data = wide)
summary(latent.model, fit.measures = T)
# the perfect fit!! 

# che palle ho gli NA, ci devo ragionare un attimo
check.monotonicity(wide[,-1], na.rm=T)


# applico rasch classico ---- 
m.1pl = tam.mml(wide[,-1], verbose = F)
summary(m.1pl)

# ha una fit discreta 
fit.1pl = tam.modelfit(m.1pl, progress = F)

item.fit.1pl = IRT.itemfit(m.1pl)
item.fit.1pl
item.fit.1pl$RMSD
fit.item.summary = item.fit.1pl$RMSD

for (i in 1:nrow(fit.item.summary)) {
  if (fit.item.summary[i, "Group1"] < .10) {
    fit.item.summary[i, "summary"] = "top"
  } else if (fit.item.summary[i, "Group1"] >= .10 & fit.item.summary[i, "Group1"] < .15) {
    fit.item.summary[i, "summary"] = "gnec"
  } else {
    fit.item.summary[i, "summary"] = "male"
  }
}
table(fit.item.summary$summary)

fit.item.summary[!fit.item.summary$summary %in% "top", ]

p = irt.icc(m.1pl)
p$icc.graph

# moelli misti per la dif sugli anni di scoalrità ------

dl$task_success = ifelse(dl$task_success == "correct", 
                         1, 0)

m0 = glmer(task_success ~ 0+ anni_scolarita + (1|id) +
             (1|matrice), 
           data = dl, 
           family = "binomial")


summary(m0)

# non mi fido molto, meglio aspettare più dati 
m1 = glmer(task_success ~ 0+ anni_scolarita + (1|id) +
             ( 0+ anni_scolarita|matrice), 
           data = dl, 
           family = "binomial", 
           glmerControl(optimizer = "bobyqa", 
                        calc.derivs = FALSE))


summary(m1)

anova(m1, m0)

m1.re = as.data.frame(ranef(m1, condVar = T))
re.item = m1.re[grep("young", m1.re$grp), ]

ggplot(re.item,
       aes(x = condval, y = grp, 
           color = grp)) + geom_point() + 
  facet_wrap(~term)



