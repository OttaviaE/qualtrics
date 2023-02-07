data = read.csv("data/raven2301NUM.csv", header = T, sep = ",")
code = read.csv("data/codeDist.csv", 
                header = T, sep = ",")

a = data[, c(grep("a_", colnames(data)), 
             grep("a1_", colnames(data)))]

acode = code[, c(
                 grep("a_", colnames(code)), 
                 grep("a1_", colnames(code)))]

acode = acode[, colnames(a)]

# adesso hanno le colonne nello stesso ordine 
for (i in 1:nrow(a)) {
  for(j in 1:ncol(a)) {
    if (is.na(a[i, j]) == T) {
      a[i, j] = a[i, j] 
    } else if ( a[i, j] == 1) {
      a[i, j] = "correct"
    } else if (a[i, j] == 2) {
      a[i, j] = acode[2, j]
    } else if (a[i, j] == 3) {
      a[i, j] = acode[3, j]
    } else if ( a[i, j] == 4) {
      a[i, j] = acode[4, j]
    } else if (a[i, j] == 5) {
      a[i, j] = acode[5, j]
    } else if (a[i, j] == 6) {
      a[i, j] = acode[6, j]
    } else if (a[i, j] == 7) {
      a[i, j] = acode[7, j] 
    } else if (a[i, j] == 8) {
      a[i, j] = acode[8, j]
    } else if (a[i, j] == 9) {
      a[i, j] = acode[9, j]
    } else if (a[i, j] == 10) {
      a[i, j] = acode[10, j]
    }
  }
}

# set B 

b = data[, c(grep("b_", colnames(data)), 
             grep("b1_", colnames(data)))]

bcode = code[, c(
  grep("b_", colnames(code)), 
  grep("b1_", colnames(code)))]

bcode = bcode[, colnames(b)]

# adesso hanno le colonne nello stesso ordine 
for (i in 1:nrow(b)) {
  for(j in 1:ncol(b)) {
    if (is.na(b[i, j]) == T) {
      b[i, j] = b[i, j] 
    } else if ( b[i, j] == 1) {
      b[i, j] = "correct"
    } else if (b[i, j] == 2) {
      b[i, j] = bcode[2, j]
    } else if (b[i, j] == 3) {
      b[i, j] = bcode[3, j]
    } else if ( b[i, j] == 4) {
      b[i, j] = bcode[4, j]
    } else if (b[i, j] == 5) {
      b[i, j] = bcode[5, j]
    } else if (b[i, j] == 6) {
      b[i, j] = bcode[6, j]
    } else if (b[i, j] == 7) {
      b[i, j] = bcode[7, j] 
    } else if (b[i, j] == 8) {
      b[i, j] = bcode[8, j]
    } else if (b[i, j] == 9) {
      b[i, j] = bcode[9, j]
    } else if (b[i, j] == 10) {
      b[i, j] = bcode[10, j]
    }
  }
}


data.r = cbind(a,b)
data.r$sbj = paste0(1, 1:nrow(data.r))
data.r = data.r[, c(ncol(data.r), 1:(ncol(data.r)-1))]

data.r = cbind(data$StartDate, 
               data$Finished,
               data$DistributionChannel, 
               data$accetto.non.accetto,
               data.r)

write.table(data.r, "data/data_recode.csv", 
            sep = ",", row.names = F)
