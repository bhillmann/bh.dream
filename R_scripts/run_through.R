# outsie of library
# load in the data
data = read.csv("C:\\Volumes\\BioInformatics.pfo\\Dream\\data\\DREAM_data.txt", sep="\t", stringsAsFactors=F, row.names=1)
head(data)

# seperate the class information
labels = data[1:5,]
# seperate the gene expresion data
gene.exp = data[6:nrow(data),]

classifier = function(train, test, labels) {bh_knn(train, test, labels, k=5)}

results = array(NA, dim(labels))
for (i in 1:nrow(labels)) {
  cl = labels[i,]
  cl.keep = !is.na(cl)
  subset.gene = t(gene.exp[,cl.keep])
  cl.kept = cl[cl.keep]
  results[i,][cl.keep] = bh_loocv(classifier, subset.gene, cl.kept)
}

auc.results = 1:nrow(labels)
for (i in 1:nrow(labels)) {
  auc.results[i] = bh_plot_auc(results[i,][!is.na(labels[i,])], labels[i,][!is.na(labels[i,])])$auc
}

k.aucs = array(NA, c(nrow(labels),3))
for (i in 1:nrow(labels)) {
  cl = labels[i,]
  cl.keep = !is.na(cl)
  subset.gene = t(gene.exp[,cl.keep])
  cl.kept = cl[cl.keep]
  k.auc.results = array(NA, c(length(cl.kept), 3))
  ks = c(3,5,7)
  for (j in 1:length(ks)) {
    k.temp = ks[j]
    classifier.temp = function(train, test, labels) {
      bh_knn(train, test, labels, k=k.temp)
    }
    k.auc.results[,j] = bh_loocv(classifier.temp, subset.gene, cl.kept)$auc
  }
  k.aucs[i,] = bh_plot_auc(k.auc.results, replicate(3, cl.kept))
}

classifier.sign = function(train, test, labels) {
  bh_knn_sign(train, test, labels, k=5)
}

results.sign = array(NA, dim(labels))
for (i in 1:nrow(labels)) {
  cl = labels[i,]
  cl.keep = !is.na(cl)
  subset.gene = t(gene.exp[,cl.keep])
  cl.kept = cl[cl.keep]
  results.sign[i,][cl.keep] = bh_loocv(classifier.sign, subset.gene, cl.kept)
}

auc.results.sign = 1:nrow(labels)
for (i in 1:nrow(labels)) {
  auc.results.sign[i] = bh_plot_auc(results.sign[i,][!is.na(labels[i,])], labels[i,][!is.na(labels[i,])])$auc
}


