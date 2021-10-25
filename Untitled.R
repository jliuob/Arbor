aList = list()
aList[[length(aList) + 1]] = list(type = 'tree',
                                  data = read.tree('~/Desktop//Tree/tree.nwk'))
aList[[length(aList) + 1]] = list(type = 'heatmap',
                                  data = read.csv('~/Desktop//Tree/heatmap.csv'))
aList[[length(aList) + 1]] = list(type = 'barplot',
                                  data = read.csv('~/Desktop//Tree/bar.csv'))
aList[[length(aList) + 1]] = list(type = 'tree',
                                  data = read.tree('~/Desktop//Tree/tree.nwk'))

for (i in seq_along(aList)) {
  if (aList[[i]]$type=='tree') {
    aList[[i]]$order=1
  } else if (aList[[i]]$type=='heatmap') {
    aList[[i]]$order=2
  } else if (aList[[i]]$type=='barplot') {
    aList[[i]]$order=3
  }
}

if (length(aList)>0){
  ord<-order(unlist(lapply(aList, function(x){x$order})))
  aList<-aList[ord]
}

View(aList)
 
