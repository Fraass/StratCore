#' A StratCore Function
#'
#' This function migrates data from a drilling depth scale to a composite depth scale
#'
#' @param
#'


depth.splicer<-function(obj,depth,splice,splice.table,tiepoints){
  CCSF.A<-0
  for(i in 1:length(obj[,"Hole"])){
    if(is.na(obj$Core[i]) == FALSE){
      which(splice.table$Hole == obj$Hole[i] &
              splice.table$Core == obj$Core[i])->a
      splice.table$CCSF.A.offset[a]+
        obj[i,depth]->CCSF.A[i]
    }

  }
  cbind(obj,CCSF.A)->c

  if(splice == 1){
    #RUN SPLICE

    d<-c[which(c[,"CCSF.A"] <= tiepoints[1,"CCSF.A1"] &
                 c$Hole == tiepoints[1,"Hole1"]),]
    o<-which(tiepoints$Append != "APPEND")
    for(i in 2:length(tiepoints$Hole1)){
      d1<-tiepoints[o[i],"CCSF.A2"]
      d2<-tiepoints[o[i-1],"CCSF.A1"]
      #    if(which(tiepoints$Append == "APPEND") == i){
      #     d2<-tiepoints[i,"CCSF.A2"]
      #   }
      which(c$Hole == tiepoints$Hole1[o[i]] &
              c[,"CCSF.A"] < d1 &
              c[,"CCSF.A"] > d2)->t
      d<-rbind(d,c[t,])
    }
    #adding append

    #end append
    o<-which(tiepoints$Append != "APPEND")
    max(o)->last
    d<-rbind(d,c[which(c[,"CCSF.A"] >= tiepoints[last,"CCSF.A2"] &
                         c$Hole == tiepoints[last,"Hole2"]),])
    d[order(d[,"CCSF.A"]),]->d
    c<-d
  }
  c
}
