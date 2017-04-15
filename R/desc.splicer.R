#desc.splicer
  #this function puts the core description onto the spliced record
  #in essence, this builds a synthetic lithostratigraphic column


desc.splicer<-function(obj,tiepoints){

  d<-obj[which(obj[,"CCSF.A.T"] <= tiepoints[1,"CCSF.A1"] &
                 obj$Hole == tiepoints[1,"Hole1"]),]
  if(d[nrow(d),"CCSF.A.B"] > tiepoints[1,"CCSF.A1"]){
    d[nrow(d),"CCSF.A.B"]<-tiepoints[1,"CCSF.A1"]
  }
  o<-which(tiepoints$Append != "APPEND")
  for(i in 2:length(tiepoints$Hole1)){
    d1<-tiepoints[o[i],"CCSF.A2"]
    d2<-tiepoints[o[i-1],"CCSF.A1"]
    if(which(tiepoints$Append == "APPEND") == i){
      d2<-tiepoints[i,"CCSF.A2"]
    }
    which(obj$Hole == tiepoints$Hole1[o[i]] &
            obj[,"CCSF.A.B"] > d2 &
            obj[,"CCSF.A.B"] < d1 &
            obj[,"CCSF.A.T"] > d2 &
            obj[,"CCSF.A.T"] < d1)->t #desc entries within spliced sections
    if(length(t)>0){
      d<-rbind(d,obj[t,])
    }

    which(obj$Hole == tiepoints$Hole1[o[i]] &
            obj[,"CCSF.A.B"] > d2 &
            obj[,"CCSF.A.B"] > d1 &
            obj[,"CCSF.A.T"] > d2 &
            obj[,"CCSF.A.T"] < d1)->t #desc entries with tops inbetween spliced sections
    if(length(t)>0){
      obj[t,]->q
      q[,'CCSF.A.B']<-d1 #replacing base of unit w/ splice point
      d<-rbind(d,q)}

    which(obj$Hole == tiepoints$Hole1[o[i]] &
            obj[,"CCSF.A.B"] > d2 &
            obj[,"CCSF.A.B"] < d1 &
            obj[,"CCSF.A.T"] < d2 &
            obj[,"CCSF.A.T"] < d1)->t #desc entries with bases inbetween spliced sections
    if(length(t)>0){
      obj[t,]->q
      q[,'CCSF.A.T']<-d2 #replacing top of unit w/ splice point
      d<-rbind(d,q)}
  }


  ##Start append code

  ##End append code

  nrow(tiepoints)->last
  tiepoints[last,"CCSF.A2"]->d2
  which(obj$Hole == tiepoints$Hole1[last] &
          obj[,"CCSF.A.B"] > d2 &
          obj[,"CCSF.A.T"] < d2)->t
  if(length(t)>0){
    obj[t,]->q
    q[,'CCSF.A.B']<-d2 #replacing base of unit w/ splice point
    d<-rbind(d,q)
  }
  which(obj$Hole == tiepoints$Hole2[last] &
          obj[,"CCSF.A.B"] > d2 &
          obj[,"CCSF.A.T"] < d2)->t
  if(length(t)>0){
    obj[t,]->q
    q[,'CCSF.A.T']<-d2 #replacing base of unit w/ splice point
    d<-rbind(d,q)
  }
  d<-rbind(d,obj[which(obj[,"CCSF.A.T"] > d2
                       & obj$Hole == tiepoints[last,"Hole2"]),])
  d[order(d[,"CCSF.A.B"]),]->d
}
