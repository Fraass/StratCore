
#this does not yet work as a function, and should be fairly flexible when made into one


#Running astrochron on data with pmag agemodel applied
##Setup for that portion
c(
  1,
  2,
  # 3,
  4,
  #5,
  6,
  #  7,
  8,
  #9,
  10,
  # 11,
  # 12,
  13,
  # 14,
  #15,
  #16,
  17
)->pmag.grab #which pmag to use
window<-130
interp<-3
start<-0
end<-1000
padding<-300
steps<-interp*2.5
bwith<-9
#First, removes volcanics from the col.reflectance data

######Translating depth to depth.nv#####
test<-col.u1396.st[,c(27,11)]

######Translating depth to depth.nv
#Fraass (v0.1 Dec'14)
#object to work on (depth column 1, values column 2)
d.obj<-test

#core description object
desc<-splice.desc


#Sediment types to skip
sed.skip<-c(
  "ash [F&S84]",
  "volcaniclastic-breccia [BGS-S81]",
  "volcaniclastic-gravel [BGS-S81]",
  "volcaniclastic-mud [BGS-S81]",
  "volcaniclastic-mudstone [BGS-S81]",
  "volcaniclastic-sand [BGS-S81]"
)

######## Begin Code

#Should first compile depths of sed, then should take depth, subtract sed.skip thickness
#should finish with three columns, (depth,value,depth.nv)
#Gather all seds listed in sed.skip into on object
a<-match(desc[,c("MAJ.Lith..Principal.name")],sed.skip)
which(a > 0)->X
rm(a)
sed<-desc[X,
          c("CCSF.A.T","CCSF.A.B","MAJ.Lith..Principal.name")]
rm(X)

#creating new object
depth.nv<-matrix(nrow=length(d.obj[,1]),ncol=3)
depth.nv[,1]<-d.obj[,1]
depth.nv[,3]<-d.obj[,2]
colnames(d.obj)[1]->colnames(depth.nv)[1]
colnames(d.obj)[2]->colnames(depth.nv)[3]
"depth.m.nv"->colnames(depth.nv)[2]
colnames(d.obj)[1]->colnames(depth.nv)[1]


#thickness calcs
sed[,4]<-sed[,2]-sed[,1]
colnames(sed)[4]<-"thick"

#transfering over sed$depths to sed$depth.nv
depth.nv[,1]->depth.nv[,2]
#subtracting sed$thick from depth to get depth.nv
min(which(sed$CCSF.A.B[1] < depth.nv[,1]))->X
for(i in X:length(depth.nv[,1])){
  depth.nv[i,1]-sum(sed$thick[which(
    sed$CCSF.A.B < depth.nv[i,1])])->depth.nv[i,2]}


# changing working objects values that are within the skipped seds to NA
for(i in 1:length(sed[,1])){
  depth.nv[which(depth.nv[,1] >= sed[i,"CCSF.A.T"]
                 & depth.nv[,1] <= sed[i,"CCSF.A.B"]),2]<-NA
}

depth.nv.t<-depth.nv[
  which(is.na(depth.nv[,2])==F)
  ,]
######## End Code




######## End Code####

## Sedrates w/out volcs pmag#####
#generating the list of instantaneous depositional events
#levels(splice.desc$MAJ.Lith..Principal.name)
#[1] ""                                 "ash [F&S84]"
#[3] "calcareous sand [Leg210]"         "carbonate ooze [Leg178]"
#[5] "volcaniclastic-breccia [BGS-S81]" "volcaniclastic-gravel [BGS-S81]"
#[7] "volcaniclastic-mud [BGS-S81]"     "volcaniclastic-sand [BGS-S81]"

#CCSF.A.T CCSF.A.B
#Fraass (v0.1 Dec'14)
#object to work on (depth column 1, values column 2)
pmag.nv<-pmag.splice[pmag.grab,"CCSF.A"]
pmag.nv<-cbind(pmag.nv,pmag.splice[pmag.grab,"Age.Ogg12"])
colnames(pmag.nv)[2]<-"Age.Ogg12"
colnames(pmag.nv)[1]<-'CCSF.A'
d.obj<-pmag.nv

#core description object
desc<-splice.desc

#saving depth.nv for later
depth.nv.t->temp
#Sediment types to skip


######## Begin Code

#Should first compile depths of sed, then should take depth, subtract sed.skip thickness
#should finish with three columns, (depth,value,depth.nv)
#Gather all seds listed in sed.skip into on object
a<-match(desc[,c("MAJ.Lith..Principal.name")],sed.skip)
which(a > 0)->X
rm(a)
sed<-desc[X,
          c("CCSF.A.T","CCSF.A.B","MAJ.Lith..Principal.name")]
rm(X)

#creating new object
depth.nv<-matrix(nrow=length(d.obj[,1]),ncol=3)
depth.nv[,1]<-d.obj[,1]
depth.nv[,3]<-d.obj[,2]
colnames(d.obj)[1]->colnames(depth.nv)[1]
colnames(d.obj)[2]->colnames(depth.nv)[3]
"depth.m.nv"->colnames(depth.nv)[2]
colnames(d.obj)[1]->colnames(depth.nv)[1]


#thickness calcs
sed[,4]<-sed[,2]-sed[,1]
colnames(sed)[4]<-"thick"

#transfering over sed$depths to sed$depth.nv
depth.nv[,1]->depth.nv[,2]
#subtracting sed$thick from depth to get depth.nv
min(which(sed$CCSF.A.B[1] < depth.nv[,1]))->X
for(i in X:length(depth.nv[,1])){
  depth.nv[i,1]-sum(sed$thick[which(
    sed$CCSF.A.B < depth.nv[i,1])])->depth.nv[i,2]}


# changing working objects values that are within the skipped seds to NA
#for(i in 1:length(sed[,1])){
#  depth.nv[which(depth.nv[,1] >= sed[i,"CCSF.A.T"]
#                 & depth.nv[,1] <= sed[i,"CCSF.A.B"]),2]<-NA
#}
#forcing the reversal in a volcanic event to be added back in

#34.8000->depth.nv[5,'depth.m.nv']

#depth.nv.t<-depth.nv[
#  which(is.na(depth.nv[,2])==F)
#  ,]
######## End Code
pmag.nv<-depth.nv
depth.nv.t<-temp;rm(temp)
colnames(pmag.nv)[2]<-"CCSF.Anv"
rm(sed.rate.nv);sed.rate.nv<-0
#calculating sedrate
for(i in 1:length(pmag.nv[,"CCSF.Anv"]))
{
  sed.rate.nv[i-1]<-(
    pmag.nv[i,'CCSF.Anv']-pmag.nv[i-1,'CCSF.Anv']
  )/(
    pmag.nv[i,'Age.Ogg12']-pmag.nv[i-1,'Age.Ogg12'])
}

Age.nv<-NA
for(i in 1:length(depth.nv.t[,2])){
  #finding appropriate sedrate
  max(which(pmag.nv[,'CCSF.Anv'] < depth.nv.t[i,2]))->X
  #age calc
  #difference in depths
  depth.nv.t[i,2]-pmag.nv[X,'CCSF.Anv']->y
  y*{sed.rate.nv[X]^-1}->Z
  Z+pmag.nv[X,'Age.Ogg12']->Age.nv[i]
  #units sed.rate=m/myr
}
cbind(depth.nv.t,Age.nv)->depth.nv.t

#only working with SPLICE
depth.nv.t[,c(4,3)]->L.pm
#L.pm[which(L.pm[,2] < 5 & L.pm[,2] >1.5),]->L.pm
par(mfcol=c(1,1))
#plot(L.pm)
#L.pm[which(L.pm[,2] < 100),]->L.pm
#plot(L.pm,xlim=c(61,62))

#convert MYR to KYR
L.pm[,1]*1000->L.pm[,1]


iso(L.pm
    ,xmin=start
    ,xmax=end
    ,genplot=F
)->L.pm


linterp(L.pm
        ,dt=interp
        ,genplot=F
)->L.pm



eha(L.pm
    ,demean=T
    ,detrend=T
    ,win=window
    ,tbw=bwith
    ,pad=padding
    ,step=steps
    ,pl=2
    ,siglevel=.85
    ,output=4
    ,sigID=T
    ,genplot=2
    #,xlab="UNTUNED"
    ,fmax=.06
    ,ydir=-1
)->L.eha
abline(v=target,col='grey',lwd=2)
abline(v=1/100,col='green',lwd=2)
abline(h=pmag.splice[pmag.grab,"Age.Ogg12"]*1000,lwd=2)
text(0.003,pmag.splice[pmag.grab,"Age.Ogg12"]*1000,pmag.grab,
     col='white',
     cex=1)

#impact of sed change
abline(h=pmag.splice[pmag.grab,"Age.Ogg12"]*1000+window/2,lwd=2,col='white')
abline(h=pmag.splice[pmag.grab,"Age.Ogg12"]*1000-window/2,lwd=2,col='white')
abline(h=depth.nv.t[4068,'Age.nv']*1000)
abline(h=depth.nv.t[4068,'Age.nv']*1000+window/2,col='red',lwd=2)
abline(h=depth.nv.t[4068,'Age.nv']*1000-window/2,col='red',lwd=2)


