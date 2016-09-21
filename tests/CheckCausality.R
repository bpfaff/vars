

###test causality
library(vars)
data(Canada)

myVar<-VAR(Canada, p=2, season=12)

causality(myVar)
causality(myVar, cause="e")
causality(myVar, cause="prod")
causality(myVar, cause="rw")
causality(myVar, cause="U")

causality(myVar, cause=c("e", "prod"))
causality(myVar, cause=c("prod","e"))
causality(myVar, cause=c("e", "prod","rw"))

myVar2<-VAR(Canada, p=3, type="trend")
causality(myVar2, cause="e")
causality(myVar2, cause="prod")
causality(myVar2, cause="rw")
causality(myVar2, cause="U")

causality(myVar2, cause=c("e", "prod"))
causality(myVar, cause=c("prod","e"))
causality(myVar, cause=c("e", "prod","rw"))

 myVar3<-VAR(Canada[,1:3], p=1, exogen=Canada[,4], type="none")

causality(myVar3)
causality(myVar3, cause="e")
causality(myVar3, cause="prod")
causality(myVar3, cause="rw")

causality(myVar3, cause=c("e", "prod"))
causality(myVar3, cause=c("prod","e"))

