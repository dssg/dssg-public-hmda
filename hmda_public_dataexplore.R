n = 100000
setwd("C:/Users/Nihar/Desktop/Public HMDA Data/2011HMDALAR - National")

headers = c("year", "bankid", "agencycode", "loantype", "propertytype",
	"loanpurpose", "occupancy", "loanamount", "preapproval", "action", "msa",
	"state", "county", "census", "appethnicity", "coappethnicity",
	"apprace1", "apprace2", "apprace3", "apprace4", "apprace5",
	"coapprace1", "coapprace2", "coapprace3", "coapprace4", "coapprace5",
	"appsex", "coappsex", "appincome", "purchasertype",
	"denial1", "denial2", "denial3", "ratespread",
	"hoepa", "lien", "edit", "sequence",
	"pop", "minoritypop", "medianincome", "tracttomsa", "owner",
	"family14", "appdate")

data = read.csv("2011HMDALAR - National.csv", header = FALSE, nrows = n, col.names = headers)

subset = as.data.frame(cbind(data$action, data$loanamount, data$apprace1, data$appincome))
colnames(subset) = c("action", "loanamount", "apprace1", "appincome")
index1 = union(which(subset$action == 1), which(subset$action == 3))
index2 = union(which(subset$apprace1 == 3), which(subset$apprace1 == 5))
index = intersect(index1, index2)
subset = subset[index,]

i = which(subset$apprace1 == 3)
subset$apprace1 = 0
subset$apprace1[i] = 1
i = which(subset$action == 1)
subset$action = 0
subset$action[i] = 1

logit <- glm(action ~ loanamount + apprace1 + appincome, data = subset, family = "binomial")
summary(logit)