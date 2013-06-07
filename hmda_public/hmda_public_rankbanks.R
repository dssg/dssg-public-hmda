#PARAMETERS
LINES_TO_READ = 20000 # lines to read
MIN_LOANS = 25 # minimum loans originated to evaluate a bank
MIN_LOANS_RACE = 5 # minimum loans originated to minorities and non-minorities to evaluate a bank
setwd("~/Downloads/hmda/")

headers = c("year", "repid", "agencycode", "loantype", "propertytype",
	"loanpurpose", "occupancy", "loanamount", "preapproval", "action", "msa",
	"state", "county", "census", "appethnicity", "coappethnicity",
	"apprace1", "apprace2", "apprace3", "apprace4", "apprace5",
	"coapprace1", "coapprace2", "coapprace3", "coapprace4", "coapprace5",
	"appsex", "coappsex", "appincome", "purchasertype",
	"denial1", "denial2", "denial3", "ratespread",
	"hoepa", "lien", "edit", "sequence",
	"pop", "minoritypop", "medianincome", "tracttomsa", "owner",
	"family14", "appdate")

classes = c('factor', 'factor', 'factor', 'factor','factor', 
             'factor','factor','numeric','factor','factor',
             'factor','factor','factor','factor','factor',
             'factor','factor','factor','factor','factor',
             'factor','factor','factor','factor','factor', 
            'factor', 'factor', 'factor', 'numeric', 'factor',
             'factor','factor','factor','numeric','factor',
             'factor','factor','factor','numeric','numeric',
             'numeric','numeric','numeric','numeric','factor')
## numeric variables are: loanamount, appincome, ratespread, and 
## the census tract aggregates

data = read.csv("2011HMDALAR - National.csv", header = FALSE, nrows = LINES_TO_READ, col.names = headers, colClasses=classes)
data$bankid = factor(data$repid:data$agencycode) ## fix the occasional nonunique repid

data_sub = subset(data, appethnicity!=3 & appethnicity !=4 & apprace1!=6 & apprace1!=7) ##ignore unreported races
data_sub = subset(data_sub, action!=6) ##ignore loans purchased from another institution

# define a person as not a minority if they are not hispanic and they are not any races other than white
data_sub$iswhite <- (data_sub$appethnicity==2 & data_sub$apprace1==5 & 
                       (data_sub$apprace2==5 | data_sub$apprace2==" ") & 
                       (data_sub$apprace3==5 | data_sub$apprace3==" ") &
                       (data_sub$apprace4==5 | data_sub$apprace4==" ") & 
                       (data_sub$apprace5==5 | data_sub$apprace5==" "))

# define a loan as approved if it is accepted or preapproved
data_sub$isapprove = (data_sub$action==1 | data_sub$action==2 | data_sub$action==8)

# count number of loans originated by a bank, exclude banks originating few loans for speed
bank_counts <- data.frame(table(data_sub$bankid))
colnames(bank_counts) <- c('bankid', 'bank_counts')
data_sub <- merge(data_sub, bank_counts, by.x="bankid", by.y="bankid")

bank_white_counts <- data.frame(table(data_sub$bankid[data_sub$iswhite]))
colnames(bank_white_counts) <- c('bankid', 'bank_white_counts')
data_sub <- merge(data_sub, bank_white_counts, by.x="bankid", by.y="bankid")

bank_nonwhite_counts <- data.frame(table(data_sub$bankid[!data_sub$iswhite]))
colnames(bank_nonwhite_counts) <- c('bankid', 'bank_nonwhite_counts')
data_sub <- merge(data_sub, bank_nonwhite_counts, by.x="bankid", by.y="bankid")

data_sub$bankid_drop <- data_sub$bankid
levels(data_sub$bankid_drop) <- c(levels(data_sub$bankid_drop),"small")
data_sub[(data_sub$bank_counts < MIN_LOANS) | (data_sub$bank_white_counts < MIN_LOANS_RACE) |
           (data_sub$bank_nonwhite_counts < MIN_LOANS_RACE),names(data_sub)=='bankid_drop'] <- "small"
data_sub$bankid_drop <- factor(data_sub$bankid_drop)
data_sub$bankid_drop <- relevel(data_sub$bankid_drop, "small") # this is the coefficient we want to leave implicit
# do logistic regression
lrfit <- glm(data_sub$isapprove ~ data_sub$iswhite + data_sub$iswhite * data_sub$bankid_drop + data_sub$bankid_drop 
             + data_sub$loantype + data_sub$propertytype + data_sub$loanpurpose + 
               data_sub$occupancy + data_sub$loanamount + data_sub$preapproval + 
               data_sub$appincome + data_sub$lien, family = binomial)

# order the bank*race coefficients by the p-value for being nonzero
coeffs <- data.frame(coef(summary(lrfit)))
discr_coeffs <- coeffs[-1:-(nrow(coeffs)-length(levels(data_sub$bankid_drop))+1),]
discr_coeffs[order(discr_coeffs$Pr...z..), ]