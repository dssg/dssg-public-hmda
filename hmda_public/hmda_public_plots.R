library(ggplot2)

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

#smoothed density plot of loan amounts by ethnicity; there are some huge outliers,
#so restrict to loans less than a million dollars just because that's where
#the bulk of the data is
ggplot(data=data_sub[data_sub$loanamount<1000,], aes(loanamount))+geom_density(aes(fill=iswhite, colour=iswhite), alpha=0.5)

#how many loans is each bank making? again, restrict to less than a thousand since
#that's where most of the data is, but the big banks are interesting too
ggplot(data=bank_counts[bank_counts$bank_counts<1000,], aes(bank_counts))+geom_bar()+scale_y_log10()

#smoothed density plot of the minority population and medianincome of the loan area, broken out
#by ethnicity; lots of loans to white people are in mostly white areas
ggplot(data=data_sub, aes(minoritypop))+geom_density(aes(fill=iswhite, colour=iswhite), alpha=0.5)
