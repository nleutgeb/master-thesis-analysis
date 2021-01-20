library(readxl)
library(datetime)
library(xts)
library(data.table) # Data processing
library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
theme_set(theme_classic())
library(pdp) # Arranging plots
library(psych) # Descriptives
library(tseries) # ADF test
library(FinTS) # LM test
library(sarima) # White Noise Test
library(rugarch) # GARCH models

######## Data preparation

Companynames <- c("ADKO", "AGR", "AMAG", "ANDR", "ATS", "BG", "CAI", "DOC", "EBS", "EVN", "FACC", "FLU", "FQT", "IIA", "KTCG", "LNZ", "MARI", "MMK", "OMV", "PAL", "POS", "POST", "PYT", "RBI", "ROS", "SBO", "SEM", "SPI", "STR", "TKA", "UBS", "UQA", "VER", "VIG", "VOE", "WIE", "WXF", "ZAG")

readexcel_transformxts <- function(stock){
  path <- paste0("./Data/Prime Market/Prime Market 20200718/EXCEL/", stock, ".xlsx")
  stock <- read_excel(path)
  class(stock$Date)
  return(xts(stock[,-1], order.by=stock$Date))
}

ADKO <- readexcel_transformxts("ADKO")
AGR <- readexcel_transformxts("AGR")
AMAG <- readexcel_transformxts("AMAG")
ANDR <- readexcel_transformxts("ANDR")
ATS <- readexcel_transformxts("ATS")
BG <- readexcel_transformxts("BG")
CAI <- readexcel_transformxts("CAI")
DOC <- readexcel_transformxts("DOC")
EBS <- readexcel_transformxts("EBS")
EVN <- readexcel_transformxts("EVN")
FACC <- readexcel_transformxts("FACC")
FLU <- readexcel_transformxts("FLU")
FQT <- readexcel_transformxts("FQT")
IIA <- readexcel_transformxts("IIA")
KTCG <- readexcel_transformxts("KTCG")
LNZ <- readexcel_transformxts("LNZ")
MARI <- readexcel_transformxts("MARI")
MMK <- readexcel_transformxts("MMK")
OMV <- readexcel_transformxts("OMV")
PAL <- readexcel_transformxts("PAL")
POS <- readexcel_transformxts("POS")
POST <- readexcel_transformxts("POST")
PYT <- readexcel_transformxts("PYT")
RBI <- readexcel_transformxts("RBI")
ROS <- readexcel_transformxts("ROS")
SBO <- readexcel_transformxts("SBO")
SEM <- readexcel_transformxts("SEM")
SPI <- readexcel_transformxts("SPI")
STR <- readexcel_transformxts("STR")
TKA <- readexcel_transformxts("TKA")
UBS <- readexcel_transformxts("UBS")
UQA <- readexcel_transformxts("UQA")
VER <- readexcel_transformxts("VER")
VIG <- readexcel_transformxts("VIG")
VOE <- readexcel_transformxts("VOE")
WIE <- readexcel_transformxts("WIE")
WXF <- readexcel_transformxts("WXF")
ZAG <- readexcel_transformxts("ZAG")

Returns <- merge(ADKO$Return, AGR$Return, AMAG$Return, ANDR$Return, ATS$Return, BG$Return, CAI$Return, DOC$Return, EBS$Return, EVN$Return, FACC$Return, FLU$Return, FQT$Return, IIA$Return, KTCG$Return, LNZ$Return, MARI$Return, MMK$Return, OMV$Return, PAL$Return, POS$Return, POST$Return, PYT$Return, RBI$Return, ROS$Return, SBO$Return, SEM$Return, SPI$Return, STR$Return, TKA$Return, UBS$Return, UQA$Return, VER$Return, VIG$Return, VOE$Return, WIE$Return, WXF$Return, ZAG$Return)
Returns <- Returns["2010-01/2020-06"]
colnames(Returns)[1:38] <- Companynames

MarketCaps <- merge(ADKO$MarketCap, AGR$MarketCap, AMAG$MarketCap, ANDR$MarketCap, ATS$MarketCap, BG$MarketCap, CAI$MarketCap, DOC$MarketCap, EBS$MarketCap, EVN$MarketCap, FACC$MarketCap, FLU$MarketCap, FQT$MarketCap, IIA$MarketCap, KTCG$MarketCap, LNZ$MarketCap, MARI$MarketCap, MMK$MarketCap, OMV$MarketCap, PAL$MarketCap, POS$MarketCap, POST$MarketCap, PYT$MarketCap, RBI$MarketCap, ROS$MarketCap, SBO$MarketCap, SEM$MarketCap, SPI$MarketCap, STR$MarketCap, TKA$MarketCap, UBS$MarketCap, UQA$MarketCap, VER$MarketCap, VIG$MarketCap, VOE$MarketCap, WIE$MarketCap, WXF$MarketCap, ZAG$MarketCap)
MarketCaps <- MarketCaps["2017-01-02::2020-06-30"]
colnames(MarketCaps)[1:38] <- Companynames

Volumes <- merge(ADKO$Volume, AGR$Volume, AMAG$Volume, ANDR$Volume, ATS$Volume, BG$Volume, CAI$Volume, DOC$Volume, EBS$Volume, EVN$Volume, FACC$Volume, FLU$Volume, FQT$Volume, IIA$Volume, KTCG$Volume, LNZ$Volume, MARI$Volume, MMK$Volume, OMV$Volume, PAL$Volume, POS$Volume, POST$Volume, PYT$Volume, RBI$Volume, ROS$Volume, SBO$Volume, SEM$Volume, SPI$Volume, STR$Volume, TKA$Volume, UBS$Volume, UQA$Volume, VER$Volume, VIG$Volume, VOE$Volume, WIE$Volume, WXF$Volume, ZAG$Volume)
Volumes <- Volumes["2010-01/2020-06"]
colnames(Volumes)[1:38] <- Companynames

#### Returns

Returns_GRI <- subset(Returns, select=c(AGR, AMAG, BG, CAI, EBS, EVN, KTCG, LNZ, OMV, POST, PAL, POS, RBI, TKA, VER, WIE))
Returns_GRI$GRI_Mean <- NA
Returns_GRI$GRI_Mean <- rowMeans(Returns_GRI, na.rm=TRUE)
Returns_Means = Returns_GRI
Returns_Means <- Returns_Means[,colnames(Returns_Means) == "GRI_Mean"]
Returns_GRI <- Returns_GRI[, colnames(Returns_GRI) != "GRI_Mean"]

Returns_nonGRI <- subset(Returns, select=c(ADKO, ANDR, ATS, DOC, FACC, FLU, FQT, IIA, MARI, MMK, PYT, ROS, SPI, SBO, SEM, STR, UBS, UQA, VIG, VOE, WXF, ZAG))
Returns_nonGRI$nonGRI_Mean <- NA
Returns_nonGRI$nonGRI_Mean <- rowMeans(Returns_nonGRI, na.rm=TRUE)
Returns_Means$nonGRI_Mean <- Returns_nonGRI$nonGRI_Mean
Returns_nonGRI <- Returns_nonGRI[, colnames(Returns_nonGRI) != "nonGRI_Mean"]

Returns_TCFD <- subset(Returns, select=c(OMV, ROS, VER))
Returns_TCFD$TCFD_Mean <- NA
Returns_TCFD$TCFD_Mean <- rowMeans(Returns_TCFD, na.rm=TRUE)
Returns_Means$TCFD_Mean <- Returns_TCFD$TCFD_Mean
Returns_TCFD <- Returns_TCFD[, colnames(Returns_TCFD) != "TCFD_Mean"]

Returns_nonTCFD <- subset(Returns, select=-c(OMV, ROS, VER))
Returns_nonTCFD$nonTCFD_Mean <- NA
Returns_nonTCFD$nonTCFD_Mean <- rowMeans(Returns_nonTCFD, na.rm=TRUE)
Returns_Means$nonTCFD_Mean <- Returns_nonTCFD$nonTCFD_Mean
Returns_nonTCFD <- Returns_nonTCFD[, colnames(Returns_nonTCFD) != "nonTCFD_Mean"]

Returns_CDP <- subset(Returns, select=c(ATS, EVN, MMK, OMV, PAL, POS, POST, RBI, TKA, VER, VIG, VOE))
Returns_CDP$CDP_Mean <- NA
Returns_CDP$CDP_Mean <- rowMeans(Returns_CDP, na.rm=TRUE)
Returns_Means$CDP_Mean <- Returns_CDP$CDP_Mean
Returns_CDP <- Returns_CDP[, colnames(Returns_CDP) != "CDP_Mean"]

Returns_nonCDP <- subset(Returns, select=-c(ATS, EVN, MMK, OMV, PAL, POS, POST, RBI, TKA, VER, VIG, VOE))
Returns_nonCDP$nonCDP_Mean <- NA
Returns_nonCDP$nonCDP_Mean <- rowMeans(Returns_nonCDP, na.rm=TRUE)
Returns_Means$nonCDP_Mean <- Returns_nonCDP$nonCDP_Mean
Returns_nonCDP <- Returns_nonCDP[, colnames(Returns_nonCDP) != "nonCDP_Mean"]

Returns_GC <- subset(Returns, select=c(UQA, MMK, VER, POS, VOE, PAL, TKA, RBI, BG, POST, EVN, WIE, OMV))
Returns_GC$GC_Mean <- NA
Returns_GC$GC_Mean <- rowMeans(Returns_GC, na.rm=TRUE)
Returns_Means$GC_Mean <- Returns_GC$GC_Mean
Returns_GC <- Returns_GC[, colnames(Returns_GC) != "GC_Mean"]

Returns_nonGC <- subset(Returns, select=-c(UQA, MMK, VER, POS, VOE, PAL, TKA, RBI, BG, POST, EVN, WIE, OMV))
Returns_nonGC$nonGC_Mean <- NA
Returns_nonGC$nonGC_Mean <- rowMeans(Returns_nonGC, na.rm=TRUE)
Returns_Means$nonGC_Mean <- Returns_nonGC$nonGC_Mean
Returns_nonGC <- Returns_nonGC[, colnames(Returns_nonGC) != "nonGC_Mean"]

Returns_SBTI <- subset(Returns, select=c(POST, TKA, VER, RBI, LNZ))
Returns_SBTI$SBTI_Mean <- NA
Returns_SBTI$SBTI_Mean <- rowMeans(Returns_SBTI, na.rm=TRUE)
Returns_Means$SBTI_Mean <- Returns_SBTI$SBTI_Mean
Returns_SBTI <- Returns_SBTI[, colnames(Returns_SBTI) != "SBTI_Mean"]

Returns_nonSBTI <- subset(Returns, select=-c(POST, TKA, VER, RBI, LNZ))
Returns_nonSBTI$nonSBTI_Mean <- NA
Returns_nonSBTI$nonSBTI_Mean <- rowMeans(Returns_nonSBTI, na.rm=TRUE)
Returns_Means$nonSBTI_Mean <- Returns_nonSBTI$nonSBTI_Mean
Returns_nonSBTI <- Returns_nonSBTI[, colnames(Returns_nonSBTI) != "nonSBTI_Mean"]

Returns_AtLeastOneStandard <- subset(Returns, select=c(AGR, AMAG, BG, CAI, EBS, EVN, KTCG, LNZ, OMV, POST, PAL, POS, RBI, TKA, VER, WIE, ROS, VOE, ATS, UQA, MMK, VIG))
Returns_AtLeastOneStandard$AtLeastOneStandard_Mean <- NA
Returns_AtLeastOneStandard$AtLeastOneStandard_Mean <- rowMeans(Returns_AtLeastOneStandard, na.rm=TRUE)
Returns_Means$AtLeastOneStandard_Mean <- Returns_AtLeastOneStandard$AtLeastOneStandard_Mean
Returns_AtLeastOneStandard <- Returns_AtLeastOneStandard[, colnames(Returns_AtLeastOneStandard) != "AtLeastOneStandard_Mean"]

Returns_NoStandard <- subset(Returns, select=-c(AGR, AMAG, BG, CAI, EBS, EVN, KTCG, LNZ, OMV, POST, PAL, POS, RBI, TKA, VER, WIE, ROS, VOE, ATS, UQA, MMK, VIG))
Returns_NoStandard$NoStandard_Mean <- NA
Returns_NoStandard$NoStandard_Mean <- rowMeans(Returns_NoStandard, na.rm=TRUE)
Returns_Means$NoStandard_Mean <- Returns_NoStandard$NoStandard_Mean
Returns_NoStandard <- Returns_NoStandard[, colnames(Returns_NoStandard) != "NoStandard_Mean"]

Returns_AllFive <- subset(Returns, select=c(VER))
Returns_AllFive$AllFive_Mean <- NA
Returns_AllFive$AllFive_Mean <- rowMeans(Returns_AllFive, na.rm=TRUE)
Returns_Means$AllFive_Mean <- Returns_AllFive$AllFive_Mean
Returns_AllFive <- Returns_AllFive[, colnames(Returns_AllFive) != "AllFive_Mean"]

Returns_AllFiveExceptOne <- subset(Returns, select=c(OMV, POST, RBI, TKA))
Returns_AllFiveExceptOne$AllFiveExceptOne_Mean <- NA
Returns_AllFiveExceptOne$AllFiveExceptOne_Mean <- rowMeans(Returns_AllFiveExceptOne, na.rm=TRUE)
Returns_Means$AllFiveExceptOne_Mean <- Returns_AllFiveExceptOne$AllFiveExceptOne_Mean
Returns_AllFiveExceptOne <- Returns_AllFiveExceptOne[, colnames(Returns_AllFiveExceptOne) != "AllFiveExceptOne_Mean"]

Returns_JustOneStandard <- subset(Returns, select=c(AGR, AMAG, BG, CAI, EBS, KTCG, LNZ, POS, WIE, ROS, VOE, ATS, UQA, VIG))
Returns_JustOneStandard$JustOneStandard_Mean <- NA
Returns_JustOneStandard$JustOneStandard_Mean <- rowMeans(Returns_JustOneStandard, na.rm=TRUE)
Returns_Means$JustOneStandard_Mean <- Returns_JustOneStandard$JustOneStandard_Mean
Returns_JustOneStandard <- Returns_JustOneStandard[, colnames(Returns_JustOneStandard) != "JustOneStandard_Mean"]

Returns_ATX <- subset(Returns, select=c(EBS, OMV, VER, VOE, RBI, CAI, ANDR, WIE, BG, IIA, MMK, POST, TKA, UQA, SPI, VIG, LNZ, ATS, DOC, SBO))
Returns_ATX$ATX_Mean <- NA
Returns_ATX$ATX_Mean <- rowMeans(Returns_ATX, na.rm=TRUE)
Returns_Means$ATX_Mean <- Returns_ATX$ATX_Mean
Returns_ATX <- Returns_ATX[, colnames(Returns_ATX) != "ATX_Mean"]

Returns_nonATX <- subset(Returns, select=-c(EBS, OMV, VER, VOE, RBI, CAI, ANDR, WIE, BG, IIA, MMK, POST, TKA, UQA, SPI, VIG, LNZ, ATS, DOC, SBO))
Returns_nonATX$nonATX_Mean <- NA
Returns_nonATX$nonATX_Mean <- rowMeans(Returns_nonATX, na.rm=TRUE)
Returns_Means$nonATX_Mean <- Returns_nonATX$nonATX_Mean
Returns_nonATX <- Returns_nonATX[, colnames(Returns_nonATX) != "nonATX_Mean"]

Returns$All_Mean <- NA
Returns$All_Mean <- rowMeans(Returns, na.rm=TRUE)
Returns_Means$All_Mean <- Returns$All_Mean
Returns <- Returns[, colnames(Returns) != "All_Mean"]

#### Clean the Environment
rm(ADKO, AGR, AMAG, ANDR, ATS, BG, CAI, DOC, EBS, EVN, FACC, FLU, FQT, IIA, KTCG, LNZ, MARI, MMK, OMV, PAL, POS, POST, PYT, RBI, ROS, SBO, SEM, SPI, STR, TKA, UBS, UQA, VER, VIG, VOE, WIE, WXF, ZAG)

#### CAPM Data

## Market Rate and Risk Free Rate
RFR <- read_excel("./Data/Riskfree Rate/Long-term government bond yields 20200821.xls", sheet="RFR")
class(RFR)
RFR <- xts(RFR[,-1], order.by=RFR$Date)
RFR <- na.locf(merge(RFR, foo=zoo(NA, order.by=seq(start(RFR), end(RFR), "day", drop=F)))[, c("RFR_AT", "RFR_EUR")])
RFR <- RFR["2010-01/2020-06"]

MR_ATXPRIME <- read_excel("./Data/Market Rate/ATX Prime 20200930.xlsx") # use german version, replace . with / to get xts compatible date format
class(MR_ATXPRIME)
MR_ATXPRIME <- xts(MR_ATXPRIME[,-1], order.by=MR_ATXPRIME$Date)
MR_ATXPRIME <- MR_ATXPRIME[,colnames(MR_ATXPRIME) == "Return"]
colnames(MR_ATXPRIME)[1] <- "MR_ATXPRIME"
MR_ATXPRIME <- MR_ATXPRIME["2010-01/2020-06"]

MR_ATX <- read_excel("./Data/Market Rate/ATX 20200821.xlsx") # use german version, replace . with / to get xts compatible date format
class(MR_ATX)
MR_ATX <- xts(MR_ATX[,-1], order.by=MR_ATX$Date)
MR_ATX <- MR_ATX[,colnames(MR_ATX) == "Return"]
colnames(MR_ATX)[1] <- "MR_ATX"
MR_ATX <- MR_ATX["2010-01/2020-06"]

MR_VOENIX <- read_excel("./Data/Market Rate/VOX 20200821.xlsx") # use german version, replace . with / to get xts compatible date format
class(MR_VOENIX)
MR_VOENIX <- xts(MR_VOENIX[,-1], order.by=MR_VOENIX$Date)
MR_VOENIX <- MR_VOENIX[,colnames(MR_VOENIX) == "Return"]
colnames(MR_VOENIX)[1] <- "MR_VOENIX"
MR_VOENIX <- MR_VOENIX["2010-01/2020-06"]

CAPMriskmarketxts <- merge(RFR$RFR_AT, RFR$RFR_EUR, MR_ATXPRIME$MR_ATXPRIME, MR_ATX$MR_ATX, MR_VOENIX$MR_VOENIX)
rm(RFR, MR_ATXPRIME, MR_ATX, MR_VOENIX)

CAPMriskmarketxts <- CAPMriskmarketxts[complete.cases(CAPMriskmarketxts[,"MR_ATX"]),]

## Transforming xts object to a dataframe
CAPM <- as.data.table(Returns)
CAPM <- gather(CAPM, `ADKO`, `AGR`, `AMAG`, `ANDR`, `ATS`, `BG`, `CAI`, `DOC`, `EBS`, `EVN`, `FACC`, `FLU`, `FQT`, `IIA`, `KTCG`, `LNZ`, `MARI`, `MMK`, `OMV`, `PAL`, `POS`, `POST`, `PYT`, `RBI`, `ROS`, `SBO`, `SEM`, `SPI`, `STR`, `TKA`, `UBS`, `UQA`, `VER`, `VIG`, `VOE`, `WIE`, `WXF`, `ZAG`, key="Company", value="Return")

## Reporting Standard Dummies, Sustainability Ratings and Industry Affiliations
Companyinfo <- read_excel("./Data/Reporting Standards, Ratings and Industries/Companyinfo.xlsx")
Companyinfo$Overview <- Companyinfo$GRI + Companyinfo$TCFD + Companyinfo$CDP + Companyinfo$GC + Companyinfo$SBTI
CAPM <- full_join(CAPM, Companyinfo, by="Company")

## Merging with Market Rates and Risk Free Rate
CAPMriskmarket <- as.data.table(CAPMriskmarketxts)
colnames(CAPMriskmarket)[1] <- "Date"
colnames(CAPM)[1] <- "Date"
CAPM <- full_join(CAPMriskmarket, CAPM, by="Date")

## USD EUR Exchange Rates
USDEUR <- read_excel("./Data/EUR USD Exchange Rate/USD_EUR.xlsx")
class(USDEUR)
USDEUR$Date <- as.Date(USDEUR$Date,format='%Y-%m-%d')
USDEUR <- xts(USDEUR[,-1], order.by=USDEUR$Date)
USDEUR <- as.data.table(USDEUR)
colnames(USDEUR)[1] <- "Date"

## Multi-Factor Model
Carhart <- read_excel("./Data/Fama French Factors/Europe_5_Factors_incl.MOM_Daily_20201001.xlsx")
class(Carhart)
Carhart$Date <- as.Date(Carhart$Date,format='%Y-%m-%d')
Carhart <- xts(Carhart[,-1], order.by=Carhart$Date)
Carhart <- as.data.table(Carhart)
colnames(Carhart)[1] <- "Date"

Carhart <- full_join(Carhart, USDEUR, by="Date")
CAPM <- full_join(CAPM, Carhart, by="Date")

## Match Return Data Format
CAPM$Mkt_RF_FF <- CAPM$Mkt_RF_FF/100
CAPM$SMB_FF <- CAPM$SMB_FF/100
CAPM$HML_FF <- CAPM$HML_FF/100
CAPM$RMW_FF <- CAPM$RMW_FF/100
CAPM$CMA_FF <- CAPM$CMA_FF/100
CAPM$RF_FF <- CAPM$RF_FF/100
CAPM$WML_FF <- CAPM$WML_FF/100

## Currency Conversion (following "Currency conversion of Fama/French factors: how and why" by Glück et al. (2020))
CAPM$Mkt_RF_EUR_FF <- (((1+CAPM$Mkt_RF_FF+CAPM$RF_FF)/(1+CAPM$r_FX_USDEUR))-1-CAPM$RFR_EUR)

CAPM$SMB_EUR_FF <- CAPM$SMB_FF/(1+CAPM$r_FX_USDEUR)
CAPM$HML_EUR_FF <- CAPM$HML_FF/(1+CAPM$r_FX_USDEUR)
CAPM$RMW_EUR_FF <- CAPM$RMW_FF/(1+CAPM$r_FX_USDEUR)
CAPM$CMA_EUR_FF <- CAPM$CMA_FF/(1+CAPM$r_FX_USDEUR)
CAPM$WML_EUR_FF <- CAPM$WML_FF/(1+CAPM$r_FX_USDEUR)

## Calculate Excess Returns
CAPM$Return_RFR_AT <- CAPM$Return - CAPM$RFR_AT
CAPM$Return_RFR_EUR <- CAPM$Return - CAPM$RFR_EUR
CAPM$ATXPRIME_RFR_AT <- CAPM$MR_ATXPRIME-CAPM$RFR_AT
CAPM$ATX_RFR_AT <- CAPM$MR_ATX-CAPM$RFR_AT
CAPM$VOENIX_RFR_AT <- CAPM$MR_VOENIX-CAPM$RFR_AT

## Market Caps and Trading Volume
MarketCapsDataTable <- MarketCaps["2020-06-30"]
MarketCapsDataTable <- as.data.table(MarketCapsDataTable)
MarketCapsDataTable <- gather(MarketCapsDataTable, `ADKO`, `AGR`, `AMAG`, `ANDR`, `ATS`, `BG`, `CAI`, `DOC`, `EBS`, `EVN`, `FACC`, `FLU`, `FQT`, `IIA`, `KTCG`, `LNZ`, `MARI`, `MMK`, `OMV`, `PAL`, `POS`, `POST`, `PYT`, `RBI`, `ROS`, `SBO`, `SEM`, `SPI`, `STR`, `TKA`, `UBS`, `UQA`, `VER`, `VIG`, `VOE`, `WIE`, `WXF`, `ZAG`, key="Company", value="MarketCap")
colnames(MarketCapsDataTable)[1] <- "Date"

MarketCapsDataTable <- full_join(MarketCapsDataTable, Companyinfo, by="Company")
MarketCapsDataTable$Overview <- MarketCapsDataTable$GRI + MarketCapsDataTable$TCFD + MarketCapsDataTable$CDP + MarketCapsDataTable$GC + MarketCapsDataTable$SBTI
class(MarketCapsDataTable$MarketCap) <- "numeric"

VolumesDataTable <- Volumes["2020-06-30"]
VolumesDataTable <- as.data.table(VolumesDataTable)
VolumesDataTable <- gather(VolumesDataTable, `ADKO`, `AGR`, `AMAG`, `ANDR`, `ATS`, `BG`, `CAI`, `DOC`, `EBS`, `EVN`, `FACC`, `FLU`, `FQT`, `IIA`, `KTCG`, `LNZ`, `MARI`, `MMK`, `OMV`, `PAL`, `POS`, `POST`, `PYT`, `RBI`, `ROS`, `SBO`, `SEM`, `SPI`, `STR`, `TKA`, `UBS`, `UQA`, `VER`, `VIG`, `VOE`, `WIE`, `WXF`, `ZAG`, key="Company", value="Volume")
colnames(VolumesDataTable)[1] <- "Date"

VolumesDataTable <- full_join(VolumesDataTable, Companyinfo, by="Company")
VolumesDataTable$Overview <- VolumesDataTable$GRI + VolumesDataTable$TCFD + VolumesDataTable$CDP + VolumesDataTable$GC + VolumesDataTable$SBTI
class(VolumesDataTable$Volume) <- "numeric"

######## Peculiarities Regarding Size and Trading Volume

#### Differences in trading volume
sum(subset(VolumesDataTable, GRI ==1)$Volume/nrow(subset(VolumesDataTable, GRI ==1)))/sum(subset(VolumesDataTable, GRI ==0)$Volume/nrow(subset(VolumesDataTable, GRI ==0)))
sum(subset(VolumesDataTable, TCFD ==1)$Volume/nrow(subset(VolumesDataTable, TCFD ==1)))/sum(subset(VolumesDataTable, TCFD ==0)$Volume/nrow(subset(VolumesDataTable, TCFD ==0)))
sum(subset(VolumesDataTable, CDP ==1)$Volume/nrow(subset(VolumesDataTable, CDP ==1)))/sum(subset(VolumesDataTable, CDP ==0)$Volume/nrow(subset(VolumesDataTable, CDP ==0)))
sum(subset(VolumesDataTable, GC ==1)$Volume/nrow(subset(VolumesDataTable, GC ==1)))/sum(subset(VolumesDataTable, GC ==0)$Volume/nrow(subset(VolumesDataTable, GC ==0)))
sum(subset(VolumesDataTable, SBTI ==1)$Volume/nrow(subset(VolumesDataTable, SBTI ==1)))/sum(subset(VolumesDataTable, SBTI ==0)$Volume/nrow(subset(VolumesDataTable, SBTI ==0)))
# between 1.7 and 3.1 times larger

#### Differences in Market Caps
sum(subset(MarketCapsDataTable, GRI ==1)$MarketCap/nrow(subset(MarketCapsDataTable, GRI ==1)))/sum(subset(MarketCapsDataTable, GRI ==0)$MarketCap/nrow(subset(MarketCapsDataTable, GRI ==0)))
sum(subset(MarketCapsDataTable, TCFD ==1)$MarketCap/nrow(subset(MarketCapsDataTable, TCFD ==1)))/sum(subset(MarketCapsDataTable, TCFD ==0)$MarketCap/nrow(subset(MarketCapsDataTable, TCFD ==0)))
sum(subset(MarketCapsDataTable, CDP ==1)$MarketCap/nrow(subset(MarketCapsDataTable, CDP ==1)))/sum(subset(MarketCapsDataTable, CDP ==0)$MarketCap/nrow(subset(MarketCapsDataTable, CDP ==0)))
sum(subset(MarketCapsDataTable, GC ==1)$MarketCap/nrow(subset(MarketCapsDataTable, GC ==1)))/sum(subset(MarketCapsDataTable, GC ==0)$MarketCap/nrow(subset(MarketCapsDataTable, GC ==0)))
sum(subset(MarketCapsDataTable, SBTI ==1)$MarketCap/nrow(subset(MarketCapsDataTable, SBTI ==1)))/sum(subset(MarketCapsDataTable, SBTI ==0)$MarketCap/nrow(subset(MarketCapsDataTable, SBTI ==0)))
# between 2.2 and 3.25 times larger

## Boxplots Market Caps and Trading Volumes
MarketCapsDataTable$GRI <- factor(MarketCapsDataTable$GRI)
MarketCapsDataTable$TCFD <- factor(MarketCapsDataTable$TCFD)
MarketCapsDataTable$CDP <- factor(MarketCapsDataTable$CDP)
MarketCapsDataTable$GC <- factor(MarketCapsDataTable$GC)
MarketCapsDataTable$SBTI <- factor(MarketCapsDataTable$SBTI)
MarketCapsDataTableboxplot <- gather(MarketCapsDataTable, GRI, TCFD, CDP, GC, SBTI, key=`Standards`, value=`Compliance`)
MarketCapsDataTableboxplot$Compliance[MarketCapsDataTableboxplot$Compliance == 0] <- "no"
MarketCapsDataTableboxplot$Compliance[MarketCapsDataTableboxplot$Compliance == 1] <- "yes"
MarketCapsDataTableboxplot$Standards[MarketCapsDataTableboxplot$Standards == "GC"] <- "UNGC"
MarketCapsDataTableboxplot$MarketCap <- MarketCapsDataTableboxplot$MarketCap/1000000000

VolumesDataTable$GRI <- factor(VolumesDataTable$GRI)
VolumesDataTable$TCFD <- factor(VolumesDataTable$TCFD)
VolumesDataTable$CDP <- factor(VolumesDataTable$CDP)
VolumesDataTable$GC <- factor(VolumesDataTable$GC)
VolumesDataTable$SBTI <- factor(VolumesDataTable$SBTI)
VolumesDataTableboxplot <- gather(VolumesDataTable, GRI, TCFD, CDP, GC, SBTI, key=`Standards`, value=`Compliance`)
VolumesDataTableboxplot$Compliance[VolumesDataTableboxplot$Compliance == 0] <- "no"
VolumesDataTableboxplot$Compliance[VolumesDataTableboxplot$Compliance == 1] <- "yes"
VolumesDataTableboxplot$Standards[VolumesDataTableboxplot$Standards == "GC"] <- "UNGC"
VolumesDataTableboxplot$Volume <- VolumesDataTableboxplot$Volume/1000000

ggplot11 <- ggplot(MarketCapsDataTableboxplot, aes(x=Standards, y=`MarketCap`, fill=Compliance)) + 
  geom_boxplot(width=0.7) +
  stat_summary(fun.y="mean", geom="point", shape = 17, size=2,
               position=position_dodge(width=0.70), color="black") +
  labs(y = "Market Capitalizations (in Billion €)", x = "") + 
  scale_fill_manual(values=c("#99CCFF", "#66CC99")) +
  theme(legend.position = "none")

ggplot22 <- ggplot(VolumesDataTableboxplot, aes(x=Standards, y=`Volume`, fill=Compliance)) + 
  geom_boxplot(width=0.7) +
  stat_summary(fun.y="mean", geom="point", shape = 17, size=2,
               position=position_dodge(width=0.70), color="black") +
  labs(y = "Trading Volumes (in Million Shares)", x = "Reporting Standards") + 
  scale_fill_manual(values=c("#99CCFF", "#66CC99")) +
  theme(legend.position = "none")

grid.arrange(ggplot11, ggplot22)

######## Industry Affiliation
Companyinfo$GRI <- factor(Companyinfo$GRI)
Companyinfo$TCFD <- factor(Companyinfo$TCFD)
Companyinfo$CDP <- factor(Companyinfo$CDP)
Companyinfo$GC <- factor(Companyinfo$GC)
Companyinfo$SBTI <- factor(Companyinfo$SBTI)

Companyinfoboxplot <- gather(Companyinfo, GRI, TCFD, CDP, GC, SBTI, key=`Standards`, value=`Compliance`)
Companyinfoboxplot$Compliance[Companyinfoboxplot$Compliance == 0] <- "no"
Companyinfoboxplot$Compliance[Companyinfoboxplot$Compliance == 1] <- "yes"
Companyinfoboxplot$Standards[Companyinfoboxplot$Standards == "GC"] <- "UNGC"

Companyinfoindustries <- subset(Companyinfo, select=-c(GRI, TCFD, GC, SBTI, CDP))
Companyinfoindustries$Standards <- " Prime market"
Companyinfoindustries$Compliance <- "yes"
Companyinfobarplot <- full_join(Companyinfoboxplot, Companyinfoindustries)
Companyinfobarplot <- subset(Companyinfobarplot, Compliance == "yes")

ggplot(Companyinfobarplot, aes(fill=Industry, y="", x=Standards)) + 
  geom_bar(width = 0.7, position="fill", stat="identity") +
  ylab("Share                                                        ") +
  xlab("") +
  scale_fill_brewer(palette ="BrBG") +
  scale_y_discrete(breaks=c("0","0.5","1")) +
  theme(legend.position = "right") +
  theme(plot.margin=unit(c(-0.47,0,0,0), "null"), text = element_text(size = 15), axis.text.x=element_text(colour="black")) +
  coord_fixed(ratio = 5)

######## Sustainability Ratings
ggplot1 <- ggplot(Companyinfoboxplot, aes(x=Standards, y=`CSRHub CSR Rating`, fill=Compliance)) + 
  geom_boxplot(width=0.7) +
  stat_summary(fun.y="mean", geom="point", shape = 17, size=2,
               position=position_dodge(width=0.70), color="black") +
  labs(x="", y = "CSRHub CSR Rating") + 
  scale_fill_manual(values=c("#99CCFF", "#66CC99")) +
  theme(legend.position = "none")

ggplot2 <- ggplot(Companyinfoboxplot, aes(x=Standards, y=`Sustainalytics ESG Risk Rating`, fill=Compliance)) + 
  geom_boxplot(width=0.7) +
  stat_summary(fun.y="mean", geom="point", shape = 17, size=2,
               position=position_dodge(width=0.70), color="black") +
  labs(y = "Sustainalytics ESG Risk Rating", x="Reporting Standards") + 
  scale_fill_manual(values=c("#99CCFF", "#66CC99")) +
  theme(legend.position = "none")

grid.arrange(ggplot1, ggplot2)

## Descriptive Statistics for the Sustainability Ratings
summary(subset(Companyinfo, GRI ==1))
summary(subset(Companyinfo, GRI ==0))
summary(subset(Companyinfo, TCFD ==1))
summary(subset(Companyinfo, TCFD ==0))
summary(subset(Companyinfo, CDP ==1))
summary(subset(Companyinfo, CDP ==0))
summary(subset(Companyinfo, GC ==1))
summary(subset(Companyinfo, GC ==0))
summary(subset(Companyinfo, SBTI ==1))
summary(subset(Companyinfo, SBTI ==0))

summary(subset(Companyinfo, Overview ==5))
summary(subset(Companyinfo, Overview ==4))
summary(subset(Companyinfo, Overview >=1))
summary(subset(Companyinfo, Overview ==1))
summary(subset(Companyinfo, Overview ==0))

summary(subset(Companyinfo, ATX ==1))
summary(subset(Companyinfo, ATX ==0))
summary(Companyinfo)

######## Return Analysis

#### Descriptive statistics
printdescriptives <- function(selection){
  print.xtable(xtable(subset(describeBy((subset(CAPM, selection)$Return)*100), select=c("mean", "median", "min", "max", "sd", "skew", "kurtosis")), caption="summary statistics", label="summary1", digits=2), booktabs=TRUE)
}

printdescriptives(CAPM$GRI == 1)        
printdescriptives(CAPM$GRI == 0)        
printdescriptives(CAPM$TCFD == 1)        
printdescriptives(CAPM$TCFD == 0)        
printdescriptives(CAPM$CDP == 1)        
printdescriptives(CAPM$CDP == 0)        
printdescriptives(CAPM$GC == 1)        
printdescriptives(CAPM$GC == 0)        
printdescriptives(CAPM$SBTI == 1)        
printdescriptives(CAPM$SBTI == 0)        

printdescriptives(CAPM$Overview == 5)        
printdescriptives(CAPM$Overview == 4)        
printdescriptives(CAPM$Overview >= 1)        
printdescriptives(CAPM$Overview == 1)        
printdescriptives(CAPM$Overview == 0)        
printdescriptives(CAPM$ATX == 1)        
printdescriptives(CAPM$ATX == 0)  
printdescriptives(TRUE) # full prime market
     
#### Welch's two-sided t-tests for unpaired samples
t.test(subset(CAPM, GRI == 1)$Return, subset(CAPM, GRI == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, TCFD == 1)$Return, subset(CAPM, TCFD == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, CDP == 1)$Return, subset(CAPM, CDP == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, GC == 1)$Return, subset(CAPM, GC == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, SBTI == 1)$Return, subset(CAPM, SBTI == 0)$Return, paired = FALSE, alternative = "two.sided")

t.test(subset(CAPM, Overview == 5)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 5)$Return, subset(CAPM, Overview >= 1)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 4)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 4)$Return, subset(CAPM, Overview >= 1)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview >= 1)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 1)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 1)$Return, subset(CAPM, Overview >= 1)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, ATX == 1)$Return, subset(CAPM, ATX == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, ATX == 1)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, ATX == 0)$Return, subset(CAPM, Overview == 0)$Return, paired = FALSE, alternative = "two.sided")
t.test(subset(CAPM, Overview == 0)$Return, CAPM$Return, paired = FALSE, alternative = "two.sided")

#### Histogram
ggplot(subset(CAPM, GRI == 1), aes(x=Return)) + 
  geom_density(aes(color = "green")) +
  geom_density(data=subset(CAPM, TCFD == 1), aes(color = "green")) +
  geom_density(data=subset(CAPM, CDP == 1), aes(color = "green")) +
  geom_density(data=subset(CAPM, GC == 1), aes(color = "green")) +
  geom_density(data=subset(CAPM, SBTI == 1), aes(color = "green")) +
  geom_density(data=subset(CAPM, GRI == 0), aes(color = "blue")) +
  geom_density(data=subset(CAPM, TCFD == 0), aes(color = "blue")) +
  geom_density(data=subset(CAPM, CDP == 0), aes(color = "blue")) +
  geom_density(data=subset(CAPM, GC == 0), aes(color = "blue")) +
  geom_density(data=subset(CAPM, SBTI == 0), aes(color = "blue")) +
  xlim(-0.05, 0.05) +
  ylim(0, 40) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_colour_manual(values = c("blue" = "royalblue3","green" = "green4")) +
  theme(legend.position = "none") + 
  labs(y = "Density in %", x ="Daily Returns")

# Replace "theme(legend.position = "none")" with the following to add a legend: 
# theme(legend.position="bottom", legend.title=element_blank(), legend.key.size = unit(1,"line")) + guides(fill=guide_legend(title=NULL)) + scale_colour_manual(values = c("blue" = "royalblue3","green" = "green4")) + scale_color_manual(labels = c("Non-compliant companies", "Compliant companies"), values = c("royalblue3", "green4")) + theme(legend.position="right", legend.title=element_blank(), legend.key.size = unit(1,"line"))
  
#### Empirical Cumulative Distribution Function
CAPMECDF = CAPM
CAPMECDF$Return <- abs(CAPMECDF$Return)

ggplot(subset(CAPMECDF, Overview == 0), aes(x=Return)) + 
  stat_ecdf(aes(color = "blue"), geom = "step") +
  stat_ecdf(data=subset(CAPMECDF, Overview >= 1), aes(color = "green"), geom = "step") + 
  labs(y = "Cumulative Distribution", x ="Absolute Daily Returns") +
  xlim(0, 0.05) +
  ylim(0, 1) +
  scale_colour_manual(values = c("blue" = "royalblue3","green" = "green4")) +
  theme(legend.position = "null")

# Replace "theme(legend.position = "null")" with the following to add a legend: 
# theme(legend.position = "right", legend.title=element_blank(), legend.key.size = unit(2,"line")) + guides(fill=guide_legend(title=NULL)) + scale_color_manual(labels = c("No standard", "At least one standard"), values = c("royalblue3", "green4"))

## Other Plots for Reference
ggplotecdffunction <- function(selection1, selection2){
    ggplot(subset(CAPMECDF, selection1), aes(x=Return)) + 
    stat_ecdf(aes(color = "blue"), geom = "step") +
    stat_ecdf(data=subset(CAPMECDF, selection2), aes(color = "green"), geom = "step") + 
    labs(y = "Cumulative Distribution", x ="Absolute Daily Returns") +
    xlim(0, 0.05) +
    ylim(0, 1) +
    scale_colour_manual(values = c("blue" = "royalblue3","green" = "green4")) +
    theme(legend.position = "null")
}

ggplotecdffunction(CAPMECDF$GRI == 0, CAPMECDF$GRI == 1)
ggplotecdffunction(CAPMECDF$TCFD == 0, CAPMECDF$TCFD == 1)
ggplotecdffunction(CAPMECDF$CDP == 0, CAPMECDF$CDP == 1)
ggplotecdffunction(CAPMECDF$GC == 0, CAPMECDF$GC == 1)
ggplotecdffunction(CAPMECDF$SBTI == 0, CAPMECDF$SBTI == 1)

ggplotecdffunction(CAPMECDF$Overview == 1, CAPMECDF$Overview >= 1)
ggplotecdffunction(CAPMECDF$Overview == 1, CAPMECDF$Overview == 5)
ggplotecdffunction(CAPMECDF$Overview == 1, CAPMECDF$Overview == 5)
ggplotecdffunction(CAPMECDF$Overview == 1, CAPMECDF$Overview == 4)
ggplotecdffunction(CAPMECDF$Overview >= 1, CAPMECDF$Overview == 5)
ggplotecdffunction(CAPMECDF$Overview >= 1, CAPMECDF$Overview == 4)
ggplotecdffunction(CAPMECDF$ATX == 0, CAPMECDF$ATX == 1)

#### CAPM

## Single-Factor CAPM with the ATX Prime as the proxy for the market rate
summary(lm(subset(CAPM, GRI == 1)$Return_RFR_AT ~ subset(CAPM, GRI == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GRI == 0)$Return_RFR_AT ~ subset(CAPM, GRI == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 1)$Return_RFR_AT ~ subset(CAPM, TCFD == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 0)$Return_RFR_AT ~ subset(CAPM, TCFD == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 1)$Return_RFR_AT ~ subset(CAPM, CDP == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 0)$Return_RFR_AT ~ subset(CAPM, CDP == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 1)$Return_RFR_AT ~ subset(CAPM, GC == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 0)$Return_RFR_AT ~ subset(CAPM, GC == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 1)$Return_RFR_AT ~ subset(CAPM, SBTI == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 0)$Return_RFR_AT ~ subset(CAPM, SBTI == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))

summary(lm(subset(CAPM, Overview == 5)$Return_RFR_AT ~ subset(CAPM, Overview == 5)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 4)$Return_RFR_AT ~ subset(CAPM, Overview == 4)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview >= 1)$Return_RFR_AT ~ subset(CAPM, Overview >= 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 1)$Return_RFR_AT ~ subset(CAPM, Overview == 1)$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 0)$Return_RFR_AT ~ subset(CAPM, Overview == 0)$ATXPRIME_RFR_AT, na.action=na.exclude))

## Single-Factor CAPM with the ATX as the proxy for the market rate
summary(lm(subset(CAPM, GRI == 1)$Return_RFR_AT ~ subset(CAPM, GRI == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GRI == 0)$Return_RFR_AT ~ subset(CAPM, GRI == 0)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 1)$Return_RFR_AT ~ subset(CAPM, TCFD == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 0)$Return_RFR_AT ~ subset(CAPM, TCFD == 0)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 1)$Return_RFR_AT ~ subset(CAPM, CDP == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 0)$Return_RFR_AT ~ subset(CAPM, CDP == 0)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 1)$Return_RFR_AT ~ subset(CAPM, GC == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 0)$Return_RFR_AT ~ subset(CAPM, GC == 0)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 1)$Return_RFR_AT ~ subset(CAPM, SBTI == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 0)$Return_RFR_AT ~ subset(CAPM, SBTI == 0)$ATX_RFR_AT, na.action=na.exclude))

summary(lm(subset(CAPM, Overview == 5)$Return_RFR_AT ~ subset(CAPM, Overview == 5)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 4)$Return_RFR_AT ~ subset(CAPM, Overview == 4)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview >= 1)$Return_RFR_AT ~ subset(CAPM, Overview >= 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 1)$Return_RFR_AT ~ subset(CAPM, Overview == 1)$ATX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 0)$Return_RFR_AT ~ subset(CAPM, Overview == 0)$ATX_RFR_AT, na.action=na.exclude))

## Single-Factor CAPM with the VÖNIX as the proxy for the market rate
summary(lm(subset(CAPM, GRI == 1)$Return_RFR_AT ~ subset(CAPM, GRI == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GRI == 0)$Return_RFR_AT ~ subset(CAPM, GRI == 0)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 1)$Return_RFR_AT ~ subset(CAPM, TCFD == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 0)$Return_RFR_AT ~ subset(CAPM, TCFD == 0)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 1)$Return_RFR_AT ~ subset(CAPM, CDP == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 0)$Return_RFR_AT ~ subset(CAPM, CDP == 0)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 1)$Return_RFR_AT ~ subset(CAPM, GC == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 0)$Return_RFR_AT ~ subset(CAPM, GC == 0)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 1)$Return_RFR_AT ~ subset(CAPM, SBTI == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 0)$Return_RFR_AT ~ subset(CAPM, SBTI == 0)$VOENIX_RFR_AT, na.action=na.exclude))

summary(lm(subset(CAPM, Overview == 5)$Return_RFR_AT ~ subset(CAPM, Overview == 5)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 4)$Return_RFR_AT ~ subset(CAPM, Overview == 4)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview >= 1)$Return_RFR_AT ~ subset(CAPM, Overview >= 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 1)$Return_RFR_AT ~ subset(CAPM, Overview == 1)$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 0)$Return_RFR_AT ~ subset(CAPM, Overview == 0)$VOENIX_RFR_AT, na.action=na.exclude))

## Models with interaction terms to capture the significance of the differences in alphas and betas in the single-factor CAPM model

# ATX PRIME
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GRI*CAPM$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$TCFD*CAPM$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$CDP*CAPM$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GC*CAPM$ATXPRIME_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$SBTI*CAPM$ATXPRIME_RFR_AT, na.action=na.exclude))

# ATX
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GRI*CAPM$ATX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$TCFD*CAPM$ATX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$CDP*CAPM$ATX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GC*CAPM$ATX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$SBTI*CAPM$ATX_RFR_AT, na.action=na.exclude))

# VÖNIX
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GRI*CAPM$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$TCFD*CAPM$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$CDP*CAPM$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$GC*CAPM$VOENIX_RFR_AT, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_AT ~ CAPM$SBTI*CAPM$VOENIX_RFR_AT, na.action=na.exclude))

## Carhart Four-Factor Model in European Setting
summary(lm(subset(CAPM, GRI == 1)$Return_RFR_EUR ~ subset(CAPM, GRI == 1)$Mkt_RF_EUR_FF + subset(CAPM, GRI == 1)$SMB_EUR_FF + subset(CAPM, GRI == 1)$HML_EUR_FF + subset(CAPM, GRI == 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, GRI == 0)$Return_RFR_EUR ~ subset(CAPM, GRI == 0)$Mkt_RF_EUR_FF + subset(CAPM, GRI == 0)$SMB_EUR_FF + subset(CAPM, GRI == 0)$HML_EUR_FF + subset(CAPM, GRI == 0)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 1)$Return_RFR_EUR ~ subset(CAPM, TCFD == 1)$Mkt_RF_EUR_FF + subset(CAPM, TCFD == 1)$SMB_EUR_FF + subset(CAPM, TCFD == 1)$HML_EUR_FF + subset(CAPM, TCFD == 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, TCFD == 0)$Return_RFR_EUR ~ subset(CAPM, TCFD == 0)$Mkt_RF_EUR_FF + subset(CAPM, TCFD == 0)$SMB_EUR_FF + subset(CAPM, TCFD == 0)$HML_EUR_FF + subset(CAPM, TCFD == 0)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, CDP == 1)$Return_RFR_EUR ~ subset(CAPM, CDP == 1)$Mkt_RF_EUR_FF + subset(CAPM, CDP == 1)$SMB_EUR_FF + subset(CAPM, CDP == 1)$HML_EUR_FF + subset(CAPM, CDP == 1)$WML_EUR_FF, na.action=na.exclude)) 
summary(lm(subset(CAPM, CDP == 0)$Return_RFR_EUR ~ subset(CAPM, CDP == 0)$Mkt_RF_EUR_FF + subset(CAPM, CDP == 0)$SMB_EUR_FF + subset(CAPM, CDP == 0)$HML_EUR_FF + subset(CAPM, CDP == 0)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 1)$Return_RFR_EUR ~ subset(CAPM, GC == 1)$Mkt_RF_EUR_FF + subset(CAPM, GC == 1)$SMB_EUR_FF + subset(CAPM, GC == 1)$HML_EUR_FF + subset(CAPM, GC == 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, GC == 0)$Return_RFR_EUR ~ subset(CAPM, GC == 0)$Mkt_RF_EUR_FF + subset(CAPM, GC == 0)$SMB_EUR_FF + subset(CAPM, GC == 0)$HML_EUR_FF + subset(CAPM, GC == 0)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 1)$Return_RFR_EUR ~ subset(CAPM, SBTI == 1)$Mkt_RF_EUR_FF + subset(CAPM, SBTI == 1)$SMB_EUR_FF + subset(CAPM, SBTI == 1)$HML_EUR_FF + subset(CAPM, SBTI == 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, SBTI == 0)$Return_RFR_EUR ~ subset(CAPM, SBTI == 0)$Mkt_RF_EUR_FF + subset(CAPM, SBTI == 0)$SMB_EUR_FF + subset(CAPM, SBTI == 0)$HML_EUR_FF + subset(CAPM, SBTI == 0)$WML_EUR_FF, na.action=na.exclude))

summary(lm(subset(CAPM, Overview == 5)$Return_RFR_EUR ~ subset(CAPM, Overview == 5)$Mkt_RF_EUR_FF + subset(CAPM, Overview == 5)$SMB_EUR_FF + subset(CAPM, Overview == 5)$HML_EUR_FF + subset(CAPM, Overview == 5)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 4)$Return_RFR_EUR ~ subset(CAPM, Overview == 4)$Mkt_RF_EUR_FF + subset(CAPM, Overview == 4)$SMB_EUR_FF + subset(CAPM, Overview == 4)$HML_EUR_FF + subset(CAPM, Overview == 4)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, Overview >= 1)$Return_RFR_EUR ~ subset(CAPM, Overview >= 1)$Mkt_RF_EUR_FF + subset(CAPM, Overview >= 1)$SMB_EUR_FF + subset(CAPM, Overview >= 1)$HML_EUR_FF + subset(CAPM, Overview >= 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 1)$Return_RFR_EUR ~ subset(CAPM, Overview == 1)$Mkt_RF_EUR_FF + subset(CAPM, Overview == 1)$SMB_EUR_FF + subset(CAPM, Overview == 1)$HML_EUR_FF + subset(CAPM, Overview == 1)$WML_EUR_FF, na.action=na.exclude))
summary(lm(subset(CAPM, Overview == 0)$Return_RFR_EUR ~ subset(CAPM, Overview == 0)$Mkt_RF_EUR_FF + subset(CAPM, Overview == 0)$SMB_EUR_FF + subset(CAPM, Overview == 0)$HML_EUR_FF + subset(CAPM, Overview == 0)$WML_EUR_FF, na.action=na.exclude))

## Models with interaction terms to capture the significance of the differences in alphas and betas in the multi-factor CAPM model
summary(lm(CAPM$Return_RFR_EUR ~ CAPM$GRI*CAPM$Mkt_RF_EUR_FF + CAPM$GRI*CAPM$SMB_EUR_FF + CAPM$GRI*CAPM$HML_EUR_FF + CAPM$GRI*CAPM$WML_EUR_FF, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_EUR ~ CAPM$TCFD*CAPM$Mkt_RF_EUR_FF + CAPM$TCFD*CAPM$SMB_EUR_FF + CAPM$TCFD*CAPM$HML_EUR_FF + CAPM$TCFD*CAPM$WML_EUR_FF, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_EUR ~ CAPM$CDP*CAPM$Mkt_RF_EUR_FF + CAPM$CDP*CAPM$SMB_EUR_FF + CAPM$CDP*CAPM$HML_EUR_FF + CAPM$CDP*CAPM$WML_EUR_FF, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_EUR ~ CAPM$GC*CAPM$Mkt_RF_EUR_FF + CAPM$GC*CAPM$SMB_EUR_FF + CAPM$GC*CAPM$HML_EUR_FF + CAPM$GC*CAPM$WML_EUR_FF, na.action=na.exclude))
summary(lm(CAPM$Return_RFR_EUR ~ CAPM$SBTI*CAPM$Mkt_RF_EUR_FF + CAPM$SBTI*CAPM$SMB_EUR_FF + CAPM$SBTI*CAPM$HML_EUR_FF + CAPM$SBTI*CAPM$WML_EUR_FF, na.action=na.exclude))

#### Conditional volatility analysis

## Removing 3 rows with missing values
sum(is.na(Returns_Means$TCFD_Mean))
sum(is.na(Returns_Means$AllFive_Mean))

Returns_Means_withoutNA <- Returns_Means[complete.cases(Returns_Means[,"AllFive_Mean"]),]
Returns_Means_withoutNA <- Returns_Means_withoutNA[complete.cases(Returns_Means_withoutNA[,"TCFD_Mean"]),]

# Augmented Dickey-Fuller test
tseries::adf.test(Returns_Means_withoutNA$GRI_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonGRI_Mean)
tseries::adf.test(Returns_Means_withoutNA$TCFD_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonTCFD_Mean)
tseries::adf.test(Returns_Means_withoutNA$CDP_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonCDP_Mean)
tseries::adf.test(Returns_Means_withoutNA$GC_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonGC_Mean)
tseries::adf.test(Returns_Means_withoutNA$SBTI_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonSBTI_Mean)

tseries::adf.test(Returns_Means_withoutNA$AllFive_Mean)
tseries::adf.test(Returns_Means_withoutNA$AllFiveExceptOne_Mean)
tseries::adf.test(Returns_Means_withoutNA$AtLeastOneStandard_Mean)
tseries::adf.test(Returns_Means_withoutNA$JustOneStandard_Mean)
tseries::adf.test(Returns_Means_withoutNA$NoStandard_Mean)
tseries::adf.test(Returns_Means_withoutNA$ATX_Mean)
tseries::adf.test(Returns_Means_withoutNA$nonATX_Mean)
tseries::adf.test(Returns_Means_withoutNA$All_Mean)

# Phillips Perron test
PP.test(Returns_Means_withoutNA$GRI_Mean)
PP.test(Returns_Means_withoutNA$nonGRI_Mean)
PP.test(Returns_Means_withoutNA$TCFD_Mean)
PP.test(Returns_Means_withoutNA$nonTCFD_Mean)
PP.test(Returns_Means_withoutNA$CDP_Mean)
PP.test(Returns_Means_withoutNA$nonCDP_Mean)
PP.test(Returns_Means_withoutNA$GC_Mean)
PP.test(Returns_Means_withoutNA$nonGC_Mean)
PP.test(Returns_Means_withoutNA$SBTI_Mean)
PP.test(Returns_Means_withoutNA$nonSBTI_Mean)

PP.test(Returns_Means_withoutNA$AllFive_Mean)
PP.test(Returns_Means_withoutNA$AllFiveExceptOne_Mean)
PP.test(Returns_Means_withoutNA$AtLeastOneStandard_Mean)
PP.test(Returns_Means_withoutNA$JustOneStandard_Mean)
PP.test(Returns_Means_withoutNA$NoStandard_Mean)
PP.test(Returns_Means_withoutNA$ATX_Mean)
PP.test(Returns_Means_withoutNA$nonATX_Mean)
PP.test(Returns_Means_withoutNA$All_Mean)

## Plotting the returns in the entire prime market
ggplot(CAPM, aes(x=Date)) + 
  geom_line(aes(y=Return)) + 
  labs(x="Year", y="Daily Returns") +
  scale_y_continuous(breaks=c(-0.20,-0.10,0.00,0.10,0.20, 0.30, 0.40)) + 
  scale_x_datetime(breaks = as.POSIXct(c("2010-01-04", "2012-01-02", "2014-01-02", "2016-01-04", "2018-01-02", "2020-01-02")),
                   labels = c("2010", "2012", "2014", "2016", "2018", "2020"))

## LM test for ARCH lags (1)
ArchTest(Returns_Means_withoutNA$GRI_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonGRI_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$TCFD_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonTCFD_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$CDP_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonCDP_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$GC_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonGC_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$SBTI_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonSBTI_Mean, lags=1)

ArchTest(Returns_Means_withoutNA$AllFive_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$AllFiveExceptOne_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$AtLeastOneStandard_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$JustOneStandard_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$NoStandard_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$ATX_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$nonATX_Mean, lags=1)
ArchTest(Returns_Means_withoutNA$All_Mean, lags=1)

## White Noise Test / Portmanteau Test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$GRI_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$GRI_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonGRI_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonGRI_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$TCFD_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$TCFD_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonTCFD_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonTCFD_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$CDP_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$CDP_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonCDP_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonCDP_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$GC_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$GC_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonGC_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonGC_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$SBTI_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$SBTI_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonSBTI_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonSBTI_Mean), method="LiMcLeod")$test

whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$AllFive_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$AllFive_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$AllFiveExceptOne_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$AllFiveExceptOne_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$AtLeastOneStandard_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$AtLeastOneStandard_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$JustOneStandard_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$JustOneStandard_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$NoStandard_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$NoStandard_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$ATX_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$ATX_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$nonATX_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$nonATX_Mean), method="LiMcLeod")$test
whiteNoiseTest(autocorrelations(Returns_Means_withoutNA$All_Mean), h0="iid", n=nrow(Returns_Means_withoutNA$All_Mean), method="LiMcLeod")$test

## GARCH (1,1) Models
SpecGarchModel <- ugarchspec(variance.model = list(model = "sGARCH", submodel = NULL, garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,0)), distribution.model="norm")

print(round(ugarchfit(data = Returns_Means_withoutNA$GRI_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonGRI_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$TCFD_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonTCFD_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$CDP_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonCDP_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$GC_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonGC_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$SBTI_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonSBTI_Mean, spec = SpecGarchModel)@fit$matcoef, 3))

print(round(ugarchfit(data = Returns_Means_withoutNA$AllFive_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$AllFiveExceptOne_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$AtLeastOneStandard_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$JustOneStandard_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$NoStandard_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$ATX_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$nonATX_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
print(round(ugarchfit(data = Returns_Means_withoutNA$All_Mean, spec = SpecGarchModel)@fit$matcoef, 3))
