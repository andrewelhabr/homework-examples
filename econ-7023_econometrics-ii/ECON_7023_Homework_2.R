#' ---
#' title: "Appendix: R Code and Output"
#' author: ""
#' output:
#'  html_document:
#'    toc: true
#'    keep_md: true
#'    theme: united
#'    highligh: tango
#' ---
#'
#'
#'
#+ global_options, include = FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  cache = FALSE,
  fig.show = "hide",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

#' # 4.11

rm(list = ls())

library('haven')
nls80 = read_dta(file = 'nls80.dta', encoding = NULL)

#' ## a) and c)

model_nls80_1 = lm(lwage ~ exper + tenure + married + south + urban + black + educ, data = nls80)
summary(model_nls80_1)

model_nls80_2 = lm(lwage ~ exper + tenure + married + south + urban + black + educ + iq, data = nls80)
summary(model_nls80_2)

model_nls80_3 = lm(lwage ~ exper + tenure + married + south + urban + black + educ + kww, data = nls80)
summary(model_nls80_3)

model_nls80_4 = lm(lwage ~ exper + tenure + married + south + urban + black + educ + iq + kww, data = nls80)
summary(model_nls80_4)

#' ## b)

fstat_1 = (sum(model_nls80_1$residuals^2) - sum(model_nls80_4$residuals^2))/sum(model_nls80_4$residuals^2)*925/2
fstat_1
pval_1 = 1-pf(fstat_1, 2, 925)
pval_1

#' ## d)

nls80$educiqadj = nls80$educ*(nls80$iq - 100)
nls80$educkwwadj = nls80$educ*(nls80$kww - mean(nls80$kww))

model_nls80_5 = lm(lwage ~ exper + tenure + married + south + urban + black + educ + iq + kww + educiqadj + educkwwadj, data = nls80)
summary(model_nls80_5)

fstat_2 = (sum(model_nls80_4$residuals^2) - sum(model_nls80_5$residuals^2))/sum(model_nls80_5$residuals^2)*923/2
fstat_2
pval_2 = 1-pf(fstat_2, 2, 923)
pval_2



#' # 4.13
 
rm(list = ls())

library('haven')
cornwell = read_dta(file = 'cornwell.dta', encoding = NULL)

#' ## a)

cornwell_87 = cornwell[which(cornwell$d87 == 1),]
model_cornwell_1 = lm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen, data = cornwell_87)
summary(model_cornwell_1)

#' ## b)

cornwell_86 = cornwell[which(cornwell$d86 == 1),]
cornwell_87$lcrmrte86 = cornwell_86$lcrmrte
model_cornwell_2 = lm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lcrmrte86, data = cornwell_87)
summary(model_cornwell_2)

#' ## c)

model_cornwell_3 = lm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lcrmrte86 + lwcon +lwtuc +lwtrd +lwfir + lwser + lwmfg +lwfed + lwsta + lwloc, data = cornwell_87)
summary(model_cornwell_3)

fstat_1 = (sum(model_cornwell_2$residuals^2) - sum(model_cornwell_3$residuals^2))/sum(model_cornwell_3$residuals^2)*75/9
fstat_1
pval_1 = 1-pf(fstat_1, 9, 75)
pval_1

#' ## d)

library('lmtest')
library('sandwich')

waldtest(model_cornwell_3,model_cornwell_2,vcov=vcovHC(model_cornwell_3, type = 'HC1'))



#' # 4.14
 
rm(list = ls())

library('haven')
attend = read_dta(file = 'attend.dta', encoding = NULL)

#' ## a)

model_attend_1 = lm(stndfnl ~ frosh + soph + atndrte, data = attend)
summary(model_attend_1)

#' ## c)

model_attend_2 = lm(stndfnl ~ frosh + soph + atndrte + priGPA + ACT, data = attend)
summary(model_attend_2)

#' ## e)

attend$priGPAsq = attend$priGPA^2
attend$ACTsq = attend$ACT^2
model_attend_3 = lm(stndfnl ~ frosh + soph + atndrte + priGPA + ACT + priGPAsq + ACTsq, data = attend)
summary(model_attend_3)

fstat_1 = (sum(model_attend_2$residuals^2) - sum(model_attend_3$residuals^2))/sum(model_attend_3$residuals^2)*671/2
fstat_1
pval_1 = 1-pf(fstat_1, 2, 671)
pval_1

#' ## f)

attend$atndrtesq = attend$atndrte^2
model_attend_4 = lm(stndfnl ~ frosh + soph + atndrte + priGPA + ACT + priGPAsq + ACTsq + atndrtesq, data = attend)
summary(model_attend_4)



#' # 5.3
 
rm(list = ls())

library('haven')
bwght = read_dta(file = 'bwght.dta', encoding = NULL)

#' ## c)

model_bwght_1 = lm(lbwght ~ male + parity + lfaminc + packs, data = bwght)
summary(model_bwght_1)

model_bwght_iv <- AER::ivreg(lbwght ~ male + parity + lfaminc + packs | male + parity + lfaminc + cigprice, data = bwght)
summary(model_bwght_iv, diagnostics = TRUE)

#' ## d)

model_bwght_2 = lm(packs ~ male + parity + lfaminc + cigprice, data = bwght)
summary(model_bwght_2)


