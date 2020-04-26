## Analisis de T° Corporal de C. crassidigitus en pastizales y bosque ZP Las Tabla, para manuscrito
## sometido a la Revista Latinoamericana de Herpetologia. Datos liberados con el ánimo de realizar  
## colaboraciones contactar a Diego Gomez e-mail: biodiego88@gmail.com ##


setwd("c:/RAnalysis/LinearModel/TemperaturaCraugastor")


# Leemos la base de datos para los analisis 

db_cc = read.csv("Ccrassidigitus.csv", sep=",")

head(db_cc)

summary(db_cc)

# Indicamos como variables categoricas la Zona, el sustrato y la actividad

db_cc$Zona = as.factor(db_cc$Zona)

db_cc$Sustrato = as.factor(db_cc$Sustrato)

db_cc$Actividad = as.factor(db_cc$Actividad)

# Realizamos un análisis de multicolinearidad, valores de VIF mayores a 3 pueden ser problematicos

library(fuzzySim) 

multicol(db_cc[ , c(6, 9, 10, 11)])

    ##Resultado##
    #               Rsquared Tolerance      VIF
    # Ta_ambiente 0.313683212 0.6863168 1.457053
    # Ta_sustrato 0.313363140 0.6866369 1.456374
    # LRC         0.039114968 0.9608850 1.040707
    # Alt_Suelo   0.002516955 0.9974830 1.002523
    ##

# CORRELACIÓN DE KENDALL #

x=(db_cc$Tiempo)
y=(db_cc$Ta_corporal)

cor.test(x, y, method = "kendall", alternative = "greater", conf.level = 0.95)
    
    #Resultado
    #z = -4.4019, p-value = 1
    #alternative hypothesis: true tau is greater than 0
    #sample estimates:
    #  tau 
    #-0.3684332 
    ##

# MODELOS LINEALES #

lm. = lm( Ta_corporal ~ 1, data=db_cc )

lm1 = lm( Ta_corporal ~ Zona, data=db_cc )

lm2 = lm( Ta_corporal ~ Alt_Suelo, data=db_cc )

lm3 = lm( Ta_corporal ~ Sustrato, data=db_cc )

lm4 = lm( Ta_corporal ~ Ta_sustrato, data=db_cc )

lm5 = lm( Ta_corporal ~ Ta_ambiente, data=db_cc )

lm6 = lm( Ta_corporal ~ LRC, data=db_cc )

lm7 = lm( Ta_corporal ~ Actividad, data=db_cc )

lm8 = lm( Ta_corporal ~ Zona+Alt_Suelo, data=db_cc )

lm9 = lm( Ta_corporal ~ Zona+Sustrato, data=db_cc )

lm10 = lm( Ta_corporal ~ Zona+Ta_sustrato, data=db_cc )

lm11 = lm( Ta_corporal ~ Zona+Ta_ambiente, data=db_cc )

lm12 = lm( Ta_corporal ~ Zona+LRC, data=db_cc )

lm13 = lm( Ta_corporal ~ Zona+Actividad, data=db_cc )

lm14 = lm( Ta_corporal ~ Alt_Suelo+Sustrato, data=db_cc )

lm15 = lm( Ta_corporal ~ Alt_Suelo+Ta_sustrato, data=db_cc )

lm16 = lm( Ta_corporal ~ Alt_Suelo+Ta_ambiente, data=db_cc )

lm17 = lm( Ta_corporal ~ Alt_Suelo+LRC, data=db_cc )

lm18 = lm( Ta_corporal ~ Alt_Suelo+Actividad, data=db_cc )

lm19 = lm( Ta_corporal ~ Sustrato+Ta_sustrato, data=db_cc )

lm20 = lm( Ta_corporal ~ Sustrato+Ta_ambiente, data=db_cc )

lm21 = lm( Ta_corporal ~ Sustrato+LRC, data=db_cc )

lm22 = lm( Ta_corporal ~ Sustrato+Actividad, data=db_cc )

lm23 = lm( Ta_corporal ~ Ta_sustrato+Ta_ambiente, data=db_cc )

lm24 = lm( Ta_corporal ~ Ta_sustrato+LRC, data=db_cc )

lm25 = lm( Ta_corporal ~ Ta_sustrato+Actividad, data=db_cc )

lm26 = lm( Ta_corporal ~ Ta_ambiente+LRC, data=db_cc )

lm27 = lm( Ta_corporal ~ Ta_ambiente+Actividad, data=db_cc )

lm28 = lm( Ta_corporal ~ LRC+Actividad, data=db_cc )

lm29 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato, data=db_cc )

lm30 = lm( Ta_corporal ~ Zona+Alt_Suelo+Ta_sustrato, data=db_cc )

lm31 = lm( Ta_corporal ~ Zona+Alt_Suelo+Ta_ambiente, data=db_cc )

lm32 = lm( Ta_corporal ~ Zona+Alt_Suelo+LRC, data=db_cc )

lm33 = lm( Ta_corporal ~ Zona+Alt_Suelo+Actividad, data=db_cc )

lm34 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato, data=db_cc )

lm35 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_ambiente, data=db_cc )

lm36 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+LRC, data=db_cc )

lm37 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Actividad, data=db_cc )

lm38 = lm( Ta_corporal ~ Sustrato+Ta_sustrato+Ta_ambiente, data=db_cc )

lm39 = lm( Ta_corporal ~ Sustrato+Ta_sustrato+LRC, data=db_cc )

lm40 = lm( Ta_corporal ~ Sustrato+Ta_sustrato+Actividad, data=db_cc )

lm41 = lm( Ta_corporal ~ Ta_sustrato+Ta_ambiente+LRC, data=db_cc )

lm42 = lm( Ta_corporal ~ Ta_sustrato+Ta_ambiente+Actividad, data=db_cc )

lm43 = lm( Ta_corporal ~ Ta_ambiente+LRC+Actividad, data=db_cc )

lm44 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato, data=db_cc )

lm45 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_ambiente, data=db_cc )

lm46 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+LRC, data=db_cc )

lm47 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Actividad, data=db_cc )

lm48 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente, data=db_cc )

lm49 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato+LRC, data=db_cc )

lm50 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato+Actividad, data=db_cc )

lm51 = lm( Ta_corporal ~ Sustrato+Ta_sustrato+Ta_ambiente+LRC, data=db_cc )

lm52 = lm( Ta_corporal ~ Sustrato+Ta_sustrato+Ta_ambiente+Actividad, data=db_cc )

lm53 = lm( Ta_corporal ~ Ta_sustrato+Ta_ambiente+LRC+Actividad, data=db_cc )

lm54 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente, data=db_cc )

lm55 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato+LRC, data=db_cc )

lm56 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato+Actividad, data=db_cc )

lm57 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente+LRC, data=db_cc )

lm58 = lm( Ta_corporal ~ Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente+Actividad, data=db_cc )

lm59 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente+LRC, data=db_cc )

lm60 = lm( Ta_corporal ~ Zona+Alt_Suelo+Sustrato+Ta_sustrato+Ta_ambiente+Actividad, data=db_cc )

# Evaluamos el modelo con mejor ajuste de acuerdo al criterio AICc

library(AICcmodavg)

Cand.models <- list(lm., lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12, lm13,
                    lm14, lm15, lm16, lm17, lm18, lm19, lm20, lm21, lm22, lm23, lm24,
                    lm26, lm27, lm28, lm29, lm30, lm31, lm32, lm33, lm34, lm35, lm36, lm37,
                    lm38, lm39, lm40, lm41, lm42, lm43, lm44, lm45, lm46, lm47, lm48, lm49,
                    lm50, lm51, lm52, lm53, lm54, lm55, lm56, lm57, lm58, lm59, lm60)
Modnames <- c("lm.", "lm1", "lm2", "lm3", "lm4", "lm5", "lm6", "lm7", "lm8", "lm9", "lm10", 
              "lm11", "lm12", "lm13", "lm14", "lm15", "lm16", "lm17", "lm18", "lm19", "lm20",
              "lm21", "lm22", "lm23", "lm24", "lm26", "lm27", "lm28", "lm29", "lm30", "lm31",
              "lm32", "lm33", "lm34", "lm35", "lm36", "lm37", "lm38", "lm39", "lm40", "lm41",
              "lm42", "lm43", "lm44", "lm45", "lm46", "lm47", "lm48", "lm49", "lm50", "lm51",
              "lm52", "lm53", "lm54", "lm55", "lm56", "lm57", "lm58", "lm59", "lm60")

print(aictab(cand.set = Cand.models, modnames = Modnames,
             second.ord = TRUE), digits = 4)

    ##Resultados##
    #      K     AICc Delta_AICc AICcWt Cum.Wt        LL
    #lm4   3  -6.8492     0.0000 0.2239 0.2239    6.6121
    #lm24  4  -6.5137     0.3355 0.1893 0.4132    7.7013
    #lm23  4  -4.7455     2.1037 0.0782 0.4915    6.6902
    #lm15  4  -4.6395     2.2097 0.0742 0.5656    6.6372
    #lm10  4  -4.6211     2.2281 0.0735 0.6391    6.6280
    #lm19  5  -4.2850     2.5642 0.0621 0.7012    7.6264
    #lm41  5  -4.0640     2.7852 0.0556 0.7569    7.7138
    #lm39  6  -3.8851     2.9641 0.0509 0.8077    8.9193
    #lm42  6  -2.6946     4.1546 0.0280 0.8358    8.0358
    #lm30  5  -2.3441     4.5051 0.0235 0.8593    6.6559
    #lm49  7  -2.2331     4.6161 0.0223 0.8816    9.4499
    #lm34  6  -2.1412     4.7080 0.0213 0.9029    7.7591
    #lm38  6  -2.1190     4.7302 0.0210 0.9239    7.7480
    #lm40  7  -1.2112     5.6380 0.0134 0.9373    8.5390
    #lm51  7  -1.1727     5.6766 0.0131 0.9504    8.9197
    #lm53  7  -0.9793     5.8699 0.0119 0.9623    8.8230
    #lm48  7   0.1307     6.9799 0.0068 0.9691    7.8680
    #lm44  7   0.3347     7.1840 0.0062 0.9753    7.7660
    #lm55  8   0.5728     7.4220 0.0055 0.9807    9.4697
    #lm57  8   0.5916     7.4408 0.0054 0.9862    9.4603
    #lm52  8   1.1478     7.9970 0.0041 0.9903    8.6464
    #lm50  8   1.1984     8.0476 0.0040 0.9943    8.6211
    #lm54  8   2.6489     9.4981 0.0019 0.9962    7.8959
    #lm59  9   3.5120    10.3612 0.0013 0.9975    9.4940
    #lm58  9   3.6846    10.5338 0.0012 0.9986    8.7094
    #lm56  9   3.8208    10.6700 0.0011 0.9997    8.6413
    #lm60 10   6.3566    13.2058 0.0003 1.0000    8.7515
    #lm26  4 186.0882   192.9375 0.0000 1.0000  -88.5997
    #lm43  6 186.7274   193.5766 0.0000 1.0000  -86.3869
    #lm46  7 188.8009   195.6501 0.0000 1.0000  -86.0671
    #lm21  5 194.4084   201.2577 0.0000 1.0000  -91.5224
    #lm12  4 195.9220   202.7712 0.0000 1.0000  -93.5166
    #lm36  6 196.9549   203.8041 0.0000 1.0000  -91.5007
    #lm32  5 198.0990   204.9482 0.0000 1.0000  -93.3677
    #lm6   3 201.5860   208.4352 0.0000 1.0000  -97.5321
    #lm28  5 203.5466   210.3959 0.0000 1.0000  -96.0915
    #lm17  4 203.9157   210.7649 0.0000 1.0000  -97.5134
    #lm45  7 237.7549   244.6041 0.0000 1.0000 -110.9441
    #lm11  4 241.6525   248.5018 0.0000 1.0000 -116.5088
    #lm31  5 243.8860   250.7352 0.0000 1.0000 -116.4591
    #lm20  5 244.3721   251.2213 0.0000 1.0000 -116.7022
    #lm35  6 246.6534   253.5026 0.0000 1.0000 -116.6382
    #lm27  5 247.3983   254.2475 0.0000 1.0000 -118.2153
    #lm5   3 247.4136   254.2629 0.0000 1.0000 -120.5193
    #lm16  4 249.6568   256.5061 0.0000 1.0000 -120.5110
    #lm9   5 264.0899   270.9391 0.0000 1.0000 -126.5611
    #lm22  6 265.2815   272.1308 0.0000 1.0000 -125.9522
    #lm29  6 266.3070   273.1562 0.0000 1.0000 -126.4650
    #lm47  8 266.3271   273.1763 0.0000 1.0000 -123.9432
    #lm37  7 267.7709   274.6201 0.0000 1.0000 -125.9521
    #lm3   4 269.9352   276.7844 0.0000 1.0000 -130.6501
    #lm14  5 271.6255   278.4747 0.0000 1.0000 -130.3289
    #lm1   3 272.8893   279.7385 0.0000 1.0000 -133.2571
    #lm8   4 275.0885   281.9377 0.0000 1.0000 -133.2268
    #lm.   2 276.0981   282.9473 0.0000 1.0000 -135.9567
    #lm13  5 276.3967   283.2460 0.0000 1.0000 -132.7145
    #lm7   4 277.6568   284.5061 0.0000 1.0000 -134.5110
    #lm33  6 278.2391   285.0884 0.0000 1.0000 -132.4310
    #lm2   3 278.2789   285.1281 0.0000 1.0000 -135.9519
    #lm18  5 279.1480   285.9972 0.0000 1.0000 -134.0901
    ##

# Se prueban los supuestos de relación lineal, distribución normal y homocedasticidad
# de manera visual

par(mfrow = c(2, 2))
plot(lm24)

par(mfrow = c(2, 2))
plot(lm4)

# Obtenemos las estimaciones de los mejores modelos y graficamos 
summary.lm(lm4)

confint(lm4)

ggplot(lm4,aes(Ta_corporal,Ta_sustrato))+geom_point()+geom_smooth(method="lm")


summary.lm(lm24)

confint(lm24)

ggplot(lm24,aes(Ta_sustrato, Ta_corporal,color=LRC))+geom_point()+stat_smooth(method="lm",se=FALSE, bg = 'white')

# Realizamos un promedio los dos modelos con mejor ajuste

Cand.models.avg <- list(lm24, lm4)
Modnames <- c("lm24", "lm4")

print(aictab(cand.set = Cand.models.avg, modnames = Modnames,
             second.ord = TRUE), digits = 4)

modavg(parm = "Ta_sustrato", cand.set = Cand.models.avg, modnames = Modnames,
       second.ord = TRUE, nobs = NULL, uncond.se = "revised",
       conf.level = 0.95)