# Visualising the SOM


#### INPUT: CHOOSE THE BEST PERFORMING SOM AND DISPLAY ####
system.time(
  ssom <- supersom(trDat, grid = somgrid(5, 8, "hexagonal"))
)
system.time(
  ssom.pred <- predict(ssom, newdata = tstDat)
)

## Create Original Master SOM
acc5  <- doSOM(trDat,tstDat, 5, 8) ## outputs list(SOM, somperf)
# save the prediction outputs
pred_outputs <- acc5$somperf
#write.csv(ptab, "COnfusionMatrix_NonverlapRandom.csv", row.names = FALSE)

MSOM <- acc5$SOM

#make the plot
colsch <- colorRampPalette(c("#A6CEE3" ,"#1F78B4" ,"#B2DF8A" ,"#33A02C", "#FB9A99" ,"#E31A1C", "#FDBF6F" ,"#FF7F00" ,"#CAB2D6", "#6A3D9A", "#FFFF99"))
cols2<-brewer.pal(11, 'Set3')
plot(MSOM, heatkey = TRUE, palette.name=colsch, type = "codes", shape = "straight", ncolors = 11)


#outputting the data for later
save(MSOM, file="MSOM_NonoverlapRandom_5by8.1.rda")
#save(tstDat, file = "tstDat_OverlapRandom.rda")

ssom.pred <- predict(MSOM, newdata = tstDat)
ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)

#Save SOM figure
#SAVE grid figures
save(ptab, file =  "MSOM_5by8.1_Confusion_Matrix.rda")
write.csv(ptab, "MSOM_5by8.1_Confusion_Matrix.csv")
