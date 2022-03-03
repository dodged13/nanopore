barcode_1 = read.delim("KatsPractical_nanopore/treatment/treatment_bc1.tsv")
barcode_2 = read.delim("KatsPractical_nanopore/treatment/treatment_bc2.tsv")
barcode_3 = read.delim("KatsPractical_nanopore/treatment/treatment_bc3.tsv")
barcode_4 = read.delim("KatsPractical_nanopore/treatment/treatment_bc4.tsv")
barcode_unnkown = read.delim("KatsPractical_nanopore/treatment/treatment_unknown.tsv")

write.table(barcode_1$ReadID[barcode_1$Confidence.Interval > 0.95], file = "barcode_1_treatment_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(barcode_2$ReadID[barcode_2$Confidence.Interval > 0.95], file = "barcode_2_treatment_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(barcode_3$ReadID[barcode_3$Confidence.Interval > 0.95], file = "barcode_3_treatment_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(barcode_4$ReadID[barcode_4$Confidence.Interval > 0.95], file = "barcode_4_treatment_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(barcode_unknown$ReadID[barcode_unknown$Confidence.Interval > 0.95], file = "barcode_unknown_treatment_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)

#code total reads barplot

tfbc1 = summary(barcode_1$Confidence.Interval > 0.95)
tfbc2 = summary(barcode_2$Confidence.Interval > 0.95)
tfbc3 = summary(barcode_3$Confidence.Interval > 0.95)
tfbc4 = summary(barcode_4$Confidence.Interval > 0.95)
tfbcunknown = summary(barcode_unknown$X0.0478 > 0.95)  

tnrate = data.frame("less than 95%" = c(tfbc1[2],tfbc2[2],tfbc3[2],tfbc4[2],tfbcunknown[2]), "greater than 95%" = c(tfbc1[3],tfbc2[3],tfbc3[3],tfbc4[3],tfbcunknown[3]))
rownames(tnrate) = c("Barcode_1","Barcode 2","Barcode 3","Barcode 4","Barcode unknown")
tnrate2 = data.frame(x=as.numeric(c(tnrate$less.than.95.,tnrate$greater.than.95.)))
tnrate2$Barcodes = c(rownames(tnrate),rownames(tnrate))
tnrate2[1:5,3] = "Less than 95%"
tnrate2[6:10,3] = "Greater than 95%"
colnames(tnrate2)[3] = "Confidence_interval"
colnames(tnrate2)[1] = "Reads"
library(ggplot2)
ggplot(data = tnrate2) + geom_bar(aes(y=x,x=Barcodes,fill=Confidence_interval),stat="identity", position = "dodge") + theme_minimal() + labs(y="Reads")


##controls
control_barcode_1 = read.delim("KatsPractical_nanopore/controls/control_bc1.tsv")
control_barcode_2 = read.delim("KatsPractical_nanopore/controls/control_bc2.tsv")
control_barcode_3 = read.delim("KatsPractical_nanopore/controls/control_bc3.tsv")
control_barcode_4 = read.delim("KatsPractical_nanopore/controls/control_bc4.tsv")
control_barcode_unnkown = read.delim("KatsPractical_nanopore/controls/control_unknown.tsv")

write.table(control_barcode_1$ReadID[control_barcode_1$Confidence.Interval > 0.95], file = "barcode_1_control_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(control_barcode_2$ReadID[control_barcode_2$Confidence.Interval > 0.95], file = "barcode_2_control_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(control_barcode_3$ReadID[control_barcode_3$Confidence.Interval > 0.95], file = "barcode_3_control_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(control_barcode_4$ReadID[control_barcode_4$Confidence.Interval > 0.95], file = "barcode_4_control_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(control_barcode_unknown$ReadID[control_barcode_unknown$Confidence.Interval > 0.95], file = "barcode_unknown_control_Filtered.tsv", row.names = FALSE, quote = FALSE, col.names = FALSE)}

#total reads barcode

cfbc1 = summary(control_barcode_1$Confidence.Interval > 0.95)
cfbc2 = summary(control_barcode_2$Confidence.Interval > 0.95)
cfbc3 = summary(control_barcode_3$Confidence.Interval > 0.95)
cfbc4 = summary(control_barcode_4$Confidence.Interval > 0.95)
cfbcunknown = summary(control_barcode_unknown$X0.0478 > 0.95)  

cnrate = data.frame("less than 95%" = c(cfbc1[2],cfbc2[2],cfbc3[2],cfbc4[2],tfbcunknown[2]), "greater than 95%" = c(cfbc1[3],cfbc2[3],cfbc3[3],cfbc4[3],cfbcunknown[3]))
rownames(cnrate) = c("Barcode_1","Barcode 2","Barcode 3","Barcode 4","Barcode unknown")
cnrate2 = data.frame(x=as.numeric(c(cnrate$less.than.95.,cnrate$greater.than.95.)))
cnrate2$Barcodes = c(rownames(cnrate),rownames(cnrate))
cnrate2[1:5,3] = "Less than 95%"
cnrate2[6:10,3] = "Greater than 95%"
colnames(cnrate2)[3] = "Confidence_interval"
colnames(cnrate2)[1] = "Reads"
ggplot(data = cnythrate2) + geom_bar(aes(y=x,x=Barcodes,fill=Confidence_interval),stat="identity", position = "dodge") + theme_minimal() + labs(y="Reads")
