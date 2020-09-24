data("stackloss")

#Operatii de statistica descriptiva
#MEDIA
print(sprintf("Media variabilei Air Flow este: %f", mean(stackloss$Air.Flow)))
print(sprintf("Media variabilei Water Temperature este: %f", mean(stackloss$Water.Temp)))
print(sprintf("Media variabilei Acid Concentation este: %f", mean(stackloss$Acid.Conc.)))
print(sprintf("Media variabilei Stack Loss este: %f", mean(stackloss$stack.loss)))

#VARIANTA
print(sprintf("Varianta variabilei Air Flow este: %f", var(stackloss$Air.Flow)))
print(sprintf("Varianta variabilei Water Temperature este: %f", var(stackloss$Water.Temp)))
print(sprintf("Varianta variabilei Acid Concentation este: %f", var(stackloss$Acid.Conc.)))
print(sprintf("Varianta variabilei Stack Loss este: %f", var(stackloss$stack.loss)))

#QUARTILELE
#print(sprintf("Quartilele aferente variabilei Air Flow sunt: "))
quantile(stackloss$Air.Flow, probs = c(.25, .5, .75)) 
#print(sprintf("Quartilele aferente variabilei Water Temperature sunt: "))
quantile(stackloss$Water.Temp, probs = c(.25, .5, .75)) 
#print(sprintf("Quartilele aferente variabilei Acid Concentation sunt: "))
quantile(stackloss$Acid.Conc, probs = c(.25, .5, .75)) 
#print(sprintf("Quartilele aferente variabilei Stack Loss sunt: "))
quantile(stackloss$stack.loss, probs = c(.25, .5, .75)) 
 
#BOX PLOT PENTRU FIECARE DINTRE CELE 4 VARIABILE
par(mfrow = c(1,4))
outlier_AirFlow <- boxplot.stats(stackloss$Air.Flow)$out
outlier_AirFlow <- paste(outlier_AirFlow,collapse = " ")
boxplot(stackloss$Air.Flow, main="Box Plot Air Flow", sub=paste("Outlier rows: ", outlier_AirFlow)) 
boxplot(stackloss$Water.Temp, main="Box Plot Water Temp.", sub=paste("Outlier rows: - ", boxplot.stats(stackloss$Water.Temp)$out))  
boxplot(stackloss$Acid.Conc, main="Box Plot Acid Conc.", sub=paste("Outlier rows: - ", boxplot.stats(stackloss$Acid.Conc)$out)) 

outlier_StackLoss <- boxplot.stats(stackloss$stack.loss)$out
outlier_StackLoss <- paste(outlier_StackLoss,collapse = " ")
boxplot(stackloss$stack.loss, main="Box Plot Stack Loss", sub=paste("Outlier rows: ", outlier_StackLoss))  

#SCATTER PLOT PENTRU FIECARE RELATIE LINIARA DINTRE VARIABILELE INDEPENDENTE(Air Flow , Water Temperature, Acid Concentration) SI VARIABILA DEPENDENTA(Stack Loss)
par(mfrow = c(1,3))
scatter.smooth(x = stackloss$Air.Flow , y = stackloss$stack.loss, main = "Scatter Plot", xlab = "Air Flow", ylab = "Stack Loss")
scatter.smooth(x = stackloss$Water.Temp , y = stackloss$stack.loss, main = "Scatter Plot", xlab = "Water Temperature", ylab = "Stack Loss")
scatter.smooth(x = stackloss$Acid.Conc , y = stackloss$stack.loss, main = "Scatter Plot", xlab = "Acid Concentration", ylab = "Stack Loss")

#DENSITY PLOT PENTRU FIECARE DINTRE CELE 4 VARIABILE
par(mfrow = c(1,4))
library(e1071)
plot(density(stackloss$Air.Flow), main=paste("Density Plot:", "Air Flow"), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(stackloss$Air.Flow), 2)))
polygon(density(stackloss$Air.Flow), col="blue")
plot(density(stackloss$Water.Temp), main=paste("Density Plot:", "Water Temperature"), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(stackloss$Water.Temp), 2)))
polygon(density(stackloss$Water.Temp), col="blue")
plot(density(stackloss$Acid.Conc), main=paste("Density Plot:", "Acid Concentration"), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(stackloss$Acid.Conc.), 2)))
polygon(density(stackloss$Acid.Conc), col="blue")
plot(density(stackloss$stack.loss), main=paste("Density Plot:", "Stack Loss"), ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(stackloss$stack.loss), 2)))
polygon(density(stackloss$stack.loss), col="blue")


