#Decision Trees

#Understanding decision trees - Entropy example

#Calculating the entropy between two classes
#Where the dataset is split between red (60%) and white (40%)
-0.60 * log2(0.60) - 0.40 * log2(0.40)

#Visualising entropy levels for all two-class arrangements using curve function
curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "entropy", lwd = 4)

#