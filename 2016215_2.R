#Question 2

WithoutBreakfastList=c(6,5,5,4,7,7,7,5,6,5)
WithBreakfastList=c(8,7,9,5,9,8,10,7,6,9)
SizeOfDataset=length(WithBreakfastList)

#T Critical Value
for(i in 0:10){
  WithBreakfastList[i]=WithBreakfastList[i]-WithoutBreakfastList[i]
}
Difference=WithBreakfastList
DegreeOfFreedom=SizeOfDataset-1
A=0.05 #Alpha
Tcritical=qt(A,DegreeOfFreedom,lower.tail=FALSE)

#Mean and Standard Deviation
m=mean(Difference)
std=sqrt(var(Difference))

#T Statistics
pm=2 #Population Mean
Tstat=(m-pm)/(std/sqrt(SizeOfDataset))

#T P-value
Tpvalue=1-pt((Tstat),DegreeOfFreedom,lower.tail = TRUE)

cat('T Critical Value: ',Tcritical)
cat('T Statistics: ',Tstat)
cat('T P-value: ',Tpvalue)