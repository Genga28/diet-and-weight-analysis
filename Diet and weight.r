#Converting csv into table
diet<-read.csv(file.choose(),header=TRUE)
#Adding weight.Loss column
diet$weight.loss = diet$Initialweight - diet$Finalweight
diet$Diet.type   = factor(diet$Diet.type,levels=c("A","B","C"))

#arrange datas
diet$Gender= factor(diet$Gender,levels=c("female","male"))

#graph
boxplot(weight.loss~Diet.type,data=diet,col="blue",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="red")


#fisher,welch,kruskal
diet.fisher  = aov(weight.loss~Diet.type,data=diet)
diet.welch   = oneway.test(weight.loss~Diet.type,data=diet)
diet.kruskal = kruskal.test(weight.loss~Diet.type,data=diet)

summary(diet.fisher)
print(diet.welch)
print(diet.kruskal)


#Two sample t-test
summary(aov(weight.loss~Diet.type,data=diet[diet$Diet.type!="B",]))
t.test(weight.loss~Diet.type,data=diet[diet$Diet.type!="B",],var.equal = TRUE)

#Model test
mean_group   = tapply(diet$weight.loss,diet$Diet.type,mean)
median_group = tapply(diet$weight.loss,diet$Diet.type,median)
mean_group
median_group



#2 way annova
diet.fisher = aov(weight.loss~Diet.type*Gender,data=diet)
summary(diet.fisher)



