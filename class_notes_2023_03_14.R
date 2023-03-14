##EmB
##2023-03-14

library(tidyverse)

data1= data.frame(ID= c(1,2),
                  X1=c("a1", "a2"))
head(data1)

data2= data.frame(ID=c(2,3),
                  X2=c("b1", "b2"))
head(data2)

#left_join
data12= left_join(data1, data2, by="ID")
head(data12)

##or with a pipe

data12=data1%>%
  left_join(data2, by="ID")
data12

data12=data1%>%
  right_join(data2, by="ID")
data12

data12=data1%>%
  inner_join(data2)
data12

#inner_join only joins columns 

data12=data1%>%
  full_join(data2)
data12


data12=data1%>%
  semi_join(data2)
data12

data12=data1%>%
  anti_join(data2)
data12

#anti-join is what is missing

#wider vs longer

survey=data.frame(quadrat_id= c(101, 102, 103, 104),
                  barancle_n=c(2,11,8,27),
                  chiton_n=c(1,0,0,2),
                  mussel_n=c(0,1,1,4))
survey

long=survey%>%
  pivot_longer(cols=c("barancle_n", "chiton_n", "mussel_n"),
               names_to="taxon",
               values_to="counts")
long

wide=long%>%
  pivot_wider(names_from=taxon,
              values_from=counts)
wide


##Exercise 1.2

ggplot(data=wide)+
  geom_point(aes(x="quadrat_id", y="barnacle_n", color="red"))+
  geom_point(aes(x="quadrat_id", y="chiton_n", color="green"))+
  geom_point(aes(x="quadrat_id", y="mussel_n",color="blue"))

ggplot(data=long, aes(x=quadrat_id, y=counts, color=taxon))+
  geom_point()

             