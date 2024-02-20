getwd()
nuevo_dir<- "c:/Practica_4_pedro"
setwd(nuevo_dir)
getwd()
library(readxl)

#Ejercicio 1
library(readxl)
spear <- read_excel("C:/Practica_4_pedro/spearheads.xlsx")
data_spear <- as.data.frame(spear)
class(data_spear)

#Ejercicio 2. Renombramos las variables
names(data_spear)[names(data_spear)== "Mat"]<- "Materiales" #Seleccionamos el operador y los corchetes, el doble igual nos compara los valores y los vamos cambiando a los nombres en español
names(data_spear)[names(data_spear)== "Con"]<- "Contexto"
names(data_spear)[names(data_spear)== "Cond"]<- "Conservación"
names(data_spear)[names(data_spear)== "Loo"]<- "Loop"
names(data_spear)[names(data_spear)== "Peg"]<- "Remache"
names(data_spear)[names(data_spear)== "Date"]<- "Fecha"
names(data_spear)[names(data_spear)== "Maxle"]<- "Ancho_max"
names(data_spear)[names(data_spear)== "Losoc"]<- "Longitud_encaje"
names(data_spear)[names(data_spear)== "Mawit"]<- "Ancho_Max_Encaje"
names(data_spear)[names(data_spear)== "Weight"]<- "Peso"
names(data_spear)[names(data_spear)== "Upsoc"]<- "Ancho_Encaje"

#Ejercicio 3
data_spear$Contexto=factor(data_spear$Contexto, levels=c('1','2','3'), labels=c("s/c", "Habitacional", "Funerario"))
data_spear$Conservación=factor(data_spear$Conservación, levels=c('1','2','3','4'), labels=c("Excelente", "Bueno", "Regular", "Malo"))
data_spear$Remache=factor(data_spear$Remache, levels=c('1','2'), labels=c("Si", "No"))
data_spear$Materiales=factor(data_spear$Materiales, levels=c('1','2'), labels=c("Bronce", "Hierro"))

#Ejercicio 4
tabla_mat<- table(data_spear$Materiales)
View(tabla_mat)
tabla_Contexto<- table(data_spear$Contexto)
View(tabla_Contexto)
tabla_cons <- table(data_spear$Conservación)
View(tabla_cons)

#Ejercicio 5
tabla_cruz_Mat_Contex <- table(data_spear$Materiales, data_spear$Contexto)
View(tabla_cruz_Mat_Contex)
print(tabla_cruz_Mat_Contex)
tabla_cruz_Mat_Cons <- table(data_spear$Materiales, data_spear$Conservación)
View(tabla_cruz_Mat_Cons)
print(tabla_cruz_Mat_Cons)

#Ejercicio 6
tabla_freqs <- table(data_spear$Materiales, data_spear$Contexto,data_spear$Conservación)
tabla_porcentajes <- prop.table(tabla_freqs *100)
print(tabla_porcentajes)
View(tabla_porcentajes)

#Ejercicio 7
tabla_cruzada_contexto <- xtabs(~ Materiales + Contexto, data = data_spear)
porcentajes_contexto <- prop.table(tabla_cruzada_contexto *100)

tabla_cruzada_conservacion <- xtabs(~ Materiales + Conservación, data = data_spear)
porcentajes_conservacion <- prop.table(tabla_cruzada_conservacion* 100)
print(porcentajes_conservacion)
print(porcentajes_contexto)
View(porcentajes_contexto)

#Ejercicio 8
graficos_cons <- ggplot(data_spear, aes(x=Conservación)) + geom_bar() + labs(title="Frecuencia de Conservación", x="Conservación", y="Frecuencia")
print(graficos_cons)

graficos_context <- ggplot(data_spear, aes(x=Contexto))+geom_bar()+ labs(title="Frecuencia de Contexto",x="Contexto",y="Frecuencia")
print(graficos_context)

#Ejercicio 9
grafico_mat <- ggplot(data_spear,aes(x=Materiales))+ geom_bar()+coord_flip()+ labs(title= "Frecuencia de materiales", x="Materiales",y="Frecuencia")
print(grafico_mat)

grafico_rem <- ggplot(data_spear,aes(x=Remache))+ geom_bar()+coord_flip()+ labs(title= "Frecuencia de Remaches", x="Remaches",y="Frecuencia")
print(grafico_rem)

#Ejercicio 10
graf_barras_mat_Cons <- ggplot(data_spear,aes(x=Conservación,fill=Materiales))+ geom_bar(position="dodge",stat="count")+labs(title = "Frecuencia de Conservación por Material",x="Conservación",y="Frecuencia")+
scale_fill_manual(values=c("A"="blue","B"="red","C"="green"))
print(graf_barras_mat_Cons)

#Ejercicio 11
#Porcentaje de cada factor
porcentajes <- prop.table(table(data_spear$Conservación))*100
print(porcentajes)

#Crear dataframe para el gráfico de sectores
df_porcentajes <- data.frame(Conservación=names(porcentajes),Porcentaje=porcentajes)

#Gráfico de sectores para la variable Conservación
ggplot(df_porcentajes,aes(x="",y=porcentajes,fill=Conservación))+geom_bar(stat="identity",width=1)+coord_polar("y")+
  labs(title="Porcentaje de Conservación",fill="Conservación")+theme_void()

#Ejercicio 12
#Gráfico de histograma de probabilidad de las variables continuas
graf_hist <- ggplot(melt(data_spear),aes(x=value,fill=variable))+geom_histogram(binwidth=10,color="black",alpha=0.7,position="identity")+
labs(title="Histograma Conjunto de Variables Continuas", x="Valor",y="Frecuencia")+
  facet_wrap(~variable, scales = "free")
print(graf_hist)
