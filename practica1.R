install.packages("insuranceData")
install.packages("dplyr")
install.packages("skimr")
install.packages("visdat")
install.packages("ggplot2")
library(insuranceData)
library(dplyr)
library(skimr)
library(visdat)
library(ggplot2)

#activar el data frame de dataCar
data(dataCar)
#para conocer la estructura del data frame
str(dataCar)
summary(dataCar) #resumen
skim(dataCar) #puede decir si tiene valores faltante 
head(dataCar) #conocer el encabezado del data frame (solo los primeros 7 valores)
tail(dataCar) #contrario al anterior on este puedo ver los ulimos valores

dim(dataCar)#dimension del data frame

glimpse(dataCar)#podemos observar como esta definida   cada varible

colnames(dataCar)# para conocer el nombre de las variables

any(is.na(dataCar)) #para saber si hay datos faltantes, dice cuantos
vis_dat(dataCar)#
vis_miss(dataCar)

#QUE PORCENTJE DE POLIZAS INCURREN EN AL MENOS UN RECLMACION
(sum(dataCar $numclaims!=0)/nrow(dataCar))*100

#TOP 5 VEHICULOS CON MAYOR NUMERO DE RECLAMACIONES

masreclamaciones <- dataCar %>%
  group_by(veh_body) %>%  # Agrupar por tipo de vehículo
  summarise(total_reclamaciones=sum(numclaims, (na.rm=TRUE))) %>%  # Sumar reclamaciones
  arrange(desc(total_reclamaciones)) %>%  # Ordenar de mayor a menor
  head(5)  

print(masreclamaciones)

#TOP 10 MONTO DE RECLAMACION

m_montoreclamacion <- dataCar %>%
  group_by(veh_body) %>%  # Agrupar por tipo de vehículo
  summarise(total_reclamaciones = sum(claimcst0, na.rm = TRUE)) %>%  # Sumar reclamaciones
  arrange(desc(total_reclamaciones)) %>%  # Ordenar de mayor a menor
  head(10)  

print(m_montoreclamacion)


#TOP5 DE VEHICULOS CON MAYOR NUMERO DE RECLAMACIONES GRAFICA 
ggplot(masreclamaciones,aes(x=reorder(veh_body,-total_reclamaciones),y=total_reclamaciones))+
  geom_bar(stat = "identity",fill="pink",color="purple")+
  labs(title = "Numero de reclamaciones por tipo de vehiculo",
       x="tipo de vehiculo",
       y="total de reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1))

#TOP 10 DE VEHICULOS CON MAYOR MONTO DE RECLAMACIONES GRAFICA
ggplot(m_montoreclamacion,aes(x=reorder(veh_body,-total_reclamaciones),y=total_reclamaciones))+
  geom_bar(stat = "identity",fill="green",color="blue")+
  labs(title = "Vehiculos con mayor monto de reclamacion",
       x="tipo de vehiculo",
       y="total de reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1))




#ANALISIS DE ACUERDO AL GENERO
##################################################################################
reclamaciones_genero <- dataCar %>%
  group_by(gender) %>%
  summarise(
    total_policies = n(),  # Total de pólizas por género
    policies_with_claims = sum(numclaims > 0),  # Pólizas con al menos una reclamación
    percentage_claims = (policies_with_claims / total_policies) * 100  # Porcentaje
  )

print(reclamaciones_genero)

# Gráfica de comparación
ggplot(reclamaciones_genero, aes(x = genero, y = percentage_claims, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Porcentaje de Pólizas con Reclamaciones por Género",
       x = "Género",
       y = "Porcentaje de Pólizas con Reclamaciones") +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink"))
##################################################################################

claims_veh_gen<- dataCar%>%
  group_by(gender, veh_body)%>%
  summarise(total_reclamaciones=sum(numclaims))%>%
  arrange(desc(total_reclamaciones))
print(claims_veh_gen)

#crear el grafico de barras
ggplot(claims_veh_gen,aes(x=reorder(veh_body,-total_reclamaciones),y=total_reclamaciones, fill=gender))+
  geom_bar(stat = "identity", position="dodge" )+
  labs(title = "Tipos de vehiculo con mas reclamacione por genero",
       x="tipo de vehiculo",
       y="Numero de reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1))
