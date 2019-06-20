##  Paqueterias necesarias
library(tidyr) #Manipulación de Data Frames
library(dplyr) #Manipulación de Data Frames
library(ggplot2) #Realizar las gráficas
library(rmarkdown) #Llamar a un archivo Rmd

#Leyendo las bases de datos generadas del archivo RMD que imputa y valida las cuatro bases de datos de la ENIGH
#En el caso de leer los archivos del año 2008
#Leyendo la base de datos ya validada
BDV2008<-read.csv("Base completa 2008.csv")
#Poniendo nombre a las variables
names(BDV2008)<-c("Folio de la vivienda","Tamaño de localidad","Ubicación geográfica","Estrato del 
                  CONAPO","Estrato de diseño muestral","Unidad primaria de muestreo","Clase de vivienda particular",
                  "Materiales de paredes","Materiales de techos","Materiales de pisos","Antigüedad de la vivienda",
                  "Cuenta con cuarto de cocina","Número de dormitorios","Número de cuartos","Disponibilidad de agua 
                  dentro de la vivienda","Cuenta con excusado","Cuenta con drenaje","Cuenta con eléctricidad","Renta",
                  "Lavadero","Fregadero","Tinaco en la azotea","Cisterna","Pileta","Calentador","Medidor de luz",
                  "Bomba de agua","Tanque de gas","Aire acondicionado","Calefacción","Factor de expansión","Estado",
                  "Municipio","Año")
#Obteniendo correlaciones
Correlaciones2008<-cor(BDV2008[,c(19,2,13,14,16,17,18,20,21,22,23,24,25,27,28,29,30)] )
write.csv(x =Correlaciones2008,file ="Correlaciones variables ENIGH 2008.csv" )
#Sustrayendo las variables de interés (para la estratificación se extraen de las variables con mayor correlación con los precios de renta)
BDR2008<-BDV2008[,c(1,2,14,19,31)] 
#Creando una variable para el año y otra para el estado
Año<-vector(mode = "numeric",length = as.matrix(dim(BDR2008))[1,1])
Estado<-vector(mode = "integer",length = as.matrix(dim(BDR2008))[1,1])
#Concatenando los vectores a la base
BDR2008<-cbind(Año, Estado,BDR2008) 
#Rellenando los valores de los vectores
for(i in 1:length(Año)){
  BDR2008[i,1]<-2008
}
for(i in 1:length(Año)){
  if(BDR2008[i,3]<100000){
    BDR2008[i,2]<-substring(BDR2008[i,3],first = 1,last = 1)
  }
  if(BDR2008[i,3]>=100000){
    BDR2008[i,2]<-substring(BDR2008[i,3],first = 1,last = 2)
  }
}


#En el caso de leer los archivos del año 2010
#Leyendo la base de datos ya validada
BDV2010<-read.csv("Base completa 2010.csv")
#Poniendo nombre a las variables
names(BDV2010)<-c("Folio de la vivienda","Tamaño de localidad","Ubicación geográfica","Estrato del
                  CONAPO","Estrato de diseño muestral","Unidad primaria de muestreo","Clase de vivienda particular",
                  "Materiales de paredes","Materiales de techos","Materiales de pisos","Antigüedad de la vivienda",
                  "Cuenta con cuarto de cocina","Número de dormitorios","Número de cuartos","Disponibilidad de agua 
                  dentro de la vivienda","Cuenta con excusado","Cuenta con drenaje","Cuenta con eléctricidad","Renta",
                  "Lavadero","Fregadero","Tinaco en la azotea","Cisterna","Pileta","Calentador","Medidor de luz",
                  "Bomba de agua","Tanque de gas","Aire acondicionado","Calefacción","Factor de expansión","Estado",
                  "Municipio","Año")
#Obteniendo correlaciones
Correlaciones2010<-cor(BDV2010[,c(19,2,13,14,16,17,18,20,21,22,23,24,25,27,28,29,30)] )
write.csv(x =Correlaciones2010,file ="Correlaciones variables ENIGH 2010.csv" )
#Sustrayendo las variables de interés (para la estratificación se extraen de las variables con mayor correlación con los precios de renta)
BDR2010<-BDV2010[,c(1,2,14,19,31)] 
#Creando una variable para el año y otra para el estado
Año<-vector(mode = "numeric",length = as.matrix(dim(BDR2010))[1,1])
Estado<-vector(mode = "integer",length = as.matrix(dim(BDR2010))[1,1])
#Concatenando los vectores a la base
BDR2010<-cbind(Año, Estado,BDR2010) 
#Rellenando los valores de los vectores
for(i in 1:length(Año)){
  BDR2010[i,1]<-2010
}
for(i in 1:length(Año)){
  if(BDR2010[i,3]<100000){
    BDR2010[i,2]<-substring(BDR2010[i,3],first = 1,last = 1)}
  if(BDR2010[i,3]>=100000){
    BDR2010[i,2]<-substring(BDR2010[i,3],first = 1,last = 2)}
}

#En el caso de leer el archivo del año 2012
#Leyendo la base de datos ya validada
BDV2012<-read.csv("Base completa 2012.csv")
#Poniendo nombre a las variables
names(BDV2012)<-c("Folio de la vivienda","Tamaño de localidad","Ubicación geográfica","Estrato del
                  CONAPO","Estrato de diseño muestral","Unidad primaria de muestreo","Clase de vivienda particular",
                  "Materiales de paredes","Materiales de techos","Materiales de pisos","Antigüedad de la vivienda",
                  "Cuenta con cuarto de cocina","Número de dormitorios","Número de cuartos","Disponibilidad de agua 
                  dentro de la vivienda","Cuenta con excusado","Cuenta con drenaje","Cuenta con eléctricidad","Renta",
                  "Lavadero","Fregadero","Tinaco en la azotea","Cisterna","Pileta","Calentador","Medidor de luz",
                  "Bomba de agua","Tanque de gas","Aire acondicionado","Calefacción","Factor de expansión","Estado",
                  "Municipio","Año")
#Obteniendo correlaciones
Correlaciones2012<-cor(BDV2012[,c(19,2,13,14,16,17,18,20,21,22,23,24,25,27,28,29,30)] )
write.csv(x =Correlaciones2012,file ="Correlaciones variables ENIGH 2012.csv" )
#Sustrayendo las variables de interés (para la estratificación se extraen de las variables con mayor correlación con los precios de renta)
BDR2012<-BDV2012[,c(1,2,14,19,31)] 
#Creando una variable para el año y otra para el estado
Año<-vector(mode = "numeric",length = as.matrix(dim(BDR2012))[1,1])
Estado<-vector(mode = "integer",length = as.matrix(dim(BDR2012))[1,1])
#Concatenando los vectores a la base
BDR2012<-cbind(Año, Estado,BDR2012) 
#Rellenando los valores de los vectores
for(i in 1:length(Año)){
  BDR2012[i,1]<-2012
}
for(i in 1:length(Año)){
  if(BDR2012[i,3]<100000){
    BDR2012[i,2]<-substring(BDR2012[i,3],first = 1,last = 1)}
  if(BDR2012[i,3]>=100000){
    BDR2012[i,2]<-substring(BDR2012[i,3],first = 1,last = 2)}
}

#En el caso de leer el archivo del año 2014
#Leyendo la base de datos ya validada
BDV2014<-read.csv("Base completa 2014.csv")
#Poniendo nombre a las variables
names(BDV2014)<-c("Folio de la vivienda","Tamaño de localidad","Ubicación geográfica","Estrato del 
                  CONAPO","Estrato de diseño muestral","Unidad primaria de muestreo","Clase de vivienda particular",
                  "Materiales de paredes","Materiales de techos","Materiales de pisos","Antigüedad de la vivienda",
                  "Cuenta con cuarto de cocina","Número de dormitorios","Número de cuartos","Disponibilidad de agua 
                  dentro de la vivienda","Cuenta con excusado","Cuenta con drenaje","Cuenta con eléctricidad","Renta",
                  "Lavadero","Fregadero","Tinaco en la azotea","Cisterna","Pileta","Calentador","Medidor de luz",
                  "Bomba de agua","Tanque de gas","Aire acondicionado","Calefacción","Factor de expansión","Estado",
                  "Municipio","Año")
#Obteniendo correlaciones
Correlaciones2014<-cor(BDV2014[,c(19,2,13,14,16,17,18,20,21,22,23,24,25,27,28,29,30)] )
write.csv(x =Correlaciones2014,file ="Correlaciones variables ENIGH 2014.csv" )
#Volviendo numérica la variable que es factor
BDV2014[,7]<-as.numeric(as.factor(BDV2014[,7]))
BDV2014[,8]<-as.numeric(as.factor(BDV2014[,8]))
BDV2014[BDV2014[,9]==5,9]<-"Techo5"
BDV2014[,9]<-as.numeric(as.factor(BDV2014[,9]))-1
BDV2014[,10]<-as.numeric(as.factor(BDV2014[,10]))

#Sustrayendo las variables de interés (para la estratificación se extraen de las variables con mayor correlación con los precios de renta)
BDR2014<-BDV2014[,c(1,2,14,19,31)] 
#Creando una variable para el año y otra para el estado
Año<-vector(mode = "numeric",length = as.matrix(dim(BDR2014))[1,1])
Estado<-vector(mode = "integer",length = as.matrix(dim(BDR2014))[1,1])
#Concatenando los vectores a la base
BDR2014<-cbind(Año, Estado,BDR2014) 
#Rellenando los valores de los vectores
for(i in 1:length(Año)){
  BDR2014[i,1]<-2014
}
for(i in 1:length(Año)){
  if(BDR2014[i,3]<1000000000){
    BDR2014[i,2]<-substring(BDR2014[i,3],first = 1,last = 1)}
  if(BDR2014[i,3]>=1000000000){
    BDR2014[i,2]<-substring(BDR2014[i,3],first = 1,last = 2)}
}

#En el caso de leer el archivo del año 2016
#Leyendo la base de datos ya validada
BDV2016<-read.csv("Base completa 2016.csv")
#Poniendo nombre a las variables
names(BDV2016)<-c("Folio de la vivienda","Tamaño de localidad","Ubicación geográfica","Estrato del 
                  CONAPO","Estrato de diseño muestral","Unidad primaria de muestreo","Clase de vivienda particular",
                  "Materiales de paredes","Materiales de techos","Materiales de pisos","Antigüedad de la vivienda",
                  "Cuenta con cuarto de cocina","Número de dormitorios","Número de cuartos","Disponibilidad de agua 
                  dentro de la vivienda","Cuenta con excusado","Cuenta con drenaje","Cuenta con eléctricidad","Renta",
                  "Lavadero","Fregadero","Tinaco en la azotea","Cisterna","Pileta","Calentador","Medidor de luz",
                  "Bomba de agua","Tanque de gas","Aire acondicionado","Calefacción","Factor de expansión","Estado",
                  "Municipio","Año")
#Obteniendo correlaciones
Correlaciones2016<-cor(BDV2016[,c(19,2,13,14,16,17,18,20,21,22,23,24,25,27,28,29,30)] )
knitr::kable(Correlaciones2016,"latex",caption = "Correlaciones de las Variables ENIGH 2016",digits = 2)
write.csv(x =Correlaciones2016,file ="Correlaciones variables ENIGH 2014.csv" )
#Volviendo numérica la variable que es factor
BDV2016[,7]<-as.numeric(as.factor(BDV2016[,7]))
BDV2016[,8]<-as.numeric(as.factor(BDV2016[,8]))
BDV2016[BDV2016[,9]==5,9]<-"Techo5"
BDV2016[,9]<-as.numeric(as.factor(BDV2016[,9]))-1
BDV2016[,10]<-as.numeric(as.factor(BDV2016[,10]))

#Sustrayendo las variables de interés (para la estratificación se extraen de las variables con mayor correlación con los precios de renta)
BDR2016<-BDV2016[,c(1,2,14,19,31)] 
#Creando una variable para el año y otra para el estado
Año<-vector(mode = "numeric",length = as.matrix(dim(BDR2016))[1,1])
Estado<-vector(mode = "integer",length = as.matrix(dim(BDR2016))[1,1])
#Concatenando los vectores a la base
BDR2016<-cbind(Año, Estado,BDR2016) 
#Rellenando los valores de los vectores
for(i in 1:length(Año)){
  BDR2016[i,1]<-2016
}
for(i in 1:length(Año)){
  if(BDR2016[i,3]<1000000000){
    BDR2016[i,2]<-substring(BDR2016[i,3],first = 1,last = 1)}
  if(BDR2016[i,3]>=1000000000){
    BDR2016[i,2]<-substring(BDR2016[i,3],first = 1,last = 2)}
}

#Cambiando los nombres de las columnas a las bases de datos
names(BDR2008)<-c("Año", "Estado","Prod.Factor.Renta","Tam.Loc","Num.Cuarto",
                  "Renta.Mensual","Factor.Exp")
names(BDR2010)<-c("Año", "Estado","Prod.Factor.Renta","Tam.Loc","Num.Cuarto",
                  "Renta.Mensual","Factor.Exp")
names(BDR2012)<-c("Año", "Estado","Prod.Factor.Renta","Tam.Loc","Num.Cuarto",
                  "Renta.Mensual","Factor.Exp")
names(BDR2014)<-c("Año", "Estado","Prod.Factor.Renta","Tam.Loc","Num.Cuarto",
                  "Renta.Mensual","Factor.Exp")
names(BDR2016)<-c("Año", "Estado","Prod.Factor.Renta","Tam.Loc","Num.Cuarto",
                  "Renta.Mensual","Factor.Exp")
#Uniendo todas las bases de datos
BD<-rbind(BDR2008,BDR2010,BDR2012,BDR2014,BDR2016)
#Verificando que el tipo de datos de cada columna sea leído correctamente
C7<-sapply(BD,class)
class(BD[,2])<-"integer"
#Rellenado la columna de prod. factor. Renta
class(BD[,3])<-"numeric"
for(j in 1:length(BD[,1])){
  BD[j,3]<-as.numeric(BD[j,6]*BD[j,7])
}
#Categorías de la variable de Tamaño de localidad:
#1 <-Urbana-Alta (localidades de 100,000 habitantes o más)
#2 <-Urbana-Media (Localidades de 15,001 a 99,999 habitantes)
#3 <-Urbana-Baja (Localidades de 2,500 a 15,000 habitantes)
#4 <-Rural ( Localidades con menos de 2,500 habitantes)

#Categorizando la variable de número de cuartos:  2<-1 y 2 cuartos,
#3<-3 cuartos, 4<-4 cuartos, 5<-5 o más cuartos
for(j in 1:length(BD[,5])){
  if(BD[j,5]<=2){
    BD[j,5]<-2
  }
  if(BD[j,5]>=5){
    BD[j,5]<-5
  }
}

#Eliminar objetos no necesarios
rm(C1,C2,C3,C4,C5,C6,C7,Año,Estado,BD2008,BDR2008,BDR2010,BDR2012,BDR2014,BD2010,BD2012,BD2014,BDP2014,BDP2012,BDP2010,BDP2008) 

#variables de estratificación: 
#Tamaño de localidad (Estrato en 2008 ; 
#tam_loc en 2010, 2012 y 2014), 
#Número de cuartos

#Vector de estados
Ve<-sort(unique(BD[,2]))
#Cambiando la clase del objeto a un data.frame como lo tenía PV
VE<-as.data.frame(Ve);VE
#Eliminando los archivos ya no necesarios
rm(Ve)

#Vector de Años
VA<-sort(unique(BD[,1]))

#Variables de estratificación
#Tamaño de localidad
TL<-sort(unique(BD[,4]));TL
#Número de cuartos
NC<-sort(unique(BD[,5]));NC

#Verificando la cantidad de observaciones por categoría 
#en tamaño de localidad
table(BD$Tam.Loc)
#Verificando la cantidad de observaciones por categoría 
#en número de cuartos
table(BD$Num.Cuarto)

#Función que obtiene las medias de los precios y las cantidades de viviendas expandidas para los estados en el periodo especificado
Medias.Precio<-function(Datos,Anios,Estado,VE1, VE2,VE3){
  #Datos: Base de datos
  #Anios: Vector de años
  #Estado: Vector de estados
  #VE1: Variable de estratificación 1
  #VE2: Variable de estratificación 2
  #VE3: Variable de estratificación 3
  
  #Matriz Nula donde se guardarán los datos
  medias<-matrix(nrow =length(Anios)*length(t(Estado))*length(VE1)*length(VE2), ncol = 7) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(medias)<-list(NULL,c("Año","Estado","Suma.Prod.Factor.Renta","Tam.Loc","Num.Cuarto","Promedio Expandido Renta.Mensual","Suma.Factor.Exp"))
  Medias<-as.data.frame(medias)
  
  for(k in 1:length(Anios)){
    for(j in 1:length(t(Estado))){
      for (i in 1:length(VE1)){
        for(l in 1:length(VE2)){
          A<-Datos[Datos[,4]==VE1[i],] # 4 por tamaño de localidad. Marco de
          #datos del nombre del i-ésimo tipo de localidad con su respectivo precio 
          A1<-A[A[,2]==Estado[j,1],] # Marco de datos del nombre del i-ésimo tipo de localidad,
          #j-ésimo estado con su respectivo precio 
          A2<-A1[A1[,1]==Anios[k],] # Marco de datos del nombre del i-ésimo tipo de localidad,
          #j-ésimo estado, k-ésimo año con su respectivo precio 
          A3<-A2[A2[,5]==VE2[l],] # Marco de datos del nombre del i-ésimo tipo de localidad, j-ésimo estado, k-ésimo año, l-ésima categoría de número de cuartos con su respectivo precio
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),6]<-(sum(A3[,3])/sum(A3[,7])) #Promedio ponderado expandido
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),1]<-Anios[k]
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),2]<-as.character(Estado[j,1])
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),4]<-as.character(VE1[i])
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),5]<-as.character(VE2[l])
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),3]<-sum(A3[,3])
          Medias[l+length(t(Estado))*length(VE1)*length(VE2)*(k-1)+length(VE1)*length(VE2)*(j-1)+length(VE2)*(i-1),7]<-sum(A3[,7])
          print(Medias) #Matriz de datos
        }
      }
    }
  }
  return(Medias)
}

#Aplicando la función a los datos
Base.datos.Medias.Precios.Estados<-Medias.Precio(Datos=BD, Anios=VA,Estado=VE,VE1=TL, VE2=NC)

#Función que calcula los índices de precios de renta de la vivienda 
#privada por Estado del tipo Paasche
IPRVPfp<-function(MediasP,Anios,Estado,VE1, VE2){
  #Medias: Marco de datos con las medias de los precios
  #Ponderaciones: Marco de datos con las ponderaciones
  #Anios: Vector de años
  #Estado: Vector de estados
  
  #Matriz Nula donde se guardarán los datos
  indices<-matrix(nrow =length(Anios)*length(t(Estado)), ncol = 5) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indices)<-list(NULL,c("Año","ID Estado","Estado","Índice de Precios
                                 de Renta de la Vivienda Privada","Incremento respecto periodo anterior"))
  Indices<-as.data.frame(indices)
  
  for(k in 1:length(Anios)){
    for(j in 1:length(t(Estado))){
      C<-matrix(nrow =length(VE1)*length(VE2), ncol = 3) #Matriz con las 
      #operaciones del cálculo del índice por ciudad
      dimnames(C)<-list(NULL,c("Media del Precio", "Cantidad",
                               "Producto Media.Precio y Cantidad"))
      for (i in 1:length(VE1)){
        for(l in 1:length(VE2)){
          A<-MediasP[MediasP[,2]==Estado[j,1],] # Marco de datos del nombre 
          #del j-ésimo estado con su respectivo precio 
          A1<-A[A[,4]==VE1[i],] # Marco de datos del nombre del i-ésimo tipo
          #de localidad, j-ésimo estado con su respectivo precio 
          PPB1<-A1 #Para obtener las medias del periodo base
          A2<-A1[A1[,1]==Anios[k],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año con su respectivo 
          #precio 
          PPB2<-PPB1[PPB1[,1]=="2008",] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base con su
          #respectivo precio
          A3<-A2[A2[,5]==VE2[l],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año, l-ésima categoría
          #de número de cuartos con su respectivo precio
          PPB3<-PPB2[PPB2[,5]==VE2[l],] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base, l-ésima 
          #categoría de número de cuartos con su respectivo precio
         if(is.na(A3[1,6])){ #Rellenando con ceros los precios de las 
            #medias no disponibles
            A3[1,6]<-0
          }
          if(is.na(PPB3[1,6])){ #Rellenando con ceros los precios de las
            #medias no disponibles del periodo base
            PPB3[1,6]<-0
          }
          if(is.na(A3[1,7])){ #Rellenando con ceros las cantidades no
            #disponibles del periodo base
            A3[1,7]<-0
          }
          if(PPB3[1,6]!=0){ #Media del Precio relativo del año k al
            #periodo base (pit/pi0)
            C[l+length(VE2)*(i-1),1]<-A3[1,6]/PPB3[1,6]} 
          if(PPB3[1,6]==0){ 
            C[l+length(VE2)*(i-1),1]<-0  
          }
          if(is.na(C[l+length(VE2)*(i-1),1])){ #Rellenando con ceros 
            #los precios de las medias no disponibles
            C[l+length(VE2)*(i-1),1]<-0
          }
          C[l+length(VE2)*(i-1),2]<-(PPB3[1,6]*A3[1,7]) #Cantidad del 
          #j-ésimo estado, i-ésimo tipo de localidad, l-ésima categoría de 
          #número de cuartos (pio*qit, pág. 11 Pink B. Brian, 2009)
          if(is.na(C[l+length(VE2)*(i-1),2])){ #Rellenando con ceros 
            #los precios de las ponderaciones no disponibles
            C[l+length(VE2)*(i-1),2]<-0
          }
          C[l+length(VE2)*(i-1),3]<-(C[l+length(VE2)*(i-1),2])*C[
            l+length(VE2)*(i-1),1] 
          # Producto de la cantidad y la media relativa del j-ésimo 
          #estado, k-ésimo año,i-ésimo tipo de localidad, l-ésima categoría 
          #de número de cuartos (pit/pi0)*(pio*qit)
          if(is.na(C[l+length(VE2)*(i-1),3])){ #Rellenando con ceros 
            #los datos no disponibles
            C[l+length(VE2)*(i-1),3]<-0
          }
        }
      }
      Indices[j+length(t(Estado))*(k-1),2]<-as.character(Estado[j,1])
      Indices[j+length(t(Estado))*(k-1),1]<-Anios[k]
      Indices[j+length(t(Estado))*(k-1),4]<-round(x=(sum(C[,3])/sum(C[,2]))*100,digits = 2) #Índice de precios de renta de la vivienda usando un índice del 
      #tipo Paasche,( sum[(pit/pi0)*(pio*qit)]/sum(pio*qit) fórmula (4.6) pág. 
      #11 Pink B. Brian, 2009)
      #Obteniendo el incremento entre periodos
      if(Anios[k]!="2008"){
        Indices[j+length(t(Estado))*(k-1),5]<-round((((Indices[j+length(t(Estado))*(k-1),4]-
                                                         Indices[j+length(t(Estado))*(k-1)-32,4])/
                                                        Indices[j+length(t(Estado))*(k-1)-32,4]))*100,digits = 2)
        #(Indice(t-1)-Indice(t))/Indice(t-1)
        
      }
      if(Anios[k]=="2008"){
        Indices[j+length(t(Estado))*(k-1),5]<-"NA"
      }
      #Rellenando con el nombre del estado
      if(Indices[j+length(t(Estado))*(k-1),2]==1){
        Indices[j+length(t(Estado))*(k-1),3]<-"Aguascalientes"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==2){
        Indices[j+length(t(Estado))*(k-1),3]<-"Baja California"
      } 
      if(Indices[j+length(t(Estado))*(k-1),2]==3){
        Indices[j+length(t(Estado))*(k-1),3]<-"Baja California Sur"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==4){
        Indices[j+length(t(Estado))*(k-1),3]<-"Campeche"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==5){
        Indices[j+length(t(Estado))*(k-1),3]<-"Coahuila de Zaragoza"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==6){
        Indices[j+length(t(Estado))*(k-1),3]<-"Colima"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==7){
        Indices[j+length(t(Estado))*(k-1),3]<-"Chiapas"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==8){
        Indices[j+length(t(Estado))*(k-1),3]<-"Chihuahua"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==9){
        Indices[j+length(t(Estado))*(k-1),3]<-"Ciudad de México"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==10){
        Indices[j+length(t(Estado))*(k-1),3]<-"Durango"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==11){
        Indices[j+length(t(Estado))*(k-1),3]<-"Guanajuato"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==12){
        Indices[j+length(t(Estado))*(k-1),3]<-"Guerrero"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==13){
        Indices[j+length(t(Estado))*(k-1),3]<-"Hidalgo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==14){
        Indices[j+length(t(Estado))*(k-1),3]<-"Jalisco"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==15){
        Indices[j+length(t(Estado))*(k-1),3]<-"México"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==16){
        Indices[j+length(t(Estado))*(k-1),3]<-"Michoacán de Ocampo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==17){
        Indices[j+length(t(Estado))*(k-1),3]<-"Morelos"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==18){
        Indices[j+length(t(Estado))*(k-1),3]<-"Nayarit"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==19){
        Indices[j+length(t(Estado))*(k-1),3]<-"Nuevo León"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==20){
        Indices[j+length(t(Estado))*(k-1),3]<-"Oaxaca"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==21){
        Indices[j+length(t(Estado))*(k-1),3]<-"Puebla"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==22){
        Indices[j+length(t(Estado))*(k-1),3]<-"Querétaro"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==23){
        Indices[j+length(t(Estado))*(k-1),3]<-"Quintana Roo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==24){
        Indices[j+length(t(Estado))*(k-1),3]<-"San Luis Potosí"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==25){
        Indices[j+length(t(Estado))*(k-1),3]<-"Sinaloa"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==26){
        Indices[j+length(t(Estado))*(k-1),3]<-"Sonora"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==27){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tabasco"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==28){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tamaulipas"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==29){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tlaxcala"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==30){
        Indices[j+length(t(Estado))*(k-1),3]<-"Veracruz de Ignacio de la Llave"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==31){
        Indices[j+length(t(Estado))*(k-1),3]<-"Yucatán"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==32){
        Indices[j+length(t(Estado))*(k-1),3]<-"Zacatecas"
      }
      print( Indices) #Matriz de datos
    }
  }
  return(Indices)
}

#Función que calcula los índices de precios de renta de la vivienda privada por Estado del tipo Laspeyres
IPRVPfl<-function(MediasP,Anios,Estado,VE1, VE2){
  #Medias: Marco de datos con las medias de los precios
  #Ponderaciones: Marco de datos con las ponderaciones
  #Anios: Vector de años
  #Estado: Vector de estados
  
  #Matriz Nula donde se guardarán los datos
  indices<-matrix(nrow =length(Anios)*length(t(Estado)), ncol = 5) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indices)<-list(NULL,c("Año","ID Estado","Estado","Índice de Precios
                                 de Renta de la Vivienda Privada","Incremento respecto periodo anterior"))
  Indices<-as.data.frame(indices)
  
  for(k in 1:length(Anios)){
    for(j in 1:length(t(Estado))){
      C<-matrix(nrow =length(VE1)*length(VE2), ncol = 3) #Matriz con las 
      #operaciones del cálculo del índice por ciudad
      dimnames(C)<-list(NULL,c("Media del Precio","Cantidad",
                               "Producto Media.Precio y Cantidad"))
      for (i in 1:length(VE1)){
        for(l in 1:length(VE2)){
          A<-MediasP[MediasP[,2]==Estado[j,1],] # Marco de datos del nombre 
          #del j-ésimo estado con su respectivo precio 
          A1<-A[A[,4]==VE1[i],] # Marco de datos del nombre del i-ésimo tipo
          #de localidad, j-ésimo estado con su respectivo precio 
          PPB1<-A1 #Para obtener las medias del periodo base
          A2<-A1[A1[,1]==Anios[k],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año con su respectivo 
          #precio 
          PPB2<-PPB1[PPB1[,1]=="2008",] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base con su
          #respectivo precio
          A3<-A2[A2[,5]==VE2[l],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año, l-ésima categoría
          #de número de cuartos con su respectivo precio
          PPB3<-PPB2[PPB2[,5]==VE2[l],] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base, l-ésima 
          #categoría de número de cuartos con su respectivo precio
          if(is.na(A3[1,6])){ #Rellenando con ceros los precios de las 
            #medias no disponibles
            A3[1,6]<-0
          }
          if(is.na(PPB3[1,6])){ #Rellenando con ceros los precios de las
            #medias no disponibles del periodo base
            PPB3[1,6]<-0
          }
          if(is.na(PPB3[1,7])){ #Rellenando con ceros las ponderaciones no
            #disponibles del periodo base
            PPB3[1,7]<-0
          }
          if(PPB3[1,6]!=0){ #Media del Precio relativo del año k al
            #periodo base (pit/pi0)
            C[l+length(VE2)*(i-1),1]<-A3[1,6]/PPB3[1,6]} 
          if(PPB3[1,6]==0){ 
            C[l+length(VE2)*(i-1),1]<-0  
          }
          if(is.na(C[l+length(VE2)*(i-1),1])){ #Rellenando con ceros 
            #los precios de las medias no disponibles
            C[l+length(VE2)*(i-1),1]<-0
          }
          C[l+length(VE2)*(i-1),2]<-(PPB3[1,6]*PPB3[1,7]) #Ponderación del 
          #j-ésimo estado, i-ésimo tipo de localidad, l-ésima categoría 
          #de número de cuartos (pio*qi0, pág. 11 Pink B. Brian, 2009)
          if(is.na(C[l+length(VE2)*(i-1),2])){ #Rellenando con ceros 
            #los precios de las ponderaciones no disponibles
            C[l+length(VE2)*(i-1),2]<-0
          }
          C[l+length(VE2)*(i-1),3]<-(C[l+length(VE2)*(i-1),2])*C[
            l+length(VE2)*(i-1),1] 
          # Producto de la Ponderación y la media relativa del j-ésimo 
          #estado,k-ésimo año,i-ésimo tipo de localidad, l-ésima 
          #categoría de número de cuartos (pit/pi0)*(pio*qi0)
          if(is.na(C[l+length(VE2)*(i-1),3])){ #Rellenando con ceros 
            #los datos no disponibles
            C[l+length(VE2)*(i-1),3]<-0
          }
        }
      }
      Indices[j+length(t(Estado))*(k-1),2]<-as.character(Estado[j,1])
      Indices[j+length(t(Estado))*(k-1),1]<-Anios[k]
      Indices[j+length(t(Estado))*(k-1),4]<-round(x=(sum(C[,3])/sum(C[,2]))*100,
                                                  digits = 2) #Índice de precios
      #de renta de la vivienda usando un indice del tipo Laspeyres,
      #( sum[(pit/pi0)*(pio*qi0)]/sum(pio*qi0) fórmula (4.6) pág. 11 Pink B. 
      #Brian, 2009)
      #Obteniendo el incremento entre periodos
      if(Anios[k]!="2008"){
        Indices[j+length(t(Estado))*(k-1),5]<-round((((Indices[j+length(t(Estado))*(k-1),4]-
                                                         Indices[j+length(t(Estado))*(k-1)-32,4])/
                                                        Indices[j+length(t(Estado))*(k-1)-32,4]))*100,digits = 2)
        #(Indice(t-1)-Indice(t))/Indice(t-1)
        
      }
      if(Anios[k]=="2008"){
        Indices[j+length(t(Estado))*(k-1),5]<-"NA"
      }
      #Rellenando con el nombre del estado
      if(Indices[j+length(t(Estado))*(k-1),2]==1){
        Indices[j+length(t(Estado))*(k-1),3]<-"Aguascalientes"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==2){
        Indices[j+length(t(Estado))*(k-1),3]<-"Baja California"
      } 
      if(Indices[j+length(t(Estado))*(k-1),2]==3){
        Indices[j+length(t(Estado))*(k-1),3]<-"Baja California Sur"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==4){
        Indices[j+length(t(Estado))*(k-1),3]<-"Campeche"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==5){
        Indices[j+length(t(Estado))*(k-1),3]<-"Coahuila de Zaragoza"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==6){
        Indices[j+length(t(Estado))*(k-1),3]<-"Colima"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==7){
        Indices[j+length(t(Estado))*(k-1),3]<-"Chiapas"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==8){
        Indices[j+length(t(Estado))*(k-1),3]<-"Chihuahua"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==9){
        Indices[j+length(t(Estado))*(k-1),3]<-"Ciudad de México"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==10){
        Indices[j+length(t(Estado))*(k-1),3]<-"Durango"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==11){
        Indices[j+length(t(Estado))*(k-1),3]<-"Guanajuato"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==12){
        Indices[j+length(t(Estado))*(k-1),3]<-"Guerrero"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==13){
        Indices[j+length(t(Estado))*(k-1),3]<-"Hidalgo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==14){
        Indices[j+length(t(Estado))*(k-1),3]<-"Jalisco"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==15){
        Indices[j+length(t(Estado))*(k-1),3]<-"México"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==16){
        Indices[j+length(t(Estado))*(k-1),3]<-"Michoacán de Ocampo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==17){
        Indices[j+length(t(Estado))*(k-1),3]<-"Morelos"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==18){
        Indices[j+length(t(Estado))*(k-1),3]<-"Nayarit"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==19){
        Indices[j+length(t(Estado))*(k-1),3]<-"Nuevo León"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==20){
        Indices[j+length(t(Estado))*(k-1),3]<-"Oaxaca"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==21){
        Indices[j+length(t(Estado))*(k-1),3]<-"Puebla"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==22){
        Indices[j+length(t(Estado))*(k-1),3]<-"Querétaro"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==23){
        Indices[j+length(t(Estado))*(k-1),3]<-"Quintana Roo"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==24){
        Indices[j+length(t(Estado))*(k-1),3]<-"San Luis Potosí"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==25){
        Indices[j+length(t(Estado))*(k-1),3]<-"Sinaloa"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==26){
        Indices[j+length(t(Estado))*(k-1),3]<-"Sonora"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==27){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tabasco"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==28){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tamaulipas"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==29){
        Indices[j+length(t(Estado))*(k-1),3]<-"Tlaxcala"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==30){
        Indices[j+length(t(Estado))*(k-1),3]<-"Veracruz de Ignacio de la Llave"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==31){
        Indices[j+length(t(Estado))*(k-1),3]<-"Yucatán"
      }
      if(Indices[j+length(t(Estado))*(k-1),2]==32){
        Indices[j+length(t(Estado))*(k-1),3]<-"Zacatecas"
      }
      print( Indices) #Matriz de datos
    }
  }
  return(Indices)
}

#Función que calcula los índices de precios de renta de la vivienda privada estratificados a nivel nacional utilizando los índices elaborados para los estados del tipo Paasche
IPRVPfnp<-function(MediasP,Anios,Estado,VE1, VE2){
  #Medias: Marco de datos con las medianas de los precios
  #Ponderaciones: Marco de datos con las ponderaciones
  #Anios: Vector de años
  #Estado: Vector de estados
  
  #Matriz Nula donde se guardarán los datos por estado
  indices<-matrix(nrow =length(Anios)*length(t(Estado)), ncol = 3) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indices)<-list(NULL,c("Año","Estado","Índice de Precios de Renta 
                                 de la Vivienda Privada"))
  Indices<-as.data.frame(indices)
  
  #Matriz Nula donde se guardarán los datos
  indicesn<-matrix(nrow =length(Anios), ncol = 4) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indicesn)<-list(NULL,c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior"))
  Indicesn<-as.data.frame(indicesn)
  
  for(k in 1:length(Anios)){
    PC<-matrix(nrow =length(t(Estado)), ncol = 3) #Matrix con las 
    #ponderaciones por estado
    dimnames(PC)<-list(NULL,c("Estado","Cantidad","Cantidad por
                              índice.Estado"))
    PC<-as.data.frame(PC)
    for(j in 1:length(t(Estado))){
      C<-matrix(nrow =length(VE1)*length(VE2), ncol = 3) #Matriz con las 
      #operaciones del cálculo del índice por ciudad
      dimnames(C)<-list(NULL,c("Media del Precio","Cantidad","Producto
                               Media.Precio y Cantidad"))
      for (i in 1:length(VE1)){
        for(l in 1:length(VE2)){
          A<-MediasP[MediasP[,2]==Estado[j,1],] # Marco de datos del nombre 
          #del j-ésimo estado con su respectivo precio 
          A1<-A[A[,4]==VE1[i],] # Marco de datos del nombre del i-ésimo tipo
          #de localidad, j-ésimo estado con su respectivo precio 
          PPB1<-A1 #Para obtener las medias del periodo base
          A2<-A1[A1[,1]==Anios[k],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año con su respectivo 
          #precio 
          PPB2<-PPB1[PPB1[,1]=="2008",] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base con su
          #respectivo precio
          A3<-A2[A2[,5]==VE2[l],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año, l-ésima categoría
          #de número de cuartos con su respectivo precio
          PPB3<-PPB2[PPB2[,5]==VE2[l],] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base, l-ésima 
          #categoría de número de cuartos con su respectivo precio
          if(is.na(A3[1,6])){ #Rellenando con ceros los precios de las 
            #medias no disponibles
            A3[1,6]<-0
          }
          if(is.na(PPB3[1,6])){ #Rellenando con ceros los precios de las
            #medias no disponibles del periodo base
            PPB3[1,6]<-0
          }
          if(is.na(A3[1,7])){ #Rellenando con ceros las ponderaciones no
            #disponibles del periodo base
            A3[1,7]<-0
          }
          if(PPB3[1,6]!=0){ #Media del Precio relativo del año k al
            #periodo base (pit/pi0)
            C[l+length(VE2)*(i-1),1]<-A3[1,6]/PPB3[1,6]} 
          if(PPB3[1,6]==0){ 
            C[l+length(VE2)*(i-1),1]<-0  
          }
          if(is.na(C[l+length(VE2)*(i-1),1])){ #Rellenando con ceros 
            #los precios de las medias no disponibles
            C[l+length(VE2)*(i-1),1]<-0
          }
          C[l+length(VE2)*(i-1),2]<-(PPB3[1,6]*A3[1,7]) #Ponderación del 
          #j-ésimo estado, i-ésimo tipo de localidad, l-ésima categoría de 
          #número de cuartos (pio*qit, pág. 11 Pink B. Brian, 2009)
          if(is.na(C[l+length(VE2)*(i-1),2])){ #Rellenando con ceros 
            #los precios de las ponderaciones no disponibles
            C[l+length(VE2)*(i-1),2]<-0
          }
          C[l+length(VE2)*(i-1),3]<-(C[l+length(VE2)*(i-1),2])*C[
            l+length(VE2)*(i-1),1] 
          # Producto de la Ponderación y la media relativa del j-ésimo 
          #estado, k-ésimo año,i-ésimo tipo de localidad, l-ésima categoría 
          #de número de cuartos (pit/pi0)*(pio*qit)
          if(is.na(C[l+length(VE2)*(i-1),3])){ #Rellenando con ceros 
            #los datos no disponibles
            C[l+length(VE2)*(i-1),3]<-0
          }
        }
      }
      Indices[j+length(t(Estado))*(k-1),2]<-as.character(Estado[j,1])
      Indices[j+length(t(Estado))*(k-1),1]<-Anios[k]
      Indices[j+length(t(Estado))*(k-1),3]<-round(x=(sum(C[,3])/sum(C[,2])
      )*100,digits = 2) 
      #Rellenando con ceros los índices para algún estado no disponible
      if(is.na(Indices[j+length(t(Estado))*(k-1),3])){
        Indices[j+length(t(Estado))*(k-1),3]<-0
      }
      #Índice de precios de renta de la vivienda usando un índice del 
      #tipo Paasche, (pág. 11 Pink B. Brian, 2009)
      PC[j,1]<-as.character(Estado[j,1])
      PC[j,2]<-sum(C[,2]) #Ponderación del estado j
      PC[j,3]<-(Indices[j+length(t(Estado))*(k-1),3])*PC[j,2] 
      #Ponderación multiplicada por el índice del estado j y año k
    }
    Indicesn[k,2]<-as.character("Nacional")
    Indicesn[k,1]<-Anios[k]
    Indicesn[k,3]<-round(x=(sum(PC[,3]))/sum(PC[,2]),digits = 2) 
    #Obteniendo el incremento entre periodos
    if(Anios[k]!="2008"){
      Indicesn[k,4]<-round((((Indicesn[k,3]-Indicesn[k-1,3])/Indicesn[k-1,3]))*100,digits = 2)
      #(Indice(t-1)-Indice(t))/Indice(t-1)
      
    }
    if(Anios[k]=="2008"){
      Indicesn[k,4]<-"NA"
    }
    #Índice de precios de renta de la vivienda privada usando un índice 
    #del tipo Paasche, (pág. 11 Pink B. Brian, 2009)
    print( Indicesn) #Matriz de datos
}
  return(Indicesn)
  }

#Función que calcula los índices de precios de renta de la vivienda privada estratificados a nivel nacional utilizando los índices elaborados para los estados del tipo Laspeyres
IPRVPfnl<-function(MediasP,Anios,Estado,VE1, VE2){
  #Medias: Marco de datos con las medianas de los precios
  #Ponderaciones: Marco de datos con las ponderaciones
  #Anios: Vector de años
  #Estado: Vector de estados
  
  #Matriz Nula donde se guardarán los datos por estado
  indices<-matrix(nrow =length(Anios)*length(t(Estado)), ncol = 3) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indices)<-list(NULL,c("Año","Estado","Índice de Precios de Renta 
                                 de la Vivienda Privada"))
  Indices<-as.data.frame(indices)
  
  #Matriz Nula donde se guardarán los datos
  indicesn<-matrix(nrow =length(Anios), ncol = 4) 
  #Colocando el nombre de cada columna de la matriz
  dimnames(indicesn)<-list(NULL,c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior"))
  Indicesn<-as.data.frame(indicesn)
  
  for(k in 1:length(Anios)){
    PC<-matrix(nrow =length(t(Estado)), ncol = 3) #Matriz con las 
    #ponderaciones por estado
    dimnames(PC)<-list(NULL,c("Estado","Cantidad","Cantidad por
                              índice.Estado"))
    PC<-as.data.frame(PC)
    for(j in 1:length(t(Estado))){
      C<-matrix(nrow =length(VE1)*length(VE2), ncol = 3) #Matriz con las 
      #operaciones del cálculo del índice por ciudad
      dimnames(C)<-list(NULL,c("Media del Precio","Cantidad","Producto
                               Media.Precio y Cantidad"))
      for (i in 1:length(VE1)){
        for(l in 1:length(VE2)){
          A<-MediasP[MediasP[,2]==Estado[j,1],] # Marco de datos del nombre 
          #del j-ésimo estado con su respectivo precio 
          A1<-A[A[,4]==VE1[i],] # Marco de datos del nombre del i-ésimo tipo
          #de localidad, j-ésimo estado con su respectivo precio 
          PPB1<-A1 #Para obtener las medias del periodo base
          A2<-A1[A1[,1]==Anios[k],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año con su respectivo 
          #precio 
          PPB2<-PPB1[PPB1[,1]=="2008",] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base con su
          #respectivo precio
          A3<-A2[A2[,5]==VE2[l],] # Marco de datos del nombre del i-ésimo 
          #tipo de localidad, j-ésimo estado, k-ésimo año, l-ésima categoría
          #de número de cuartos con su respectivo precio
          PPB3<-PPB2[PPB2[,5]==VE2[l],] # Marco de datos del nombre del 
          #i-ésimo tipo de localidad, j-ésimo estado, año base, l-ésima 
          #categoría de número de cuartos con su respectivo precio
          if(is.na(A3[1,6])){ #Rellenando con ceros los precios de las 
            #medias no disponibles
            A3[1,6]<-0
          }
          if(is.na(PPB3[1,6])){ #Rellenando con ceros los precios de las
            #medias no disponibles del periodo base
            PPB3[1,6]<-0
          }
          if(is.na(PPB3[1,7])){ #Rellenando con ceros las ponderaciones no
            #disponibles del periodo base
            PPB3[1,7]<-0
          }
          if(PPB3[1,6]!=0){ #Media del Precio relativo del año k al
            #periodo base (pit/pi0)
            C[l+length(VE2)*(i-1),1]<-A3[1,6]/PPB3[1,6]} 
          if(PPB3[1,6]==0){ 
            C[l+length(VE2)*(i-1),1]<-0  
          }
          if(is.na(C[l+length(VE2)*(i-1),1])){ #Rellenando con ceros 
            #los precios de las medias no disponibles
            C[l+length(VE2)*(i-1),1]<-0
          }
          C[l+length(VE2)*(i-1),2]<-(PPB3[1,6]*PPB3[1,7]) #Ponderación del 
          #j-ésimo estado, i-ésimo tipo de localidad, l-ésima categoría 
          #de número de cuartos (pio*qi0, pág. 11 Pink B. Brian, 2009)
          if(is.na(C[l+length(VE2)*(i-1),2])){ #Rellenando con ceros 
            #los precios de las ponderaciones no disponibles
            C[l+length(VE2)*(i-1),2]<-0
          }
          C[l+length(VE2)*(i-1),3]<-(C[l+length(VE2)*(i-1),2])*C[
            l+length(VE2)*(i-1),1] 
          # Producto de la Ponderación y la media relativa del j-ésimo 
          #estado,k-ésimo año,i-ésimo tipo de localidad, l-ésima 
          #categoría de número de cuartos (pit/pi0)*(pio*qi0)
          if(is.na(C[l+length(VE2)*(i-1),3])){ #Rellenando con ceros 
            #los datos no disponibles
            C[l+length(VE2)*(i-1),3]<-0
          }
        }
      }
      Indices[j+length(t(Estado))*(k-1),2]<-as.character(Estado[j,1])
      Indices[j+length(t(Estado))*(k-1),1]<-Anios[k]
      Indices[j+length(t(Estado))*(k-1),3]<-round(x=(sum(C[,3])/sum(C[,2])
      )*100,digits = 2)
      #Rellenando con ceros los índices para algún estado no disponible
      if(is.na(Indices[j+length(t(Estado))*(k-1),3])){
        Indices[j+length(t(Estado))*(k-1),3]<-0
      }
      #Índice de precios de renta de la vivienda usando un índice del 
      #tipo Laspeyres, (pág. 11 Pink B. Brian, 2009)
      PC[j,1]<-as.character(Estado[j,1])
      PC[j,2]<-sum(C[,2]) #Ponderación del estado j
      PC[j,3]<-(Indices[j+length(t(Estado))*(k-1),3])*PC[j,2] 
      #Ponderación multiplicada por el índice del estado j y año k
    }
    Indicesn[k,2]<-as.character("Nacional")
    Indicesn[k,1]<-Anios[k]
    Indicesn[k,3]<-round(x=(sum(PC[,3]))/sum(PC[,2]),digits = 2) 
    #Obteniendo el incremento entre periodos
    if(Anios[k]!="2008"){
      Indicesn[k,4]<-round((((Indicesn[k,3]-Indicesn[k-1,3])/Indicesn[k-1,3]))*100,digits = 2)
      #(Indice(t-1)-Indice(t))/Indice(t-1)
      
    }
    if(Anios[k]=="2008"){
      Indicesn[k,4]<-"NA"
    }
    #Índice de precios de renta de la vivienda privada usando un índice 
    #del tipo Laspeyres, (pág. 11 Pink B. Brian, 2009)
    print( Indicesn) #Matriz de datos
}
  return(Indicesn)
  }
#Aplicando la función a los datos para obtener el IPRVP(Índice de Precios de Renta de la Vivienda Privada estratificado Estado del tipo Pasche)
IPRVP.EP<-IPRVPfp(MediasP = Base.datos.Medias.Precios.Estados,
                  Anios = VA, Estado=VE,VE1=TL, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVP(Índice de Precios de 
#Renta de la Vivienda Privada por Estado del tipo Laspeyres)
IPRVP.EL<-IPRVPfl(MediasP = Base.datos.Medias.Precios.Estados,
                  Anios = VA, Estado=VE,VE1=TL, VE2=NC)

#Obteniendo el IPRVP.E (Índice de Precios de Renta de la Vivienda Privada por estado del tipo Fisher
IPRVP.EF<-IPRVP.EL

for(i in 1:length(IPRVP.EL[,4])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.EF[i,4]<-round(sqrt(IPRVP.EP[i,4]*IPRVP.EL[i,4]),digits = 2) 
} 
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.EF[,4])){
  if(IPRVP.EF[i,1]!="2008"){
    IPRVP.EF[i,5]<-round((((IPRVP.EF[i,4]-IPRVP.EF[i-32,4])/
                             IPRVP.EF[i-32,4]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.EF[i,1]=="2008"){
    IPRVP.EF[i,5]<-"NA"
  }
}
#Obteniendo el índice deflactado por el índice de precios implícito del gasto en consumo privado (IPICP),que es utlizado por la OCDE para deflactar el índice SHF de precios de la vivienda 
#Creando un vector
D<-IPRVP.EF[,4]
D<-as.data.frame(D) 
names(D)<-"IPRVP deflactado al 2008"
IPRVP.EF<-cbind(IPRVP.EF,D)
#Calculando los valores
for(i in 1:length(IPRVP.EF[,1])){
  if(IPRVP.EF[i,1]=="2010"){
    IPRVP.EF[i,6]<-round(IPRVP.EF[i,4]/1.098,digits = 2)
  }
  if(IPRVP.EF[i,1]=="2012"){
    IPRVP.EF[i,6]<-round(IPRVP.EF[i,4]/1.181,digits = 2)
  }
  if(IPRVP.EF[i,1]=="2014"){
    IPRVP.EF[i,6]<-round(IPRVP.EF[i,4]/1.263,digits = 2)
  }
}
# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.EF,file = "IPRVP por estado.csv",row.names = FALSE)  

#Aplicando la función a los datos para obtener el IPRVPN (Índice de Precios
#de Renta de la Vivienda Privada estratificado a nivel Nacional del
#tipo Paasche
IPRVP.NP<-IPRVPfnp(MediasP = Base.datos.Medias.Precios.Estados,
                   Anios = VA, Estado=VE,VE1=TL, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVPN (Índice de Precios de Renta de la Vivienda Privada estratificado a nivel Nacional del tipo Laspeyres
IPRVP.NL<-IPRVPfnl(MediasP = Base.datos.Medias.Precios.Estados,
                   Anios = VA, Estado=VE,VE1=TL, VE2=NC)

#Obteniendo el IPRVPN (Índice de Precios de Renta de la Vivienda Privada estratificado a nivel Nacional del tipo Fisher
IPRVP.NF<-IPRVP.NL

for(i in 1:length(IPRVP.NL[,3])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.NF[i,3]<-round(sqrt(IPRVP.NP[i,3]*IPRVP.NL[i,3]),digits = 2)   
}
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.NF[,4])){
  if(IPRVP.NF[i,1]!="2008"){
    IPRVP.NF[i,4]<-round((((IPRVP.NF[i,3]-IPRVP.NF[i-1,3])/
                             IPRVP.NF[i-1,3]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.NF[i,1]=="2008"){
    IPRVP.NF[i,4]<-"NA"
  }
}
#Obteniendo el índice deflactado por el índice de precios implícito del gasto en consumo privado (IPICP),que es utlizado por la OCDE para deflactar el índice SHF de precios de la vivienda 
#Creando un vector
D<-IPRVP.NF[,3]
D<-as.data.frame(D) 
names(D)<-"IPRVP deflactado al 2008"
IPRVP.NF<-cbind(IPRVP.NF,D)
#Calculando los valores
for(i in 1:length(IPRVP.NF[,1])){
  if(IPRVP.NF[i,1]=="2010"){
    IPRVP.NF[i,5]<-round(IPRVP.NF[i,3]/1.098,digits = 2)
  }
  if(IPRVP.NF[i,1]=="2012"){
    IPRVP.NF[i,5]<-round(IPRVP.NF[i,3]/1.181,digits = 2)
  }
  if(IPRVP.NF[i,1]=="2014"){
    IPRVP.NF[i,5]<-round(IPRVP.NF[i,3]/1.263,digits = 2)
  }
}
# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.NF,file = "IPRVP Nacional.csv",row.names = FALSE)  
#Obteniendo las desagregaciones del índice para zona rural y urbana
#Área Rural
Ru<-TL[-c(1:3)]
#Aplicando la función para obtener las medias y cantidades
Base.datos.Medias.Precios.Estados.Rural<-Medias.Precio(Datos=BD, Anios=VA,Estado=VE,VE1=Ru, VE2=NC)
#Rellenando los valores faltantes en las medias con un cero
for( j in 1:length(Base.datos.Medias.Precios.Estados.Rural[,6])){
  if(is.na(Base.datos.Medias.Precios.Estados.Rural[j,6])){
    Base.datos.Medias.Precios.Estados.Rural[j,6]<-0
  }
}
#Aplicando la función a los datos para obtener el IPRVP del tipo Pasche
IPRVP.EPR<-IPRVPfp(MediasP = Base.datos.Medias.Precios.Estados.Rural,
                   Anios = VA, Estado=VE,VE1=Ru, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVP del tipo Laspeyres
IPRVP.ELR<-IPRVPfl(MediasP = Base.datos.Medias.Precios.Estados.Rural,
                   Anios = VA, Estado=VE,VE1=Ru, VE2=NC)

#Obteniendo el el IPRVP.E  del tipo Fisher 
IPRVP.EFR<-IPRVP.ELR

for(i in 1:length(IPRVP.ELR[,4])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.EFR[i,4]<-round(sqrt(IPRVP.EPR[i,4]*IPRVP.ELR[i,4]),digits = 2)   
}
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.EFR[,4])){
  if(IPRVP.EFR[i,1]!="2008"){
    IPRVP.EFR[i,5]<-round((((IPRVP.EFR[i,4]-IPRVP.EFR[i-32,4])/
                              IPRVP.EFR[i-32,4]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.EFR[i,1]=="2008"){
    IPRVP.EFR[i,5]<-"NA"
  }
}
# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.EFR,file = "IPRVP por estado en el área rural.csv",row.names = FALSE)  

#Aplicando la función a los datos para obtener el IPRVPN  a nivel Nacional del tipo Paasche
IPRVP.NPR<-IPRVPfnp(MediasP = Base.datos.Medias.Precios.Estados.Rural,
                    Anios = VA, Estado=VE,VE1=Ru, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVPN nivel Nacional del tipo Laspeyres
IPRVP.NLR<-IPRVPfnl(MediasP = Base.datos.Medias.Precios.Estados.Rural,
                    Anios = VA, Estado=VE,VE1=Ru, VE2=NC)

#Obteniendo el IPRVPN del tipo Fisher 
IPRVP.NFR<-IPRVP.NLR

for(i in 1:length(IPRVP.NLR[,3])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.NFR[i,3]<-round(sqrt(IPRVP.NPR[i,3]*IPRVP.NLR[i,3]),digits = 2)   
}
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.NFR[,4])){
  if(IPRVP.NFR[i,1]!="2008"){
    IPRVP.NFR[i,4]<-round((((IPRVP.NFR[i,3]-IPRVP.NFR[i-1,3])/
                              IPRVP.NFR[i-1,3]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.NFR[i,1]=="2008"){
    IPRVP.NFR[i,4]<-"NA"
  }
}

# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.NFR,file = "IPRVP Nacional en el área Rural.csv",row.names = FALSE)
#Área Urbana
Ur<-TL[-4]
#Aplicando la función para obtener las medias y cantidades
Base.datos.Medias.Precios.Estados.Urbana<-Medias.Precio(Datos=BD, Anios=VA,Estado=VE,VE1=Ur, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVP del tipo Pasche
IPRVP.EPU<-IPRVPfp(MediasP = Base.datos.Medias.Precios.Estados.Urbana,
                   Anios = VA, Estado=VE,VE1=Ur, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVP del tipo Laspeyres
IPRVP.ELU<-IPRVPfl(MediasP = Base.datos.Medias.Precios.Estados.Urbana,
                   Anios = VA, Estado=VE,VE1=Ur, VE2=NC)

#Obteniendo el IPRVP.E  del tipo Fisher 
IPRVP.EFU<-IPRVP.ELU

for(i in 1:length(IPRVP.ELU[,4])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.EFU[i,4]<-round(sqrt(IPRVP.EPU[i,4]*IPRVP.ELU[i,4]),digits = 2)   
}
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.EFU[,4])){
  if(IPRVP.EFU[i,1]!="2008"){
    IPRVP.EFU[i,5]<-round((((IPRVP.EFU[i,4]-IPRVP.EFU[i-32,4])/
                              IPRVP.EFU[i-32,4]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.EFU[i,1]=="2008"){
    IPRVP.EFU[i,5]<-"NA"
  }
}
# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.EFU,file = "IPRVP por estado en el área urbana.csv",row.names = FALSE)  

#Aplicando la función a los datos para obtener el IPRVPN  a nivel Nacional del tipo Paasche
IPRVP.NPU<-IPRVPfnp(MediasP = Base.datos.Medias.Precios.Estados.Urbana,
                    Anios = VA, Estado=VE,VE1=Ur, VE2=NC)

#Aplicando la función a los datos para obtener el IPRVPN nivel Nacional del tipo Laspeyres
IPRVP.NLU<-IPRVPfnl(MediasP = Base.datos.Medias.Precios.Estados.Urbana,
                    Anios = VA, Estado=VE,VE1=Ur, VE2=NC)

#Obteniendo el IPRVPN del tipo Fisher 
IPRVP.NFU<-IPRVP.NLU

for(i in 1:length(IPRVP.NLU[,3])){
  #Obteniendo la media geométrica de los índices de Laspeyres y Paasche
  IPRVP.NFU[i,3]<-round(sqrt(IPRVP.NPU[i,3]*IPRVP.NLU[i,3]),digits = 2)   
}
#Obteniendo el incremento entre periodos
for(i in 1:length(IPRVP.NFU[,4])){
  if(IPRVP.NFU[i,1]!="2008"){
    IPRVP.NFU[i,4]<-round((((IPRVP.NFU[i,3]-IPRVP.NFU[i-1,3])/
                              IPRVP.NFU[i-1,3]))*100,digits = 2)
    #(Indice(t-1)-Indice(t))/Indice(t-1)
    
  }
  if(IPRVP.NFU[i,1]=="2008"){
    IPRVP.NFU[i,4]<-"NA"
  }
}
# Guardar en un archivo de tipo CSV
write.csv(x = IPRVP.NFU,file = "IPRVP Nacional en el Área Urbana.csv",row.names = FALSE)
#Gráficas de los índices finales 
setwd("Gráficas")
#Estados
EST<-sort(unique(IPRVP.EF[,3]));EST
#Por Estados 
lapply(1:32,function(i){
  E<-IPRVP.EF[IPRVP.EF[,3]==EST[i],]
  jpeg(filename=paste(EST[i],".jpeg",sep = ""),quality=100,width = 600)
  print(qplot(x=E[,1],y=E[,4],data=E,geom = "point",asp=1/2, 
              xlab = "Año",ylab="IPRVP", main =paste("Índice de Precios de Renta de la Vivienda Privada en",EST[i] ,sep = " ")))
  dev.off() 
})

# Nacional
jpeg(filename="Índice de Precios de Renta de la Vivienda Privada Nacional.jpeg",quality=100,width = 600)
qplot(x=IPRVP.NF[,1],y=IPRVP.NF[,3],data=IPRVP.NF,geom = "point",asp=1/2, xlab = "Año",
      ylab="IPRVP",main ="Índice de Precios de Renta de la Vivienda Privada Nacional")
dev.off()

#Gráficas de los índices finales desagregados por urbana y rural 
setwd("Rural")
#Estados
EST<-sort(unique(IPRVP.EFR[,3]));EST
#Por Estados 
lapply(1:32,function(i){
  E<-IPRVP.EFR[IPRVP.EFR[,3]==EST[i],]
  jpeg(filename=paste(EST[i],".jpeg",sep = ""),quality=100,width = 600)
  print(qplot(x=E[,1],y=E[,4],data=E,geom = "point",asp=1/2, 
              xlab = "Año",ylab="IPRVP", main =paste("Índice de Precios de Renta de la Vivienda Privada en el Área Rural en",EST[i] ,sep = " ")))
  dev.off() 
})

# Nacional
jpeg(filename="IPRVP Nacional Área Rural.jpeg",quality=100,width = 600)
qplot(x=IPRVP.NFR[,1],y=IPRVP.NFR[,3],data=IPRVP.NFR,geom = "point",asp=1/2, xlab = "Año",
      ylab="IPRVP",main ="Índice de Precios de Renta de la Vivienda Privada Nacional Área Rural")
dev.off()
setwd("../Urbana")
#Estados
EST<-sort(unique(IPRVP.EFU[,3]));EST
#Por Estados 
lapply(1:32,function(i){
  E<-IPRVP.EFU[IPRVP.EFU[,3]==EST[i],]
  jpeg(filename=paste(EST[i],".jpeg",sep = ""),quality=100,width = 600)
  print(qplot(x=E[,1],y=E[,4],data=E,geom = "point",asp=1/2, 
              xlab = "Año",ylab="IPRVP", main =paste("Índice de Precios de Renta de la Vivienda Privada en el Área Urbana en",EST[i] ,sep = " ")))
  dev.off() 
})

# Nacional
jpeg(filename="IPRVP Nacional Área Urbana.jpeg",quality=100,width = 600)
qplot(x=IPRVP.NFU[,1],y=IPRVP.NFU[,3],data=IPRVP.NFU,geom = "point",asp=1/2, xlab = "Año",
      ylab="IPRVP",main ="Índice de Precios de Renta de la Vivienda Privada Nacional Área Urbana")
dev.off()

#Gráficas Conjuntas
class(IPRVP.EF[,2])<-"numeric"
#Gráficas de estados por grupos
IPRVP.EFG<-IPRVP.EF[IPRVP.EF[,2]<=8,]
E<-qplot(x = IPRVP.EFG[,1],y =IPRVP.EFG[,4],data = IPRVP.EFG,col= Estado,xlim = c(2008,2016),
         main = "IPVRP Estatal",ylab = "IPRVP",geom = "path",xlab = "Año")
E+scale_color_brewer(palette="Set1")
E+geom_point(aes(x = IPRVP.EFG[,1],y =IPRVP.EFG[,4]))
#Nacionales
IPRVP.NFR[,2]<-"Nacional Rural"
IPRVP.NFU[,2]<-"Nacional Urbana"
IPRVP.N<-rbind(IPRVP.NF,IPRVP.NFR,IPRVP.NFU)
N<-qplot(x = IPRVP.N[,1],y =IPRVP.N[,3],data = IPRVP.N,col= Nivel,
         main = "IPVRP Nacional",ylab = "IPRVP",geom = "path",xlab = "Año",xlim = c(2008,2016))
N+scale_color_brewer(palette="Set1")
N+geom_point(aes(x = IPRVP.N[,1],y =IPRVP.N[,3]))
N + geom_text(aes(y =IPRVP.N[,3], ymax = IPRVP.N[,3], label = IPRVP.N[,3]), 
                    position = position_dodge(width = 0.9), size=3, vjust=-1, hjust=0.5 ,col="black")
    #Creando un conjunto con los índices de algunos estados y el nacional para hacer un comparativo
IPRVP.EFG1<-IPRVP.EF[IPRVP.EF[,2]==3,-2]
names(IPRVP.EFG1)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
IPRVP.EFG2<-IPRVP.EF[IPRVP.EF[,2]==9,-2]
names(IPRVP.EFG2)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
IPRVP.EFG3<-IPRVP.EF[IPRVP.EF[,2]==21,-2]
names(IPRVP.EFG3)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
IPRVP.EFG4<-IPRVP.EF[IPRVP.EF[,2]==15,-2]
names(IPRVP.EFG4)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
IPRVP.EFG5<-IPRVP.EF[IPRVP.EF[,2]==14,-2]
names(IPRVP.EFG5)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
IPRVP.EFG6<-IPRVP.EF[IPRVP.EF[,2]==32,-2]
names(IPRVP.EFG6)<-c("Año","Nivel","Índice de Precios de Renta 
                                  de la Vivienda Privada","Incremento respecto periodo anterior")
    #Concatenando las bases de datos
IPRVP.NCE<-rbind(IPRVP.NF,IPRVP.EFG1,IPRVP.EFG2,IPRVP.EFG3,IPRVP.EFG4,IPRVP.EFG5,IPRVP.EFG6)
    #Graficándolos
N<-qplot(x = IPRVP.NCE[,1],y =IPRVP.NCE[,3],data = IPRVP.NCE,col= Nivel,
         main = "IPVRP Comparativo Nacional vs Estados",ylab = "IPRVP",geom = "path",xlab = "Año")
N+scale_color_brewer(palette="Set1")
N+geom_point(aes(x = IPRVP.NCE[,1],y =IPRVP.NCE[,3]))
