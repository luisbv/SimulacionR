library("ggplot2")
library("parallel")

repeticiones = 10
duracion = 1000
dimensiones = 8

caminata = function(replica){
  inicio = Sys.time()
  coord = rep(0, dim)
  centros = 0
  for (paso in 1:duracion){
    pos = sample(1:dim, 1)
    coord[pos] = coord[pos] + (1 - (runif(1) > 0.5) * 2)
    centros = centros + (sum(abs(coord)) == 0) * 1
  }
  final = Sys.time() - inicio
  return(c(centros, final))
}

resultados = data.frame(Centros=integer(), Dimension=integer(), Tiempo=double(), Implementación=character())
tiempos = data.frame(Dimension=integer(), Tiempo=double(),Implementación=character())

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")

for (dim in 1:dimensiones){

  #Secuencial
  inicio_sec = Sys.time()
  resultado = sapply(1:repeticiones, caminata)
  final_sec = Sys.time() - inicio_sec
  centro = resultado[1,]
  tiempo = resultado[2,]
  sec = data.frame(Centros=centro,Dimension=rep(dim,repeticiones), Tiempo=tiempo,Implementación=rep("Secuencial",repeticiones))
  resultados = rbind(resultados, sec)

  #Paralelo
  clusterExport(cluster, "dim")
  inicio_par = Sys.time()
  resultado = parSapply(cluster,1:repeticiones, caminata)
  final_par = Sys.time() - inicio_par
  centro = resultado[1,]
  tiempo = resultado[2,]
  par = data.frame(Centros=centro,Dimension=rep(dim,repeticiones), Tiempo=tiempo,Implementación=rep("Paralela",repeticiones))
  resultados = rbind(resultados, par)

  tiempos = rbind(tiempos, data.frame(Dimension=dim, Tiempo=final_sec, Implementación="Secuencial"))
  tiempos = rbind(tiempos, data.frame(Dimension=dim, Tiempo=final_par, Implementación="Paralela"))
}
stopCluster(cluster)

png("Centros_Replica_Sec_vs_Par.png")
ggplot(resultados,aes(x=factor(Dimension), y=Centros)) +
geom_boxplot(aes(fill=Implementación)) +
scale_x_discrete(limits=paste(seq(1:dimensiones)),labels=paste(seq(1:dimensiones)))+
theme(legend.position = "bottom")
graphics.off()

png("Tiempos_Replica_Sec_vs_Par.png")
ggplot(resultados,aes(x=factor(Dimension), y=Tiempo)) +
geom_boxplot(aes(fill=Implementación)) +
scale_x_discrete(limits=paste(seq(1:dimensiones)),labels=paste(seq(1:dimensiones)))+
theme(legend.position = "bottom")
graphics.off()

png("Tiempos_Caminata_Sec_vs_Par.png")
ggplot(tiempos,aes(x=Dimension, y=Tiempo, colour=Implementación)) +
geom_point(size=5) +
geom_line(linetype="dashed") +
scale_x_discrete(limits=paste(seq(1:dimensiones)),labels=paste(seq(1:dimensiones)))+
theme(legend.position = "bottom")
graphics.off()
