
#------   INICIALIZAR  -------------------------------------------------------------------------------------------------------------------------
if(!require('utils')) {
  install.packages('utils')
  library('utils')
}


rm(list = ls())
cat("\014")

getwd()

if (interactive() && .Platform$OS.type == "windows") {
  choose.dir(getwd(), "Choose a suitable folder")
} else {
  file.choose()
}

dir = as.character(getwd())
texto1 = paste0(dir,"/")
texto1 = as.character(chartr("/","\\",texto1))

source(paste0(texto1,"Modelo 0 Inicializar.R"))

DATOS_INICIO = (Inicializar())
rightnow = as.character(Sys.time(),format="%d-%m-%Y %H%M%S")

N = 100
Consumos_base = (DATOS_INICIO[,c(3,5,6)])
Generacion_base = (DATOS_INICIO[,2])
Num_perfiles_gen = 51
p_tr_case = "continuo"
lambda = 0.5



# OBTENER DATOS DE CALCULO -----------------------------------------------------------------------------------------------------

#Prueba nicial de funcionamiento
p_grid_buy = rep(0.30,8760)     #???/kWh
p_grid_sell = rep(0.10,8760)    #???/kWh
# p_tr = rep(0.25,8760)           #???/kWh
p_facility_buy = rep(0,8760)    #???/kWh

Distrib = c(rep(2,round(N*0.5)),rep(3,round(N*0.5)))
PERFILGEN = 3
Resultado = Costes_price_based(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN,p_tr_case)
Resumen_costes_cont = (Resultado$Resumen_costes)
Resumen_costes_cont[c(4:9,11:16)] = round(as.numeric(unlist(Resumen_costes_cont[c(4:9,11:16)])),2)

# INICIaLIZAR DISTRUBUCIONES
# Probamos 3 Distrib de 2 y 3 respectivamente, 90-10%, 74-26%, 50-50%
Conjunto_Distrib = data.frame(Corr90 = c(rep(2,round(N*0.1)),rep(3,round(N*0.9))),
                              Corr70 = c(rep(2,round(N*0.3)),rep(3,round(N*0.7))),
                              Corr50 = c(rep(2,round(N*0.5)),rep(3,round(N*0.5))))

Conjunto_PERFILGEN = c(1:Num_perfiles_gen)



# Caso 1 "Resumen_TEST2_pbuy_1.0_psell_0.1_N-100_P-51.RData" ---------------------------------------------
p_grid_buy = rep(1.0,8760)     #???/kWh
p_grid_sell = rep(0.10,8760)    #???/kWh
# p_tr = rep(0.25,8760)           #???/kWh
p_facility_buy = rep(0,8760)    #???/kWh

Resumen_TEST2 = Resumen_costes_cont[1,]
Resumen_TEST2 = cbind(Distrib = NA, PerfilGEN = NA, Resumen_TEST2)
Resumen_TEST2 = Resumen_TEST2[-1,]

for (i in 1:ncol(Conjunto_Distrib)) {
  
  for (u in 1:length(Conjunto_PERFILGEN)) {
    
    Distrib = Conjunto_Distrib[,i]
    PERFILGEN = Conjunto_PERFILGEN[u]
    
    Resultado = Costes_price_based(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN,p_tr_case)
    
    Resumen_costes_cont = (Resultado$Resumen_costes)
    Resumen_costes_cont[c(4:9,11:16)] = round(as.numeric(unlist(Resumen_costes_cont[c(4:9,11:16)])),2)
    
    nombreresultado = paste("Resumen ",colnames(Conjunto_Distrib)[i],"PERF",(Conjunto_PERFILGEN)[u])
    # 
    # assign(paste("Resumen ",colnames(Conjunto_Distrib)[i],"PERF",(Conjunto_PERFILGEN)[u]),
    #        Resumen_costes_cont)
    
    Temp = data.frame(Distrib = colnames(Conjunto_Distrib)[i],
                      PerfilGEn = PERFILGEN,
                      Resumen_costes_cont)
    
    Resumen_TEST2 = rbind(Resumen_TEST2, Temp[N+1,])
    
    # GraficoPLOT = Resultado$GraficoPLOT
    # 
    nombregraf = paste0(nombreresultado,".jpeg")
    print(nombregraf)
 
    
  }
}

path = paste0(getwd(),"/")
nombrefile = paste0("Resumen_TEST2_pbuy_",p_grid_buy[1],"_psell_",p_grid_sell[1],"_N-",N,"_P-",PERFILGEN," ",rightnow,".RData")
print(nombrefile)
save(Resumen_TEST2,file=nombrefile)


# Caso 2 "Resumen_TEST2_pbuy_0.3_psell_0.1_N-100_P-51.RData" ---------------------------------------------

p_grid_buy = rep(0.3,8760)     #???/kWh
p_grid_sell = rep(0.10,8760)    #???/kWh
# p_tr = rep(0.25,8760)           #???/kWh
p_facility_buy = rep(0,8760)    #???/kWh



Resumen_TEST2 = Resumen_costes_cont[1,]
Resumen_TEST2 = cbind(Distrib = NA, PerfilGEN = NA, Resumen_TEST2)
Resumen_TEST2 = Resumen_TEST2[-1,]

for (i in 1:ncol(Conjunto_Distrib)) {
  
  for (u in 1:length(Conjunto_PERFILGEN)) {
    
    Distrib = Conjunto_Distrib[,i]
    PERFILGEN = Conjunto_PERFILGEN[u]
    
    Resultado = Costes_price_based(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN,p_tr_case)
    
    Resumen_costes_cont = (Resultado$Resumen_costes)
    Resumen_costes_cont[c(4:9,11:16)] = round(as.numeric(unlist(Resumen_costes_cont[c(4:9,11:16)])),2)
    
    nombreresultado = paste("Resumen ",colnames(Conjunto_Distrib)[i],"PERF",(Conjunto_PERFILGEN)[u])
    # 
    # assign(paste("Resumen ",colnames(Conjunto_Distrib)[i],"PERF",(Conjunto_PERFILGEN)[u]),
    #        Resumen_costes_cont)
    
    Temp = data.frame(Distrib = colnames(Conjunto_Distrib)[i],
                      PerfilGEn = PERFILGEN,
                      Resumen_costes_cont)
    
    Resumen_TEST2 = rbind(Resumen_TEST2, Temp[N+1,])
    
    # GraficoPLOT = Resultado$GraficoPLOT
    # 
    nombregraf = paste0(nombreresultado,".jpeg")
    print(nombregraf)

    
  }
}

path = paste0(getwd(),"/")
nombrefile = paste0("Resumen_TEST2_pbuy_",p_grid_buy[1],"_psell_",p_grid_sell[1],"_N-",N,"_P-",PERFILGEN," ",rightnow,".RData")
print(nombrefile)
save(Resumen_TEST2,file=nombrefile)


#------ GRAFICAS COMPARATIVAS DEL PAPER  -------------------------------------------------------------------------------------------------------------------------

source(paste0(texto1,"Modelo 2-graphs.R"))

# Ruta y nombre de la carpeta a crear
ruta_carpeta <- paste0(getwd(),"/Graficos")

# Verificar si la carpeta fue creada exitosamente
if (file.exists(ruta_carpeta)) {
  print("Ya existe la carpeta")
} else {
  dir.create(ruta_carpeta)
  print("Carpeta creada")
}


# 0. GRAFICO PERFILES DE GENERACION -----------------------------------------------------------------------------------------------------
GraficoPLOT_FILA0_FINAL = GraficoPLOT_PERFILES()
print(GraficoPLOT_FILA0_FINAL)


nombregraf = paste0("0. Perfiles consumo generacion ",rightnow,".jpeg")
print(nombregraf)
ggsave(plot = GraficoPLOT_FILA0_FINAL,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graficos"),
       device = "jpeg",
       height = 15, width = 30, dpi = 150, units = "cm")


# 1. GRAFICO COMPARATIVO DE ESCENARIOS DE COSTES -----------------------------------------------------------------------------------------------------
path = paste0(getwd(),"/")
(load(paste0(path,"Resumen_TEST2_pbuy_0.3_psell_0.1_N-100_P-51 05-07-2023 231114.RData")))
SimData = Resumen_TEST2

GraficoPLOT_FILA1_FINAL = GraficoPLOT_FILA1("Corr90", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA1("Corr70", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA1("Corr50", SimData,Num_perfiles_gen,N)

print(GraficoPLOT_FILA1_FINAL)

nombregraf = paste0("1. Escenarios de costes v1 ",rightnow,".jpeg")
print(nombregraf)
ggsave(plot = GraficoPLOT_FILA1_FINAL,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graficos"),
       device = "jpeg",
       height = 20, width = 45, dpi = 150, units = "cm")




# 2. GRAFICO COMPARATIVO DE AHORROS -----------------------------------------------------------------------------------------------------
path = paste0(getwd(),"/")
(load(paste0(path,"Resumen_TEST2_pbuy_1_psell_0.1_N-100_P-51 05-07-2023 230125.RData")))
SimData = Resumen_TEST2

GraficoPLOT_FILA2_FINAL = GraficoPLOT_FILA2("Corr90", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA2("Corr70", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA2("Corr50", SimData,Num_perfiles_gen,N)
print(GraficoPLOT_FILA2_FINAL)

nombregraf = paste0("2. Escenarios de ratio ahorro v2 ",rightnow,".jpeg")
print(nombregraf)
ggsave(plot = GraficoPLOT_FILA2_FINAL,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graficos"),
       device = "jpeg",
       height = 15, width = 35, dpi = 150, units = "cm")


# 3. GRAFICO COMPARATIVO DE EXCEDENTE -----------------------------------------------------------------------------------------------------
path = paste0(getwd(),"/")

(load(paste0(path,"Resumen_TEST2_pbuy_0.3_psell_0.1_N-100_P-51 05-07-2023 231114.RData")))
RESUMEN_pbuy_0.3 = Resumen_TEST2

(load(paste0(path,"Resumen_TEST2_pbuy_1_psell_0.1_N-100_P-51 05-07-2023 230125.RData")))
RESUMEN_pbuy_1 = Resumen_TEST2


GraficoPLOT_FILA3_FINAL = GraficoPLOT_FILA3("Corr90", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA3("Corr70", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA3("Corr50", SimData,Num_perfiles_gen,N)

print(GraficoPLOT_FILA3_FINAL)

nombregraf = paste0("3. Escenarios de surplus 2 escenarios ",rightnow,".jpeg")
print(nombregraf)
ggsave(plot = GraficoPLOT_FILA3_FINAL,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graficos"),
       device = "jpeg",
       height = 15, width = 35, dpi = 150, units = "cm")


# 4. GRAFICO COMPARATIVO DE ENERGIAS -----------------------------------------------------------------------------------------------------
path = paste0(getwd(),"/")
(load(paste0(path,"Resumen_TEST2_pbuy_1_psell_0.1_N-100_P-51 05-07-2023 230125.RData")))
SimData = Resumen_TEST2

GraficoPLOT_FILA4_FINAL = GraficoPLOT_FILA4("Corr90", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA4("Corr70", SimData,Num_perfiles_gen,N) + 
                          GraficoPLOT_FILA4("Corr50", SimData,Num_perfiles_gen,N)
print(GraficoPLOT_FILA4_FINAL)

nombregraf = paste0("4. Escenarios de energia ",rightnow,".jpeg")
print(nombregraf)
ggsave(plot = GraficoPLOT_FILA4_FINAL,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graficos"),
       device = "jpeg",
       height = 20, width = 45, dpi = 150, units = "cm")
