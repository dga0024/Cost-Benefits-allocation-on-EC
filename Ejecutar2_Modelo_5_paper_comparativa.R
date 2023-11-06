
#------   INICIALIZAR  -------------------------------------------------------------------------------------------------------------------------
if(!require('utils','tcltk')) {
  install.packages('utils','tcltk')
}

library('utils')
library('tcltk')

rm(list = ls())
cat("\014")

getwd()

#Establecer directorio de trabajo
if (interactive() && .Platform$OS.type == "windows") {
  dir = choose.dir(getwd(), "Choose a suitable folder")
  setwd(dir)
  texto1 = paste0(dir,"/")
  texto1 = as.character(chartr("/","\\",texto1))
} else {
  dir = file.choose()
  # dir = tk_choose.dir()
  
  dir3 = as.character(chartr("\\","/",dir))
  resultado <- strsplit(dir3, "/")
  AA = unlist(resultado)
  BB=""
  for (i in 1:length(AA)-1){
    if (i==1){
      BB = paste0(AA[i])
    }else {
      BB = paste0(BB,"/",AA[i])
    }
  }
  dir = BB
  setwd(dir)
  texto1 = paste0(dir,"/")
  
}


source(paste0(texto1,"Modelo_0_Inicializar.R"))

DATOS_INICIO = (Inicializar())
INITIAL_CONDITIONS = (Inicializar01())

p_grid_buy = rep(0.30,8760)     #???/kWh
p_grid_sell = rep(0.10,8760)    #???/kWh
# p_tr = rep(0.25,8760)           #???/kWh
p_facility_buy = rep(0,8760)    #???/kWh


#------   TEST 1  -------------------------------------------------------------------------------------------------------------------------
N = as.numeric(INITIAL_CONDITIONS[1])
Consumos_base = (DATOS_INICIO[,c(3,5,6)])
Distrib = c(rep(1,round(N/3)),
            rep(2,round(N/3)),
            rep(3,N - 2*round(N/3)))
Distrib = c(rep(2,N/2),rep(3,N/2))
Num_perfiles_gen = as.numeric(INITIAL_CONDITIONS[2])
PERFILGEN = as.numeric(INITIAL_CONDITIONS[3])

{
p_tr_case = "continuo"
lambda = 0.5
Resultado = Costes_price_based(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN,p_tr_case)
Resumen_costes_cont = Resultado$Resumen_costes
Resumen_surplus_cont = Resultado$Resumen_surplus

p_tr_case = "discontinuo"
Resultado = Costes_price_based(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN,p_tr_case)
Resumen_costes_discont = Resultado$Resumen_costes
Resumen_surplus_discont = Resultado$Resumen_surplus

Resultado = Costes_all_as_one(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN)
Resumen_costes_all_as_one = Resultado$Resumen_costes
Resumen_surplus_all_as_one = Resultado$Resumen_surplus

Resultado = Costes_surplus_ownership(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN)
Resumen_costes_ownership = Resultado$Resumen_costes
Resumen_surplus_ownership= Resultado$Resumen_surplus

Resultado = Costes_bill_sharing(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN)
Resumen_costes_bill_sharing = Resultado$Resumen_costes
Resumen_surplus_bill_sharing= Resultado$Resumen_surplus

Resultado = Costes_surplus_ownership_v2(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN)
Resumen_costes_ownership_v2 = Resultado$Resumen_costes
Resumen_surplus_ownership_v2= Resultado$Resumen_surplus
}


X_Resumen_GLOBAL = rbind(data.frame(Sim = rep("Price_based_cont",N),Resumen_costes_cont[1:N,]),
                         data.frame(Sim = rep("Price_based_discont",N),Resumen_costes_discont[1:N,]),
                         data.frame(Sim = rep("All_as_one",N),Resumen_costes_all_as_one[1:N,]),
                         data.frame(Sim = rep("Surplus_ownership",N),Resumen_costes_ownership[1:N,]),
                         data.frame(Sim = rep("Bill_sharing",N),Resumen_costes_bill_sharing[1:N,]))

X_Resumen_GLOBAL[c(5:10,12:17)] = round(as.numeric(unlist(X_Resumen_GLOBAL[c(5:10,12:17)])),2)


GraficoPLOT_FILA2 <- function() {
  
  n_breaks = 10
  # X_LABELS = seq(NA,n_breaks+2)
  # 
  # for (i in 1:length(X_LABELS)) {
  #   X_LABELS[i] = paste0((i-1)/(n_breaks)*100*2,"%")
  # }
  # 
  # X_LABELS[1] = "0%"
  
  
  override.shape <- c(16, 1, 4, 10)
  override.linetype <- c(1, 1, 1, 1)
  
  lim1 = N/2
  lim2 = N/2+1
  
  GraficoPLOT_FILA2 = 
    ggplot() +
    # ggtitle(paste0("Simulation of allocation systems - N=",N)) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tama?o relativo de la letra del t?tulo
                                     vjust=2, hjust = 0, #Justificaci?n vertical, para separarlo del gr?fico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separaci?n entre l?neas
    theme_bw() +
    geom_point(data = Resumen_costes_cont[1:lim1,],
              aes(x=seq(1,N/2,1), y=as.numeric(Cost_NoTrade), colour = "No_Trade"),
              size = 3, na.rm = TRUE, shape = 20) +
    geom_point(data = Resumen_costes_cont[lim2:N,],
              aes(x=seq(N/2+1,N,1), y=as.numeric(Cost_NoTrade), colour = "No_Trade"),
              size = 3, na.rm = TRUE, shape = 20) +
    
    geom_point(data = Resumen_costes_cont[1:lim1,],
               aes(x=seq(1,N/2,1), y=as.numeric(Cost_Trade), colour = "Mid-price"),
               size = 3, na.rm = TRUE, shape = 1) +
    geom_point(data = Resumen_costes_cont[lim2:N,],
               aes(x=seq(N/2+1,N,1), y=as.numeric(Cost_Trade), colour = "Mid-price"),
               size = 3, na.rm = TRUE, shape = 1) +
    
    geom_point(data = Resumen_costes_bill_sharing[1:lim1,],
               aes(x=seq(1,N/2,1), y=as.numeric(Cost_Trade), colour = "Bill_Sharing"),
               size = 3, na.rm = TRUE, shape = 4) +
    geom_point(data = Resumen_costes_bill_sharing[lim2:N,],
               aes(x=seq(N/2+1,N,1), y=as.numeric(Cost_Trade), colour = "Bill_Sharing"),
               size = 3, na.rm = TRUE, shape = 4) +
    
    geom_point(data = Resumen_costes_ownership_v2[1:lim1,],
               aes(x=seq(1,N/2,1), y=as.numeric(Cost_Trade), colour = "Surplus-Based"),
               size = 3, na.rm = TRUE, shape = 10) +
    geom_point(data = Resumen_costes_ownership_v2[lim2:N,],
               aes(x=seq(N/2+1,N,1), y=as.numeric(Cost_Trade), colour = "Surplus-Based"),
               size = 3, na.rm = TRUE, shape = 10) +
    
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Participant's ID", y = "Cost (Euro)",color = "") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(1.5))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    scale_x_continuous(breaks = waiver(), n.breaks = n_breaks,
                       limit = c(1,N),
                       labels = waiver()) +
    scale_y_continuous(breaks = waiver(), limit = c(250,450)) +
    scale_color_manual(values = c("No_Trade" = "black",
                                  "Mid-price" = "dodgerblue",
                                  "Bill_Sharing" = "olivedrab",
                                  "Surplus-Based" = "violetred"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=15,face="bold"),
          legend.text = element_text(colour="black", size=15,face="bold"),
          legend.direction = "horizontal") +
   guides(colour = guide_legend(override.aes = list(shape = override.shape, 
                                                    linetype = override.linetype,
                                                    size = 4)))+
   scale_shape(guide = "none") +
   scale_linetype(guide = "none")
  
  # print(GraficoPLOT_FILA2)
  
}

# Ruta y nombre de la carpeta a crear
ruta_carpeta <- paste0(getwd(),"/Graphs")

# Verificar si la carpeta fue creada exitosamente
if (file.exists(ruta_carpeta)) {
  print("Folder alreagy exists")
} else {
  dir.create(ruta_carpeta)
  print("Folder created")
}


GraficoPLOT = GraficoPLOT_FILA2()
print(GraficoPLOT)

rightnow = as.character(format(Sys.time(), "%Y-%m-%d %H-%M-%S"))
nombregraf = paste0("Fig11. Allocation systems simulation ",rightnow,".jpeg")
print(nombregraf)

ggsave(plot = GraficoPLOT,   #nombre de la gr?fica en R
       filename=nombregraf,
       path = paste0(getwd(),"/Graphs"),
       device = "jpeg",
       height = 15, width = 35, dpi = 150, units = "cm")

  