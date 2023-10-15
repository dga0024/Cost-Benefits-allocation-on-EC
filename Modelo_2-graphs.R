
#FUNCIONES   ##################################################################################################################

GraficoPLOT_PERFILES <- function() {
  
  # A = round(max(as.numeric((Resumen_TEST2$Surplus))),-2)
  # B = round(min(as.numeric((Resumen_TEST2$Surplus))),-2)
  # X_MAX = round(A,(-1)*(nchar(A)-1)) + 10^(nchar(A)-1)
  #
  # if (B < 0){
  #   X_MIN = round(B,(-1)*(nchar(A)-1)) - 10^(nchar(A)-1)
  # }else {
  #   X_MIN = 0
  # }
  # DIV = 10^(nchar(A)-1)
  n_breaks = 24
  X_LABELS = seq(1,n_breaks,1)
  
  override.shape <- c(16, 17, 18, 15)
  override.linetype <- c(1, 1, 1, 1)
  
  
  GraficoPLOT_FILA3 =
    ggplot() +
    ggtitle(paste0("Energy profiles")) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tamaño relativo de la letra del título
                                     vjust=2, hjust = 0, #Justificación vertical, para separarlo del gráfico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=2)) + #Separación entre líneas
    theme_bw() +
    geom_line(data = Consumos_base[1:24,],
              aes(x=seq(1,24,1), y=`Consumo tipo Gauss`, colour = "Gauss"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Consumos_base[1:24,],
               aes(x=seq(1,24,1), y=`Consumo tipo Gauss`, colour = "Gauss"),
               size = 4, na.rm = TRUE, shape = 16) +
    geom_line(data = Consumos_base[1:24,],
              aes(x=seq(1,24,1), y=`Consumo tipo mercado`, colour = "anti-Gauss"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Consumos_base[1:24,],
               aes(x=seq(1,24,1), y=`Consumo tipo mercado`, colour = "anti-Gauss"),
               size = 4, na.rm = TRUE, shape = 17) +
    geom_line(data = Generacion_base[1:24,],
              aes(x=seq(1,24,1), y=`Prod solar 1kWp`*0.64, colour = "PV_coverage_50%"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Generacion_base[1:24,],
               aes(x=seq(1,24,1), y=`Prod solar 1kWp`*0.64, colour = "PV_coverage_50%"),
               size = 5, na.rm = TRUE, shape = 18) +
    geom_line(data = Generacion_base[1:24,],
              aes(x=seq(1,24,1), y=`Prod solar 1kWp`*1.28, colour = "PV_coverage_100%"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Generacion_base[1:24,],
               aes(x=seq(1,24,1), y=`Prod solar 1kWp`*1.28, colour = "PV_coverage_100%"),
               size = 4, na.rm = TRUE, shape = 15) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Hours", y = "Energy (kWh)",color = "") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(1.5))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    scale_x_continuous(breaks = waiver(), n.breaks = n_breaks,
                       limit = c(1,24),
                       labels = waiver()) +
    scale_y_continuous(breaks = seq(0,1000, 100), limit = c(0,1000)) +
    scale_color_manual(values = c("Gauss" = "dodgerblue",
                                  "anti-Gauss" = "olivedrab",
                                  "PV_coverage_50%" = "brown",
                                  "PV_coverage_100%" = "red"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=15,face="bold"),
          legend.text = element_text(colour="black", size=15,face="bold")) +
    guides(colour = guide_legend(override.aes = list(shape = override.shape,
                                                     linetype = "blank",
                                                     size = 3)))+
    scale_shape(guide = "none") +
    scale_linetype(guide = "none")
  
  # print(GraficoPLOT_FILA3)
  
}

GraficoPLOT_FILA1 <- function(Corr_case, Resumen_TEST2,Num_perfiles_gen,N) {
  
  A = round(max(as.numeric((Resumen_TEST2$Cost_original))),-2)
  B = round(min(as.numeric((Resumen_TEST2$Cost_Trade))),-2)
  X_MAX = round(A,(-1)*(nchar(A)-1)) + 10^(nchar(A)-1)
  
  if (B < 0){
    X_MIN = round(B,(-1)*(nchar(A)-1)) - 10^(nchar(A)-1)
  }else {
    X_MIN = 0
  }
  DIV = 10^(nchar(A)-1)
  
  X_LABELS = rep(NA,Num_perfiles_gen)
  
  for (i in 1:Num_perfiles_gen) {
    X_LABELS[i] = paste0((i-1)/(Num_perfiles_gen-1)*100*2,"%")
  }
  
  override.shape <- c(16, 17,3)
  
  GraficoPLOT_FILA1 = 
    ggplot() +
    ggtitle(paste0("Costs ",Corr_case," - N=",N)) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tamaño relativo de la letra del título
                                     vjust=2, hjust = 0, #Justificación vertical, para separarlo del gráfico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separación entre líneas
    theme_bw() +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Cost_original, colour = "Cost_original"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Cost_NoTrade, colour = "Cost_NoTrade" ),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Cost_Trade, colour = "Cost_Trade" ),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Cost_original, colour = "Cost_original"),
               size = 3, na.rm = TRUE, shape = 16) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Cost_NoTrade, colour = "Cost_NoTrade" ),
               size = 3, na.rm = TRUE, shape = 17) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Cost_Trade, colour = "Cost_Trade" ),
               size = 3, na.rm = TRUE, shape = 3) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = NULL, y = "Money units",color = "Costs") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    scale_x_continuous(breaks = seq(1,Num_perfiles_gen,1), 
                       limit = c(1,Num_perfiles_gen),
                       labels = X_LABELS) +
    scale_y_continuous(breaks = seq(X_MIN,X_MAX, DIV), limit = c(X_MIN,X_MAX)) + 
    scale_color_manual(values = c("Cost_original" = "dodgerblue",
                                  "Cost_NoTrade" = "red",
                                  "Cost_Trade" = "black"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=10,face="bold")) +
    guides(colour = guide_legend(override.aes = list(shape = override.shape,
                                                     linetype = "blank",
                                                     size = 3)))+
    scale_shape(guide = "none") +
    scale_linetype(guide = "none")
  
  # print(GraficoPLOT_FILA1)
  
}

GraficoPLOT_FILA2 <- function(Corr_case, Resumen_TEST2,Num_perfiles_gen,N) {
  
  n_breaks = 10
  X_LABELS = rep(NA,n_breaks+2)
  
  for (i in 1:length(X_LABELS)) {
    X_LABELS[i] = paste0((i-1)/(n_breaks)*100*2,"%")
  }
  
  X_LABELS[1] = "0%"
  
  Corr_case1 = switch(  
    Corr_case,  
    "Corr90"= "10G-90aG",  
    "Corr70"= "30G-70aG",
    "Corr50"= "50G-50aG")
  
  override.shape <- c(17, 16)
  
  GraficoPLOT_FILA2 = 
    ggplot() +
    ggtitle(paste0("Savings(%) - ",Corr_case1)) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tamaño relativo de la letra del título
                                     vjust=2, hjust = 0, #Justificación vertical, para separarlo del gráfico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separación entre líneas
    theme_bw() +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Surplus_ratiox100_FV, colour = "PV + No_Trade"),
              size = 0.8, na.rm = TRUE,lty = 1) +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Surplus_ratiox100_ALLOC+Surplus_ratiox100_FV, colour = "PV + Mid-Price" ),
              size = 0.8, na.rm = TRUE,lty = 1) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Surplus_ratiox100_FV, colour = "PV + No_Trade"),
               size = 2.5, na.rm = TRUE, shape = 16) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Surplus_ratiox100_ALLOC+Surplus_ratiox100_FV, colour = "PV + Mid-Price" ),
               size = 2.5, na.rm = TRUE, shape = 17) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Fraction of consumption coverage with PV system", y = "Savings (%)",color = "") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    scale_x_continuous(breaks = waiver(), n.breaks = n_breaks,
                       limit = c(1,Num_perfiles_gen),
                       labels = X_LABELS) +
    scale_y_continuous(breaks = seq(0,65, 10), limit = c(0,65)) + 
    scale_color_manual(values = c("PV + Mid-Price" = "dodgerblue",
                                  "PV + No_Trade" = "firebrick"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=15,face="bold"),
          legend.text = element_text(colour="black", size=12,face="bold")) +
    guides(colour = guide_legend(override.aes = list(shape = override.shape,
                                                     linetype = "blank",
                                                     size = 5)))+
    scale_shape(guide = "none") +
    scale_linetype(guide = "none")
  
  
  # print(GraficoPLOT_FILA2)
}

GraficoPLOT_FILA3 <- function(Corr_case, Resumen_TEST2,Num_perfiles_gen,N) {
  
  A = round(max(as.numeric((Resumen_TEST2$Surplus))),-2)
  B = round(min(as.numeric((Resumen_TEST2$Surplus))),-2)
  X_MAX = round(A,(-1)*(nchar(A)-1)) + 10^(nchar(A)-1)
  
  if (B < 0){
    X_MIN = round(B,(-1)*(nchar(A)-1)) - 10^(nchar(A)-1)
  }else {
    X_MIN = 0
  }
  DIV = 10^(nchar(A)-1)
  
  n_breaks = 10
  X_LABELS = rep(NA,n_breaks+2)
  
  for (i in 1:length(X_LABELS)) {
    X_LABELS[i] = paste0((i-1)/(n_breaks)*100*2,"%")
  }
  
  X_LABELS[1] = "0%"
  
  Corr_case1 = switch(  
    Corr_case,  
    "Corr90"= "10G-90aG",  
    "Corr70"= "30G-70aG",
    "Corr50"= "50G-50aG")
  
  override.shape <- c(17, 16)
  
  GraficoPLOT_FILA3 = 
    ggplot() +
    ggtitle(paste0("Surplus_trading - ",Corr_case1)) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(0.8), #Tamaño relativo de la letra del título
                                     vjust=2, hjust = 0, #Justificación vertical, para separarlo del gráfico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separación entre líneas
    theme_bw() +
    geom_line(data = RESUMEN_pbuy_1[which(RESUMEN_pbuy_1$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Surplus, colour = "High_Spread"),
              size = 0.75, na.rm = TRUE,lty = 1) +
    geom_point(data = RESUMEN_pbuy_1[which(RESUMEN_pbuy_1$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Surplus, colour = "High_Spread"),
               size = 2, na.rm = TRUE, shape = 17) +
    geom_line(data = RESUMEN_pbuy_0.3[which(RESUMEN_pbuy_0.3$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Surplus, colour = "Low_Spread"),
              size = 0.75, na.rm = TRUE,lty = 1) +
    geom_point(data = RESUMEN_pbuy_0.3[which(RESUMEN_pbuy_0.3$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Surplus, colour = "Low_Spread"),
               size = 2, na.rm = TRUE, shape = 16) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Fraction of consumption coverage with PV system", y = "Surplus (???)",color = "") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(0.95))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1)))+
    scale_x_continuous(breaks = waiver(), n.breaks = n_breaks,
                       limit = c(1,Num_perfiles_gen),
                       labels = X_LABELS) +
    scale_y_continuous(breaks = seq(0,12000, 2000), limit = c(0,12000)) + 
    scale_color_manual(values = c("High_Spread" = "dodgerblue",
                                  "Low_Spread" = "firebrick"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=15,face="bold"),
          legend.text = element_text(colour="black", size=12,face="bold"),
          legend.direction = "horizontal") +
    guides(colour = guide_legend(override.aes = list(shape = override.shape,
                                                     linetype = "blank",
                                                     size = 5)))+
    scale_shape(guide = "none") +
    scale_linetype(guide = "none")
  
  # print(GraficoPLOT_FILA3)
  
}

GraficoPLOT_FILA4 <- function(Corr_case, Resumen_TEST2,Num_perfiles_gen,N) {
  
  A = round(max(as.numeric((Resumen_TEST2$Consumo_en_sol))),-2)
  B = round(min(as.numeric((Resumen_TEST2$Consumo_en_sol))),-2)
  X_MAX = round(A,(-1)*(nchar(A)-1)) + 10^(nchar(A)-1)
  
  if (B < 0){
    X_MIN = round(B,(-1)*(nchar(A)-1)) - 10^(nchar(A)-1)
  }else {
    X_MIN = 0
  }
  DIV = 10^(nchar(A)-1)
  
  X_LABELS = rep(NA,Num_perfiles_gen)
  
  for (i in 1:Num_perfiles_gen) {
    X_LABELS[i] = paste0((i-1)/(Num_perfiles_gen-1)*100*2,"%")
  }
  
  CONS_SOL_0 = which(Resumen_TEST2$Consumo_en_sol==0)
  
  Resumen_TEST2$Consumo_en_sol[CONS_SOL_0] = Resumen_TEST2$Consumo_en_sol[CONS_SOL_0+1]
  
  override.shape <- c(16, 17,3)
  
  GraficoPLOT_FILA4 = 
    ggplot() +
    # ggtitle(paste0("Costs ",Corr_case," - N=",N)) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tamaño relativo de la letra del título
                                     vjust=2, hjust = 0, #Justificación vertical, para separarlo del gráfico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separación entre líneas
    theme_bw() +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=Consumo_en_sol, colour = "Consumo_horas_sol"),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=E_autoconsumida, colour = "E_autoconsumida(AC)" ),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_line(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
              aes(x=seq(1,Num_perfiles_gen,1), y=E_autoconsumida+E_transfer_NC, colour = "AC+Trading" ),
              size = 1, na.rm = TRUE,lty = 1) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=Consumo_en_sol, colour = "Consumo_horas_sol"),
               size = 3, na.rm = TRUE, shape = 16) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=E_autoconsumida, colour = "E_autoconsumida(AC)" ),
               size = 3, na.rm = TRUE, shape = 17) +
    geom_point(data = Resumen_TEST2[which(Resumen_TEST2$Distrib == Corr_case),],
               aes(x=seq(1,Num_perfiles_gen,1), y=E_autoconsumida+E_transfer_NC, colour = "AC+Trading" ),
               size = 3, na.rm = TRUE, shape = 3) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Demand coverage Ratio with FV", y = "Energy units",color = "Energy item") +
    theme(axis.text.x=element_text(angle=45, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    scale_x_continuous(breaks = seq(1,Num_perfiles_gen,1), 
                       limit = c(1,Num_perfiles_gen),
                       labels = X_LABELS) +
    scale_y_continuous(breaks = seq(X_MIN,X_MAX, DIV), limit = c(X_MIN,X_MAX)) + 
    scale_color_manual(values = c("Consumo_horas_sol" = "dodgerblue",
                                  "E_autoconsumida(AC)" = "red",
                                  "AC+Trading" = "black"))+
    theme(legend.position = "top", legend.title = element_text(colour="black", size=10,face="bold")) +
    guides(colour = guide_legend(override.aes = list(shape = override.shape,
                                                     linetype = "blank",
                                                     size = 3)))+
    scale_shape(guide = "none") +
    scale_linetype(guide = "none")
  
  # print(GraficoPLOT_FILA4)
  
}





