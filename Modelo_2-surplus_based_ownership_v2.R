
#FUNCIONES   ##################################################################################################################

Costes_surplus_ownership_v2 <- function(N, Consumos_base, Distrib, Num_perfiles_gen, PERFILGEN) {

  Agentes <- (Consumos_base[,Distrib])/1000
  Agentes <- sapply(Agentes, unlist)
  for (i in 1:N) {
    colnames(Agentes)[i] = as.character(paste("Agente_",i))
  }
  
  # SUMAS = colSums(Agentes)
  # plot(sort(SUMAS))
  # 
  # plot(Agentes[,1], type = "l")
  # 
  # for (i in 1:N) {
  #   plot(Agentes[,i], type = "l")
  #   
  # }
  
  EA_C_kWh = sum(colSums(Agentes))
  
  # Ecosistema_agentes = as.data.frame(rowSums(Agentes))
  
  
  #Generacion de perfiles de generacion
  Generacion_base = (DATOS_INICIO[,2])  # Se han filtrado las 100 primeras filas
  
  Ratio_FV_base = EA_C_kWh/(sum(Generacion_base)/1000)
  Multiplicador_cobertura <- seq(0,2, length=Num_perfiles_gen)
  
  # PERFILGEN = 3
  
  Perfiles_gen <- as.data.frame(matrix(ncol = Num_perfiles_gen, nrow = nrow(Consumos_base)))
  
  for (i in 1:Num_perfiles_gen) {
    colnames(Perfiles_gen)[i] = as.character(paste("Escenario",i," - ",round(Multiplicador_cobertura[i]*100,0),""))
    Perfiles_gen[,i] = Generacion_base[,1]/1000*Ratio_FV_base*Multiplicador_cobertura[i]
    
  }
  
  # SUMAS_GEN = colSums(Perfiles_gen)
  # plot(sort(SUMAS_GEN))
  
  EA_G_kWh = sum(Perfiles_gen[,PERFILGEN])
  
  
  #Balances de los agentes
  Coef_reparto = 1/N #Considerando todos iguales
  Coef_reparto_Matrix = matrix(data = Coef_reparto, nrow = nrow(Agentes), ncol = N)
  
  Coef_reparto_Matrix_v2 = Coef_reparto_Matrix
  
  for (i in 1:ncol(Coef_reparto_Matrix)) {
    Coef_reparto_Matrix_v2[,i] = rep(i/((N+1)*(N/2)),nrow(Coef_reparto_Matrix))
  }
  

  colnames(Coef_reparto_Matrix) = colnames(Agentes)
  
  Agentes_Balance <- Agentes - Perfiles_gen[,PERFILGEN]*Coef_reparto
  NP = (Agentes_Balance <= 0)
  
  ID_NP = which(NP=="TRUE")
  ID_NC = which(NP=="FALSE")
  
  TempNC = Agentes_Balance
  TempNC[ID_NP] = TempNC[ID_NP]*0
  
  TempNP = Agentes_Balance
  TempNP[ID_NC] = TempNP[ID_NC]*0
  
  E_NC = as.data.frame(apply(TempNC, MARGIN = 1, FUN = sum))
  E_NP = as.data.frame(apply(TempNP, MARGIN = 1, FUN = sum))
  
  
  E_NC = rowSums(TempNC)
  E_NP = rowSums(TempNP)
  
  TempET = data.frame(abs(E_NC),abs(E_NP))
  E_TRANSFER = as.data.frame(apply(TempET, MARGIN = 1, FUN = min))
  
  EA_tr_kWh = sum(E_TRANSFER)
  
  
  Agentes_E_traded = NP
  
  TempNC_v2 = TempNC/rowSums(TempNC)
  TempNP_v2 = TempNP/rowSums(TempNP)
  
  E_TRANSFER_NC = TempNC_v2
  E_TRANSFER_NP = TempNP_v2
  
  for (i in 1:nrow(NP)) {
    E_TRANSFER_NC[i,] = (-1) * TempNC_v2[i,]  * E_TRANSFER[i,1]
    E_TRANSFER_NP[i,] = (+1) * TempNP_v2[i,]  * E_TRANSFER[i,1] 
  }
  
  Agentes_E_traded = E_TRANSFER_NC + E_TRANSFER_NP
  
  Temp4 = which(Agentes_E_traded=="NaN")
  Agentes_E_traded[Temp4] = 0
  Temp4 = which(E_TRANSFER_NC=="NaN")
  E_TRANSFER_NC[Temp4] = 0
  Temp4 = which(E_TRANSFER_NP=="NaN")
  E_TRANSFER_NP[Temp4] = 0
  
  sum(abs(na.omit(Agentes_E_traded))/2)
  sum(E_TRANSFER)
  
  #C�lculo de los costes
  
  #NO TRADING
  COST_NP = TempNP * p_grid_sell
  COST_NC = TempNC * p_grid_buy
  COST_FAC = Agentes/Agentes * Perfiles_gen[,PERFILGEN]*Coef_reparto * p_facility_buy
  
  Cost_NoTrade = COST_NP + COST_NC + COST_FAC
  
  
  Cost_NoTrade_TOTAL1 = sum(colSums(Cost_NoTrade))
  
  SUMAS_NOTRADE = colSums(Cost_NoTrade)
  # plot(sort(SUMAS_NOTRADE))
  
  

  #TRADING
  E_producida = as.data.frame(Perfiles_gen[,PERFILGEN])
  E_consumida = as.data.frame(rowSums(Agentes))
  
  Surplus_total = sum((p_grid_buy-p_grid_sell)*E_TRANSFER)
  Surplus = ((p_grid_buy-p_grid_sell)*E_TRANSFER)
  
  Surplus_Agentes_reparto = Agentes*0
  
  
  
  for (i in 1:nrow(Surplus)) {
    Surplus_Agentes_reparto[i,] = Coef_reparto_Matrix_v2[i,] * Surplus[i,1]
    
  }
  
  Cost_Trade = Cost_NoTrade - Surplus_Agentes_reparto
  Cost_Trade_TOTAL1 = sum(Cost_Trade)
  
  
  #SURPLUS
  # Surplus = Cost_NoTrade - Cost_Trade
  
  Surplus1 = Cost_NoTrade_TOTAL1 - Cost_Trade_TOTAL1
  Surplus2 = sum((p_grid_buy-p_grid_sell)*E_TRANSFER)
  
  Surplus_PRUEBA = Cost_NoTrade - Cost_Trade
  Surplus3 = sum(Surplus_PRUEBA)
  
  Surplus_ratio = Surplus3/Cost_Trade_TOTAL1*100
  
  
  #SURPLUS DISTRIBUTION
  Surplus_NP = Surplus_Agentes_reparto
  Surplus_NP[ID_NC] = Surplus_NP[ID_NC]*0
  
  Surplus_NC = Surplus_Agentes_reparto
  Surplus_NC[ID_NP] = Surplus_NC[ID_NP]*0
  
  Surplus_sum = as.data.frame(apply(Surplus, MARGIN = 1, FUN = sum))
  Surplus_NP_sum = as.data.frame(apply(Surplus_NP, MARGIN = 1, FUN = sum))
  Surplus_NC_sum = as.data.frame(apply(Surplus_NC, MARGIN = 1, FUN = sum))
  
  Surplus_NP_reparto = Surplus_NP_sum/Surplus_sum
  Surplus_NC_reparto = Surplus_NC_sum/Surplus_sum
  
  Surplus_Agentes_NP = as.data.frame(apply(Surplus_NP, MARGIN = 2, FUN = sum))
  Surplus_Agentes_NC = as.data.frame(apply(Surplus_NC, MARGIN = 2, FUN = sum))
  Surplus_Agentes = as.data.frame(apply(Surplus, MARGIN = 2, FUN = sum))
  
  # Surplus_NP1 = E_TRANSFER * (p_tr - p_grid_sell)
  # Surplus_NC1 = E_TRANSFER * (p_grid_buy - p_tr)
  # 
  # Surplus_NC1 + Surplus_NP1
  
  Surplus_NP2 = sum(Cost_NoTrade[ID_NP]) - sum(Cost_Trade[ID_NP])
  Surplus_NC2 = sum(Cost_NoTrade[ID_NC]) - sum(Cost_Trade[ID_NC])
  

  
  # RESUMEN DE COSTES
  
  E_autoconsumida = as.data.frame(Coef_reparto_Matrix)
  
  for (i in 1:ncol(E_autoconsumida)) {
    E_autoconsumida[,i] = E_autoconsumida[,i] * E_producida
  }
  
  for (i in 1:ncol(E_autoconsumida)) {
    ID_Excedente = which(E_autoconsumida[,i] > Agentes[,i])
    E_autoconsumida[ID_Excedente,i] = Agentes[ID_Excedente,i]
  }
  
  
  sum(E_autoconsumida)
  sum(E_producida)
  sum(E_consumida)
  
  Cost_original = as.data.frame(Agentes*p_grid_buy)
  
  RESUMEN = data.frame(Tipo_Agente = colnames(Consumos_base)[Distrib],
                       Cost_original = colSums(Cost_original),
                       Cost_NoTrade = colSums(Cost_NoTrade),
                       Cost_Trade = colSums(Cost_Trade),
                       Surplus = as.numeric(colSums(Cost_NoTrade) - colSums(Cost_Trade)),
                       Surplus_ratiox100_FV = ((colSums(Cost_original) - colSums(Cost_Trade))/colSums(Cost_original))*100,
                       Surplus_ratiox100_ALLOC = ((colSums(Cost_NoTrade) - colSums(Cost_Trade))/colSums(Cost_original))*100,
                       Part_beneficiosa = (colSums(Cost_NoTrade) > colSums(Cost_Trade)),
                       Consumo_en_sol = colSums(Agentes[which(E_producida > 0),]),
                       E_producida = sum(E_producida),
                       E_consumida = sum(E_consumida),
                       E_autoconsumida= colSums(E_autoconsumida),
                       E_transfer_NC = abs(colSums(E_TRANSFER_NC)),
                       E_transfer_NP = colSums(E_TRANSFER_NP))
  
  RESUMEN2 = data.frame(Num_Agente = seq(1,N,1),
                        Escenario = rep(paste0(colnames(Perfiles_gen[PERFILGEN]),"% N=",N),N),
                        RESUMEN[order(RESUMEN$Cost_NoTrade),])
  
  TOTALES = c(length(RESUMEN2$Num_Agente),
              paste0(colnames(Perfiles_gen[PERFILGEN]),"% N=",N),
              " ",
              sum(RESUMEN2$Cost_original), 
              sum(RESUMEN2$Cost_NoTrade), 
              sum(RESUMEN2$Cost_Trade), 
              sum(RESUMEN2$Surplus),
              mean(RESUMEN2$Surplus_ratiox100_FV),
              mean(RESUMEN2$Surplus_ratiox100_ALLOC),
              length(which(RESUMEN2$Part_beneficiosa=="TRUE"))/N,
              sum(RESUMEN2$Consumo_en_sol),
              sum(RESUMEN2$E_producida),
              sum(RESUMEN2$E_consumida),
              sum(RESUMEN2$E_autoconsumida),
              sum(RESUMEN2$E_transfer_NC),
              sum(RESUMEN2$E_transfer_NP))
  
  RESUMEN3 = rbind(RESUMEN2, TOTALES = TOTALES)
  
  
  # RESUMEN DE SURPLUS
  RESUMEN_Surplus = data.frame(Surplus_total = rowSums(Surplus),
                       NC = Surplus_NC_sum,
                       NP = Surplus_NP_sum,
                       Reparto_NP = Surplus_NP_reparto,
                       Reparto_NC = Surplus_NC_reparto)
  colnames(RESUMEN_Surplus) = c("Surplus_total", "NC", "NP","Reparto_NP","Reparto_NC")
  

  #Datos para sacar grafico
  
  A = round(max(as.numeric((RESUMEN2$Cost_NoTrade))),-2)
  B = round(min(as.numeric((RESUMEN2$Cost_Trade))),-2)
  X_MAX = round(A,(-1)*(nchar(A)-1)) + 10^(nchar(A)-1)
  
  if (B < 0){
    X_MIN = round(B,(-1)*(nchar(A)-1)) - 10^(nchar(A)-1)
  }else {
    X_MIN = 0
  }
  DIV = 10^(nchar(A)-1)
  
  
  #SACAR GRAFICOS COSTES
  
  titulo = paste0("SURPLUS-OWNERSHIP  Gen ",colnames(Perfiles_gen[PERFILGEN]),"x100 N=",N," ",p_tr_case)
  
  GraficoPLOT1 = 
    ggplot(data = RESUMEN2, aes(x=seq(1,N,1), y=Cost_NoTrade, colour = "Cost_NoTrade")) +
    labs(title = paste0("Costes "), 
         subtitle = titulo) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tama�o relativo de la letra del t�tulo
                                     vjust=2, hjust = 0, #Justificaci�n vertical, para separarlo del gr�fico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5)) + #Separaci�n entre l�neas
    geom_line(size = 0.5, na.rm = TRUE) +
    geom_line(data = RESUMEN2,aes(x=seq(1,N,1), y=Cost_Trade,colour = "Cost_Trade" ),
              size = 0.5, na.rm = TRUE,lty = 1) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Agentes", y = "Surplus") +
    scale_x_continuous(breaks = seq(1,N,1), limit = c(1,N)) +
    scale_y_continuous(breaks = seq(0,X_MAX, DIV), limit = c(0,X_MAX)) +    
    scale_color_manual(values = c("Cost_NoTrade" = "red", "Cost_Trade" = "blue"))+
    theme(legend.position="top",legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 12, face = "bold"))
  # 
  # print(GraficoPLOT1)

  
 
  #SACAR GRAFICOS SURPLUS
  GraficoPLOT2 = 
    ggplot(data = RESUMEN_Surplus, aes(x=seq(1,nrow(RESUMEN_Surplus),1), y=Reparto_NP, colour = "Reparto_NP")) +
    labs(title = paste0("Reparto Surplus "), 
         subtitle = titulo) +
    theme (plot.title = element_text(family="Arial",
                                     size=rel(1.5), #Tama�o relativo de la letra del t�tulo
                                     vjust=2, hjust = 0, #Justificaci�n vertical, para separarlo del gr�fico
                                     face="bold", #Letra negrilla. Otras posibilidades "plain", "italic", "bold" y "bold.italic"
                                     color="black", #Color del texto
                                     lineheight=1.5), #Separaci�n entre l�neas
                                      ) + 
    geom_point(size = 3, na.rm = TRUE) +
    geom_point(data = RESUMEN_Surplus,aes(x=seq(1,nrow(RESUMEN_Surplus),1), y=Reparto_NC,colour = "Reparto_NC"),
               size = 1, na.rm = TRUE) +
    theme(axis.text.x=element_text(angle=0, hjust=1, colour = "black", size=rel(1))) +
    theme(axis.text.y=element_text(hjust=1, colour = "black", size=rel(1.5)))+
    labs(x = "Horas", y = "Reparto Surplus") +
    scale_x_continuous(breaks = seq(1,nrow(RESUMEN_Surplus),1000), limit = c(1,nrow(RESUMEN_Surplus))) +
    scale_y_continuous(breaks = seq(0,1,0.1), limit = c(0,1)) + 
    scale_color_manual(values = c("Reparto_NP" = "blue", "Reparto_NC" = "red"))+
    theme(legend.position="top",legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 12, face = "bold"))
    
  GraficoPLOT <- GraficoPLOT1 / GraficoPLOT2
  
  # print(GraficoPLOT)
  
  
  
  resultado <- list(Resumen_costes=RESUMEN3,
                    Resumen_surplus=RESUMEN_Surplus)
  
  return(resultado)
}

