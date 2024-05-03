
# 1. SELECCIÓN DE ACTIVOS ----------------------------------------------------


library(quantmod) #Librería para importar datos desde Yahoo Finance

tickers <- c("BIMBOA.MX", "ALFAA.MX", "GFNORTEO.MX", "GAPB.MX",
             "OMAB.MX", "ASURB.MX", "TLEVISACPO.MX", "ELEKTRA.MX",
             "CEMEXCPO.MX", "GCARSOA1.MX") # Tickers en YFinance de 10 acciones

close <- list() #Lista con precios de cierre

for (i in c(1:length(tickers))){
  
  getSymbols(tickers[i], src = "yahoo", 
             from = "2022-01-01", to = "2024-04-01", 
             auto.assign = TRUE) #Obtendrá los precios asociados a cada ticker
  
  close[[i]] <- Cl(get(tickers[i])) #Lista con los vectores de precio de cierre
} 

precios <- data.frame(close) # Convertimos la lista close a dataframe
names(precios) <- tickers # Renombramiento por cada columna



# 1.1 CÁLCULO DE RENDIMIENTOS ---------------------------------------------


#precios es el dataframe con los precios históricos de cada acción por columna.
#tickers es una lista de tickers con la que se formó el dataframe precios.

calcular_rendimientos <- function(precios){
  rendimientos <- list() #Almacenará rendimientos de cada activo
  for (i in 1:ncol(precios)){
    
    rend_indiv <- c() # Rendimientos del i-ésimo activo en la lista tickers
    
    for (k in 2:nrow(precios) ){#precios[,i][k] accede a la fila k de la columna i
      rend_indiv[k] <- (precios[,i][k]/precios[,i][k-1]) - 1 
    } #Este ciclo calcula los rendimientos
    
    rendimientos[[i]] <- rend_indiv #El elemento i de la lista es el vector de rendimientos del activo k
  }
  names(rendimientos) <- paste0('Rendimiento ', colnames(precios))
  return(rendimientos) #rendimientos es una lista
}




# 2. Valor en Riesgo No Paramétrico ---------------------------------------
# Se determinarán los valores en riesgo no paramétricos de cada activo

# VaR por Simulación Histórica
  # Sea c el nivel de confianza deseado
  # Sea h el horizonte de tiempo deseado
  # Sea cierre la serie de datos históricos

VaR_SH <- function(c, h, cierre) {
  df <- data.frame(matrix(nrow = length(cierre), ncol = 4)) #Genera un df vacío
  colnames(df) <- c('Activo', 'Rendimiento', 'Reevaluar', 'PL') #Nombra columnas
  df$Activo <- cierre
  
  #Este ciclo rellena la tabla
  for (i in c(2:length(cierre))){
    df$Rendimiento[i] <- (df$Activo[i]/df$Activo[i-1]) - 1
    df$Reevaluar[i] <- df$Activo[length(cierre)]*(1+df$Rendimiento[i])
    df$PL[i] <- df$Activo[length(cierre)]-df$Reevaluar[i]
  }
  
  #VaR en un día
  VaR <- quantile(df$PL[2:length(cierre)], c)
  return(VaR * sqrt(h))
}







# VaR por Simulación Montecarlo
  # Sea t el horizonte de tiempo deseado
  # Sea alpha el nivel de confianza deseado
  # Sea sim el número de simulaciones deseadas en la metodología
  # Sea cierre la serie de datos históricos

VaR_SM <- function(alpha, t, cierre, sim){
  
  VaR_rendnorm <- function(c,cierre){
    activo <- cierre
    rend <- c()
    rend_sim <- c()
    reeval <- c()
    pl <- c()
    
    for (i in c(2:length(activo))){
      
      rend[i] <- (activo[i]/activo[i-1])-1
      
    } # Esto obtiene los rendimientos por fecha, fila por fila
    
    m <- mean(rend, na.rm = TRUE) # Requerimos media y varianza de los rendimientos
    s <- sd(rend, na.rm = TRUE) # para simular rendimientos normales con esas características
    
    for (i in c(2: length(activo))){
      rend_sim[i] <- rnorm(1, mean = m, sd = s) # Simula un rendimiento N(m, s) para el periodo
      reeval[i] <- activo[length(activo)]*(1+rend_sim[i]) # Toma último precio y reevalua con simulación
      pl[i] <- activo[length(activo)]-reeval[i] #P&L con base en el precio simulado - último precio
    } # Este ciclo rellena cada fila
    
    VaR_def <- quantile(x = pl[2:length(pl)], c) #Definición de VaR (percentil de PL)
    return(VaR_def)
    
  }
  
  #Simulamos VaR para promediar y definirlo como VaR por simulación montecarlo
  V <- c() #Vector vacío para guardar simulaciones de VaR con rendimientos normales
  for (k in c(1:sim)) {
    var <- VaR_rendnorm(alpha, cierre)
    V <- c(V, var)
  }
  VaR_SM <- mean(V) #VaR Por simulación montecarlo con 'sim' simulaciones
  return(sqrt(t)*VaR_SM) 
}

# VaR_SM(alpha = .99, t = 1, cierre = precios$OMAB.MX, sim = 100) #Ejemplo






# VaR por Alisado Exponencial
  # Sea f(n) = alpha^(n-1)*beta la función de decaimiento exponencial
  # Sea t el horizonte de tiempo deseado
  # Sea c el nivel de confianza deseado
  # Sea cierre la serie de datos históricos

VaR_AE <- function(alpha, beta, t, c, cierre){
    activo <- cierre
    rend <- c()
    reeval <- c()
    pl <- c()
    periodo <- seq(from = length(activo), to = 1, by = -1) #Periodos desc
    alisado <- alpha**(periodo-1)*beta
    
    for (i in c(2:length(activo))){
      rend[i] <- (activo[i]/activo[i-1])-1
      reeval[i] <- activo[length(activo)]*(1+rend[i])
      pl[i] <- activo[length(activo)] - reeval[i]
    } # Esto rellena cada entrada de los vectores
    
    df <- data.frame(cbind(periodo, activo, reeval, pl, alisado)) #Columnas necesarias para Sx y Fx
    orden_filas <- order(-df$pl)  # Orden descendente en P&L
    df_ordenado <- df[orden_filas, ] # df que se utilizará 
    
    Sx <- c() #Aquí se guardará la función de supervivencia
    for (i in c(1:length(df$alisado))){
      Sx[i] <- sum(df_ordenado$alisado[i:length(df_ordenado$alisado)])
    }
    
    df_ordenado <- cbind(df_ordenado, Sx)
    indices_mayores_que_c <- which(Sx > c) #Vemos qué índices tienen un número más grande que c en Sx
    numero_mas_pequeno_mayor_que_c <- min(Sx[indices_mayores_que_c]) #El número más pequeño de ellos
    indice_numero_mas_pequeno <- which(Sx == numero_mas_pequeno_mayor_que_c) #Índice de dicho número
    VaR_c <- df_ordenado$pl[indice_numero_mas_pequeno] #Definimos VaR por metodología
    return(sqrt(t)*VaR_c) 
    #return(df_ordenado)
    
}    
  




# 3. Valor en Riesgo Paramétrico ------------------------------------------

# 3.1. VaR Individual -----------------------------------------------------

# VaR Paramétrico
  # Sea VaR = Fac*S*sigma*sqrt(t) donde:
  # p es el nivel de confianza
  # Fac el percentil p de la distribución N(0,1)
  # S el monto total de exposición al riesgo
  # sigma la desviación estándar de los rendimientos del activo
  # t el horizonte de tiempo del VaR
  

VaR_PAR <- function(p, t, activo){ # activo = vector con precios de cierre
  rends <- c()
  for (k in 2:length(activo) ){
    rends[k] <- (activo[k]/activo[k-1]) - 1 
  } #Rendimientos del activo
  
  Fac <- qnorm(p)
  sigma <- sd(rends, na.rm = TRUE)
  sqrt_t <- sqrt(t)
  S <- activo[length(activo)] 
  VaR <- Fac*S*sigma*sqrt_t
  return(VaR)
}



# VaR Delta Normal
  # Sea VaR = (mu + z * sigma)*cierre_final, donde:
  # mu es la media de los rendimientos
  # sigma es la desviación de los rendimientos
  # z es el percentil p de la distribución normal estándar
  # p es el nivel de confianza del VaR 
  # t el horizonte de tiempo deseado
  

VaR_DNORM <- function(p, t, activo){ # activo = vector con precios de cierre
  rends <- c()
  for (k in 2:length(activo) ){
    rends[k] <- (activo[k]/activo[k-1]) - 1 
  } #Rendimientos del activo
  
  mu <-  mean(rends, na.rm = T)
  sigma <- sd(rends, na.rm = T)
  z <- qnorm(p)
  VaR <- (mu + z * sigma) * activo[length(activo)]
  return(sqrt(t)*VaR)
  
}



# VaR Cornish Fisher
# Se utiliza cuando se supone que los rendimientos no siguen una distribución normal
# Incorpora información de los coeficientes de asimetría y curtosis en el cálculo
  # Sea VaR = (mu + z * sigma)*cierre_final, donde:
  # mu es la media de los rendimientos
  # w = z+((z**2)-1)*(S/6)+z*((z**2)-3)*(k/24)-z*(2*(z**2)-5)*((S**2)/36)
  # z es el percentil p de la distribución normal estándar
  # p es el nivel de confianza del VaR
  # S es el coeficiente de asimetría de los rendimientos (=3 sería normal)
  # k es la curtosis de los rendimientos
  # t el horizonte de tiempo deseado


library(moments) # coeficientes de asimetría y curtosis

VaR_CF <- function(p, t, activo){
  rends <- c()
  
  for (i in 2:length(activo) ){
    rends[i] <- (activo[i]/activo[i-1]) - 1 
  } #Rendimientos del activo
  
  
  k <- kurtosis(rends, na.rm = TRUE)
  S <- skewness(rends, na.rm = TRUE)
  z <- qnorm(p) 
  m <- mean(rends, na.rm = TRUE)
  sigma <- sd(rends, na.rm = TRUE)
  
  w <- z+((z**2)-1)*(S/6)+z*((z**2)-3)*(k/24)-z*(2*(z**2)-5)*((S**2)/36)
  VaR <- (m + w * sigma)*activo[length(activo)]
  return(VaR*sqrt(t))
}





# VaR EWMA
# Asigna un mayor peso a los rendimientos actuales
  # Sea t el horizonte de tiempo deseado y p el nivel de confianza
  # se utiliza el mismo resultado del VaR paramétrico
  # La diferencia recae sobre la forma en que sigma se calcula
  # Sea act el vector con precios de cierre de un activo

VaR_EWMA <- function(p, t, act){
  
  # Construcción del df usado en EWMA ------------------------------------
  df_EWMA <- function(lambda, activo){
    
    N <- as.numeric(length(activo):1) #Columna de observaciones descendentes
    Fecha <- row.names(precios) #Columna de fechas
    Precio <- activo #Columna de precios
    Rend <- c() #Columna de rendimientos
    
    for (i in 2:length(activo) ){
      Rend[i] <- (activo[i]/activo[i-1]) - 1 
    } #Da valores a Rend
    
    wi <- (1-lambda)*(lambda**(N-1)) #Columna wi
    Rend_ajus <- wi* (Rend**2) #Columna rendimiento ajustado
    M_recursivo <- c()
    
    for (i in 2:length(N)) {
      if (i == 2) {
        M_recursivo[i] <- Rend_ajus[i]    
      } else{
        M_recursivo[i] <- (1-lambda)* Rend[i]**2 + M_recursivo[i-1]*lambda
      }
    } #Llenado de la columna M_recursivo
    
    Vol_din <- sqrt(M_recursivo) #Columna volatilidad dinámica
    Errores <- ((Rend**2-M_recursivo*lambda)^2)/N[1] #Columna Errores
    
    df <- as.data.frame(cbind(N,Fecha,Precio, Rend, wi,Rend_ajus)) # df base
    df <- cbind(df, M_recursivo, Vol_din, Errores) #df con columnas faltantes
    
    return(df)
  }
  

  # Construcción del error ------------------------------------------------
  # l es lambda, a es activo
  Err <- function(l, a){
    df <- df_EWMA(l, a)
    ssuma_Errores <- sqrt(sum(df$Errores, na.rm = T))
    return(ssuma_Errores)
  }


  # Optimización de lambda ------------------------------------------------
  #Deseamos minimizar Err, cambiando el parámetro lambda
  
  optim <-  optim(par = 0.5, fn = Err, a = act,method = "L-BFGS-B",
                  lower = 0, upper = 1) #Objeto de optimización; 0.5 es inicial
  
  lambda_optim <- optim[[1]]#Esto regresa el lambda óptimo
  

  # Dataframe óptimo -------------------------------------------------------
  df_optimo <- df_EWMA(lambda_optim, act)
  

  # Determinación del VaR --------------------------------------------------
  varianza <- sum(as.numeric(df_optimo$Rend_ajus),na.rm = T)
  sigma <- sqrt(varianza)
  VaR <- as.numeric(df_optimo$Precio[length(df_optimo$Precio)])*sigma*pnorm(p)*sqrt(t)
  return(VaR)
  
}



# 3.2. VaR Portafolio -----------------------------------------------------



#VaR Paramétrico, de Varianza Mínima y de Punto de Tangencia
  #Definir tickers primero para nombrar las columnas de rendimientos
  #num_acc es vector con cantidad de acciones de cada activo en el portafolio 
  #p es el nivel de confianza del VaR; t su horizonte de tiempo 
  #type = 1, 2, 3 para paramétrico, mínima varianza y óptimo, respectivamente

VaRP <- function(precios, num_acc, p, t, type){

  # Data frame Precios/Rendimientos ---------------------------------------
  df_rends <- as.data.frame(calcular_rendimientos(precios= precios))
  colnames(df_rends) <- paste0('Rend_',colnames(precios))
  df_preciosrend <- cbind(precios,df_rends)
  
  
  
  # Dataframe no diversificado ---------------------------------------------#############################
  vol_rends <- apply(df_rends, 2, sd, na.rm = T) #Columna de sd de rendimientos
  mean_rends <- colMeans(df_rends,na.rm = T) #Columna de medias de los rendimientos
  ultimos_precios <- tail(precios, n = 1) #Ultimos precios del período
  monto <- num_acc * ultimos_precios #Columna Monto en riesgo por activo
  part <- unlist(unname(monto/sum(monto))) #Columna de participación. Minimizará la varianza
  VaR <- monto*vol_rends*qnorm(p)*sqrt(t) #Columna VaR paramétrico individual 
  VaR_no_div <- sum(VaR) #VaR no diversificado
  
  

  # Matriz de Varianzas y Covarianzas de Rendimientos ----------------------
  df_rends <- df_rends[-1,] #Quitamos la primer fila, pues tiene valores NaN
  matriz <- cov(df_rends)
  

  # Varianza de rendimientos del portafolio --------------------------------
  var_rp <- function(participaciones){
    participaciones <- as.matrix(participaciones)
    var <- t(participaciones) %*% matriz %*% participaciones
    return(var)
  }

  # VaR paramétrico diversificado ------------------------------------------###########################
  varianza <- var_rp(participaciones = part)
  sd <- sqrt(varianza)
  VaR_paramétrico <- sum(monto)*qnorm(p)*sd*sqrt(t)


  #VaR paramétrico mínima varianza -----------------------------------------############################
  library(Rsolnp) #Para optimizar la varianza
  # Optimizamos la función varianza
  opt_func <- function(participaciones){
    t(as.matrix(participaciones)) %*% matriz %*% as.matrix(participaciones)
  }
  
  #Especificamos la restricción suma de las participaciones de cada activo. 
  # La suma debe ser 1. Ese 1 agregará en un argumento adicional en solnp
  equal <- function(x) {
    sum(x)
  }
  
  #Objeto optimizador. Contiene en el elemento $pars el vector de participación
  #Minimiza por default
  solnl1 <- solnp(rep(1/length(num_acc), length(num_acc)), #Valores iniciales (aleatorios - entre 0 y 1, suman 1)
        opt_func, # función que desea optimizarse
        eqfun=equal, # función que genera la restricción de igualdad 
        eqB=1,   # restricción de la suma (que sea 1)
        LB=rep(0.001,length(num_acc)), #valor mínimo que puede tomar cada participación
        UB=rep(1, length(num_acc)),
        control = list(trace = 0)) #valor máximo que puede tomar cada participación)
  
  #Vector de participación que genera la varianza mínima:
  part_vm <- solnl1[[1]] #Primer elemento del objeto (lista) de optimización
  names(part_vm) <- tickers
  var_mv <- var_rp(part_vm) #varianza mínima (optimizada por participación part_vm)
  sd_mv <- sqrt(var_mv)
  VaR_MV <- sum(monto)*qnorm(p)*sd_mv*sqrt(t)
  
  
  
  #Coeficiente de variación del portafolio -----------------------------------
  # se desea minimizar el coef de variación: rend_esperado_port/sd_anual_port
  # cambiando los coeficientes de participación. Sea rp rends del portafolio
  coef_rp <- function(participaciones){
    var_anual <- var_rp(participaciones)*252
    sd_anual <- sqrt(var_anual)
    rend_esp_anual <- (participaciones%*%mean_rends)*252
    coef_var <- rend_esp_anual/sd_anual
    return(coef_var)
  }
  
  

  # VaR por punto de tangencia ----------------------------------------------
  opt_func <- function(participaciones){
    var_anual <- var_rp(participaciones)*252
    sd_anual <- sqrt(var_anual)
    rend_esp_anual <- (participaciones%*%mean_rends)*252
    coef_var <- sd_anual/rend_esp_anual
    return(coef_var)
  }

  equal <- function(x) {
    sum(x)
  }
  
  solnl2 <- solnp(rep(1/length(num_acc), length(num_acc)), 
                  opt_func, 
                  eqfun=equal,  
                  eqB=1,   
                  LB=rep(0.001,length(num_acc)),
                  UB=rep(1, length(num_acc)),
                  control = list(trace = 0))
  
  #Vector de participación que genera el punto de tangencia:########################################
  part_pt <- solnl2[[1]] #Primer elemento del objeto (lista) de optimización
  names(part_pt) <- tickers
  var_pt <- var_rp(part_pt) #varianza mínima (optimizada por participación part_vm)
  sd_pt <- sqrt(var_pt)
  VaR_pt <- sum(monto)*qnorm(p)*sd_pt*sqrt(t)
  #return(VaR_pt)
  
  if (type == 1) {
    lista <- list(VaR_no_div, VaR_paramétrico)
    names(lista) <- c('No_diversificado','Diversificado')
    return(lista)
    
  }else if (type == 2) {
    
    return(list(part_vm, VaR_MV))   
    
  }else if (type == 3){
    
    return(list(part_pt, VaR_pt))  
     
    
  } else{return(paste('type should be param = 1, mv = 2, pt = 3'))}
  

  
}




numacc <- c(rep(100,5), rep(200, 5))






