# Para leer datos
leer_tabla <- function(archivo){
  library(readODS)
  df <- read_ods(archivo)
  return(df)
}

# Convertir de milímetros a escala Krumbein phi (ϕ)
# Referencia: 
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/End-member-modelling-analysis/The-EMMA-algorithm/The-EMMAgeo-package/index.html
obtener_phi <- function(mm, ref = 1){
  phi <- -log2(mm/ref)
  return(phi)
}

# Función para construir gráfico de "curva de tamaño de grano de frecuencias acumuladas suavizada",
# usando una función Bezier. Tomado de: https://rpubs.com/manchulu/706871
generar_tabla_frec_acum_bezier <- function(x, y, n=10)
{
  outx <- NULL
  outy <- NULL
  
  i <- 1
  for (t in seq(0, 1, length.out=n))
  {
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y
    
    i <- i+1
  }
  
  return (list(x=outx, y=outy))
}

bez <- function(x, y, t)#smoothing function
{
  outx <- 0
  outy <- 0
  n <- length(x)-1
  for (i in 0:n)
  {
    outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
    outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
  }
  
  return (list(x=outx, y=outy))
}

crear_grafico <- function(muestra, phi=T) {
  plot(
    pesos_tamanos_mm_phi_rel_acum_unido$phi,
    pesos_tamanos_mm_phi_rel_acum_unido[, paste0(muestra, '_acum'), drop=T],
    type="n", 
    lty=1, pch=20 , cex=2,
    yaxt='n', 
    xaxt='n',
    xlim = c(-3, 4),
    ylim = c(0, 100),
    main = paste("Curva de frecuencia acumulada", muestra),
    ylab = "Porcentaje de peso acumulado (%)",
    xlab = if(phi) expression("Tamaño de malla  " *phi) else "Tamaño de malla (mm)")
  points(
    generar_tabla_frec_acum_bezier(
      pesos_tamanos_mm_phi_rel_acum_unido$phi,
      pesos_tamanos_mm_phi_rel_acum_unido[, paste0(muestra, '_acum'), drop=T], 20),
    type="l", col="black", lty = 1, lwd=2)
  axis(2, at=seq(0,100,10), labels =seq(0,100,10), lty=2, las=2)
  if (phi) Xaxis_label <- pesos_tamanos_mm_phi_rel_acum_unido$phi else
    Xaxis_label <- pesos_tamanos_mm_phi_rel_acum_unido$mm
  att <- pesos_tamanos_mm_phi_rel_acum_unido$phi
  axis(1, at=att, labels =Xaxis_label, lty=2,las=2)
  grid(col = 'grey70')
}
