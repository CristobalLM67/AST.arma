#' Analisis de series temporales, modelo ARMA
#'
#' El paquete resuelve una ecuación ARIMA (Autorregresión integrada de media móvil), que modela la serie temporal y permite hacer pronósticos futuros. La ecuación se ajusta a los datos de entrenamiento y utiliza el orden del AR (Autorregresión) y del MA (Media Móvil) especificados en la función. Luego, se realizan pronósticos con el modelo ajustado y se evalúa el rendimiento del mismo. Finalmente, se visualizan los resultados y se generan pronósticos futuros para un horizonte determinado.
#'
#' @param datos debe de ser una serie temporal
#' @param p orden del modelo autorregresivo (AR)
#' @param q orden del modelo de media móvil (MA)
#' @param n_predicciones el numero de predicciones que se desea utilizar
#' @param confianza el nivel de confianza de los resultados
#' @param graficar si se desea ver grafica o no
#'
#' @return pronosticos futuros
#' @export
#'
#' @examples
#' datos <- read.csv("ruta/archivo.csv", sep = ";")
#' print(datos)
#' serie_temporal <- ts(datos$toneladas, start = c(2022,1), frequency = 12)
#' print(serie_temporal)
#' datos <- Analisis_series_temporales(serie_temporal, 1, 2, 12)

# Función de predicciones con ARMA
Analisis_series_temporales <- function(datos, p, q, n_predicciones, confianza = 0.95, graficar = TRUE) {

  # Validación de datos
  if (!is.ts(datos)) stop("El argumento 'datos' debe ser una serie temporal")

  # Entrenamiento del modelo
  modelo <- arima(datos, order = c(p, 0, q))

  # Predicciones
  predicciones <- predict(modelo, n.ahead = n_predicciones, level = confianza)

  # Preparación de resultados
  error <- predicciones$se * qnorm((1 - confianza)/2)
  resultados <- list(
    datos = datos,
    ajuste = modelo$fitted.values,
    predicciones = predicciones$pred,
    intervalo_inferior = predicciones$pred - predicciones$se * qnorm((1 - confianza) / 2),
    intervalo_superior = predicciones$pred + predicciones$se * qnorm((1 - confianza) / 2)
  )
  print(resultados)


  # Gráficos si se solicitan
  if (graficar) {
    plot.ts(resultados$datos, col = "blue", main = "Predicciones")
    lines(resultados$ajuste, col = "red", type = "l")
    lines(resultados$predicciones, col = "green", type = "p")
    polygon(
      x = c(time(resultados$predicciones), rev(time(resultados$predicciones))),
      y = c(resultados$intervalo_inferior, rev(resultados$intervalo_superior)),
      col = "purple",
      border = FALSE
    )
    legend(
      "topright",
      legend = c("Datos", "Predicciones"),
      col = c("blue", "green"),
      lty = 1
    )
  }

  # Resultados
  return(resultados)
}

