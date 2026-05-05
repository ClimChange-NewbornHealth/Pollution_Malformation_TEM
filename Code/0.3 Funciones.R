# Funciones de apoyo para los calculos -----

## Función para colapsar contaminación del aire en rango de fechas ----
ColapseByMeanDates <- function(query1, query2, data, vardate, varcolapse) {
  # Seleccionamos datos por rango de fecha
  data2 <- data[which(data[, vardate] >= query1 &
                      data[, vardate] <= query2),
                c(vardate, varcolapse)]
  # Calculamos media de variable seleccionada
  variable <- mean(na.omit(data2[, varcolapse]))
  # Calculamos completitud
  comp <- length(na.omit(data2[, varcolapse])) / length(data2[, varcolapse])
  # Reportamos NA si completitud < 50%
  variable <- ifelse(comp >= 0.5, variable, NA)
  # Retornamos resultados
  return(variable)
}

## Trimestres gestacionales (fechaini = día 0 del inicio de la semana 1)
## T1 = semanas 1-13, T2 = 14-27, T3 = 28 hasta nacimiento (fechafin)
n_weeks_trimester3 <- function(fechaini, fechafin) {
  start_t3 <- fechaini + 27L * 7L
  d <- as.numeric(fechafin - start_t3, units = "days")
  nw <- as.integer(pmax(1L, ceiling(pmax(0, d) / 7)))
  nw[start_t3 > fechafin] <- NA_integer_
  nw
}

## Predicción LUR promediada sobre el tercer trimestre (duración según fechafin)
SolutionByDates_trimester3 <- function(model, type, idbase, fechaini, fechafin) {
  nw <- as.integer(n_weeks_trimester3(fechaini, fechafin))
  if (length(nw) > 1L) nw <- nw[1L]
  if (length(nw) == 0L || is.na(nw) || nw < 1L) return(c(NA_real_, NA_real_))
  SolutionByDates(model, type, idbase, fechaini + 27L * 7L, nw, fechafin)
}

## Función para predecir PM por modelo en rango de tiempo ----

SolutionByDates <- function(model, type, idbase, dateini, weeks, dateend) {
  if (!(dateini < dateend) || is.na(dateini)) return(c(NA, NA))

  frame <- data.frame(
    idbase = idbase,
    dateini2 = seq(dateini, length.out = weeks, by = "7 day"),
    dateend2 = seq(dateini + 6, length.out = weeks, by = "7 day"),
    stringsAsFactors = FALSE
  )
  frame <- frame[which(frame$dateini2 < dateend), ]
  frame <- merge(frame, base@data, by = "idbase", all.x = FALSE)

  if (nrow(frame) == 0) return(c(NA, NA))

  frame$LLEE_PM25 <- mapply(ColapseByMeanDates, frame$dateini2, frame$dateend2,
                            MoreArgs = list(data = pmcs, vardate = "fecha", varcolapse = "LLEE_PM25"))
  frame$LLEE_TEMP <- mapply(ColapseByMeanDates, frame$dateini2, frame$dateend2,
                            MoreArgs = list(data = pmcs, vardate = "fecha", varcolapse = "LLEE_TEMP"))

  # comprobar variables del modelo no todas NA
  vars <- intersect(all.vars(formula(model)), names(frame))
  if (length(vars) == 0 || any(sapply(frame[vars], function(x) all(is.na(x))))) {
    return(c(NA, NA))
  }

  predict_safe <- function(mod, newdata) {
    tryCatch({
      predict(mod, newdata)
    }, error = function(e) {
      warning("predict failed: ", conditionMessage(e), call. = FALSE)
      rep(NA_real_, nrow(newdata))
    })
  }

  mc_sc <- predict_safe(modelopmcs, frame)
  frame$MassC_SC_pred <- ifelse(is.na(mc_sc), NA_real_, exp(mc_sc))

  if (type == "levo") frame$LevoC_SC_pred <- ifelse(is.na(predict_safe(modelolevocs, frame)), NA_real_, exp(predict_safe(modelolevocs, frame)))
  if (type == "k")    frame$KC_SC_pred   <- ifelse(is.na(predict_safe(modeloKcs, frame)),   NA_real_, exp(predict_safe(modeloKcs, frame)))

  frame$pred <- ifelse(is.na(predict_safe(model, frame)), NA_real_, exp(predict_safe(model, frame)))

  cs <- if (type == "levo") mean(na.omit(frame$LevoC_SC_pred)) else if (type == "k") mean(na.omit(frame$KC_SC_pred)) else mean(na.omit(frame$MassC_SC_pred))
  pred <- mean(na.omit(frame$pred))
  return(c(cs, pred))
}

## Función para predecir por semana individual ----

SolutionByWeek <- function(model, type, idbase, dateini, week_offset, dateend) {
  # Calculamos la fecha de inicio para esta semana específica
  week_start <- dateini + (week_offset * 7)
  week_end <- week_start + 6
  
  # Ejecutamos solo si la semana es válida
  # Permitimos semanas desde -12 hasta el final del período gestacional
  if (week_start < dateend & week_end <= dateend & !is.na(week_start) & 
      week_start >= dateini - (12 * 7)) {
    # Creamos data frame para esta semana
    frame <- data.frame(idbase = idbase, 
                       dateini2 = week_start,
                       dateend2 = week_end)
    # Si no hay filas tras el merge devolvemos NA (evita llamadas a ns() con vectores vacíos)
    if (nrow(frame) == 0) {
      return(c(NA, NA))
    }
    # Unimos con datos espaciales por 'idbase'
    frame <- merge(frame, base@data, by = "idbase")
    # Unimos con datos de central site
    frame$LLEE_PM25 <- mapply(ColapseByMeanDates, frame$dateini2, frame$dateend2, 
                              list(pmcs), "fecha", "LLEE_PM25")
    frame$LLEE_TEMP <- mapply(ColapseByMeanDates, frame$dateini2, frame$dateend2, 
                              list(pmcs), "fecha", "LLEE_TEMP")
    # Wrapper seguro para predict (si falla, devuelve vector de NA en vez de parar)
    predict_safe <- function(mod, newdata) {
      res <- tryCatch({
        predict(mod, newdata)
      }, error = function(e) {
        warning("predict failed for model: ", conditionMessage(e))
        rep(NA_real_, nrow(newdata))
      })
      return(res)
    }

    # Predecimos CS (uso predict_safe)
    mc_sc <- predict_safe(modelopmcs, frame)
    frame$MassC_SC_pred <- exp(mc_sc)
    if (type == "levo") {
      lv_sc <- predict_safe(modelolevocs, frame)
      frame$LevoC_SC_pred <- exp(lv_sc)
    }
    if (type == "k") {
      k_sc <- predict_safe(modeloKcs, frame)
      frame$KC_SC_pred <- exp(k_sc)
    }

    # Predecimos para esta semana según modelo (uso predict_safe)
    pred_raw <- predict_safe(model, frame)
    frame$pred <- exp(pred_raw)

    # Extraemos CS y PRED (primera fila correspondiente a la semana)
    cs <- if (type == "levo") {
      frame$LevoC_SC_pred[1]
    } else if (type == "k") {
      frame$KC_SC_pred[1]
    } else {
      frame$MassC_SC_pred[1]
    }
    pred <- frame$pred[1]

    return(c(cs, pred))
  } else {
    return(c(NA, NA))
  }
}
