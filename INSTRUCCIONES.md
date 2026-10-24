# Laboratorio de Econometría — versión para estudiantes

## Empezar
1. Instalar R y RStudio (si no los tienes).
2. Abrir `instalar_paquetes.R` en RStudio y ejecutar todo. Se necesita Internet para instalar los paquetes.
3. Abrir `app.R` y presionar **Run App** o, desde esta carpeta, ejecutar `shiny::runApp()`.
4. La aplicación se abre en una ventana o navegador. No es necesario modificar el código.

## Datos
- Acepta CSV, XLSX y XLS.
- Cada fila es una observación y cada variable es una columna. La base debe tener al menos una columna numérica.
- La base predefinida es **simulada**, no corresponde a datos reales de El Salvador y solo sirve para practicar.
- Para series de tiempo, ordenar cronológicamente antes de cargar, verificar observaciones regulares y no duplicadas.
- Para las pruebas de raíces unitarias no debe haber valores vacíos en la variable seleccionada.

## Qué incluye
- Visualización y estadística descriptiva.
- ADF, Phillips-Perron y KPSS en nivel y/o diferencias con constante o tendencia, estadísticos, valores críticos y decisiones.
- Regresión MCO y diagnósticos de Breusch–Pagan, Breusch–Godfrey, Jarque–Bera.
- ARIMA y ARIMAX, ajuste y Ljung–Box, descarga de ajustados.
- Exportación de resultados de raíz unitaria.

## Qué falta respecto al proyecto de investigación
El prototipo **no ejecuta** todavía Zivot–Andrews, ARDL/NARDL, bounds, Johansen, VAR/VECM, IRF, FEVD, Toda–Yamamoto, pruebas de estabilidad ni el flujo integral de robustez. Es una versión inicial docente, no un reemplazo completo del análisis original.

## Compartir con los alumnos
Puedes enviar esta carpeta comprimida. Cada estudiante necesita instalar R y los paquetes en su equipo.
Alternativamente, puedes desplegarla en shinyapps.io o un servidor Shiny para que ingresen por URL sin instalar R. Esto requiere crear/configurar el despliegue y revisar las condiciones de acceso y límites del servicio.

## Nota metodológica
El botón ARIMAX no genera pronósticos futuros si hay regresores exógenos porque se requieren escenarios futuros para dichos regresores. El orden de la serie lo determina el orden de las filas, no la columna de fecha. Los resultados no sustituyen la revisión econométrica del docente.
