# BAR
Practicas de servicio comunitario, banco de alimentos
# Sistema de Gestión y Analítica Predictiva - B.A.R.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-004D40?style=for-the-badge&logo=Rstudio&logoColor=white)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-316192?style=for-the-badge&logo=postgresql&logoColor=white)

Un sistema de registro, visualización y modelado predictivo desarrollado para el Banco de Alimentos de Riobamba (B.A.R.). Esta plataforma digitaliza la gestión del desperdicio alimentario del Mercado "El Mayorista" y proyecta escenarios futuros utilizando técnicas de Machine Learning[cite: 1, 3]. 

Diseñado con una arquitectura local eficiente que no depende de conexión permanente a internet, garantizando estabilidad operativa[cite: 3].

## 🚀 El Problema y la Solución

El B.A.R. necesitaba reemplazar sus procesos manuales con una solución informática estructurada y escalable[cite: 3]. 

**La Solución:** Un dashboard interactivo full-stack que permite:
*   **Gestión Administrativa:** Operaciones CRUD conectadas a una base de datos PostgreSQL normalizada para garantizar la integridad y trazabilidad de los registros[cite: 1, 3].
*   **Proyección Futura:** Implementación de modelos de Machine Learning (Random Forest, SVM, Regresión Lineal y Ridge) con una estrategia de predicción recursiva multi-paso para estimar volúmenes de desperdicio a largo plazo[cite: 1, 3].
*   **Identificación de Patrones:** Uso de clustering no supervisado (K-Means y Jerárquico) para detectar patrones operativos y optimizar la logística de transporte[cite: 1, 3].
*   **Impacto Social y Ambiental:** Cálculo automatizado que convierte los kilos rescatados en raciones de comida servidas (0.4 kg = 1 plato) y emisiones de CO2 evitadas (2.5 kg CO2 por kg rescatado)[cite: 1, 3].

## 🛠️ Arquitectura y Tecnologías

El sistema está construido sobre un stack analítico robusto:

*   **Frontend/Backend:** R y Shiny (con `shinydashboard`, `shinyjs`, `DT`)[cite: 1, 3].
*   **Visualización de Datos:** `plotly` y `ggplot2` para dashboards dinámicos e interactivos[cite: 1, 3].
*   **Base de Datos Relacional:** PostgreSQL, gestionada mediante la librería `DBI` y `RPostgres`[cite: 1].
*   **Modelado Predictivo (Machine Learning):** `randomForest`, `e1071` (SVM), `glmnet` (Ridge Regression) y `cluster`[cite: 1].

La base de datos cuenta con una estructura relacional centralizada de cuatro tablas principales: `roles`, `usuarios`, `sensores` y `pesos`, previniendo conflictos de sincronización local de datos[cite: 2, 3].

## ⚙️ Instrucciones de Despliegue Local

Para levantar el entorno localmente, sigue estos pasos:

### 1. Preparar la Base de Datos (PostgreSQL)
1. Instala PostgreSQL y asegúrate de que el puerto `5432` esté habilitado[cite: 1].
2. Crea una base de datos llamada `BAR`[cite: 2].
3. Ejecuta el script SQL incluido en este repositorio para restaurar el esquema estructural (Tablas: `pesos`, `roles`, `sensores`, `usuarios`) y los datos semilla[cite: 2].
4. Credenciales por defecto configuradas en el entorno local[cite: 1]:
   * **Host:** `localhost`[cite: 1]
   * **User:** `postgres`[cite: 1]
   * **Password:** `12345`[cite: 1]

### 2. Levantar el Entorno en R
Ejecuta el siguiente script en tu consola de R o RStudio para instalar las dependencias requeridas[cite: 1]:

```R
install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "shinyjs", "lubridate", "plotly", "DBI", "RPostgres", "randomForest", "e1071", "glmnet", "cluster", "ggplot2"))
