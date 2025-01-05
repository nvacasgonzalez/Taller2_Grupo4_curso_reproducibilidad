
rm(list = ls())

invisible(gc())



# Función para instalar y cargar paquetes si no están instalados

install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    tryCatch({
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }, error = function(e) {
      message(paste("Error al instalar el paquete:", package))
    })
  }
}

# Lista de paquetes necesarios
packages <- c("knitr", "kableExtra", "DT", "table1", "reticulate", "gapminder", "latex2exp", "tidyverse", "plotly", "maps", "sf", "leaflet", "tmap", "rio", "readxl","readr", "DiagrammeR", "ggplot2")

if(!require(tmap)){install.packages("tmap")}

if(!require(dplyr)){install.packages("dplyr")}



# Importación de bases de datos a utilizar 

est_general_20241130 <- read_csv("C:/Users/natal/OneDrive - Universidad Alberto Hurtado/PERSONAL/DIPLOMADO DATA SCIENCE/M. REPRODUCIBILIDAD/Taller_n2/Taller2_Grupo4_curso_reproducibilidad/docs/_data/est_general_20241130.csv")
View(est_general_20241130)

library(readr)
caracterizacion_20241130 <- read_delim("C:/Users/natal/OneDrive - Universidad Alberto Hurtado/PERSONAL/DIPLOMADO DATA SCIENCE/M. REPRODUCIBILIDAD/Taller_n2/Taller2_Grupo4_curso_reproducibilidad/docs/_data/caracterizacion_20241130.csv", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE)
View(caracterizacion_20241130)

library(readr)
X20240912_Directorio_Oficial_EE_2024_20240430_WEB <- read_delim("C:/Users/natal/OneDrive - Universidad Alberto Hurtado/PERSONAL/DIPLOMADO DATA SCIENCE/M. REPRODUCIBILIDAD/Taller_n2/Taller2_Grupo4_curso_reproducibilidad/docs/_data/20240912_Directorio_Oficial_EE_2024_20240430_WEB.csv", 
                                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(X20240912_Directorio_Oficial_EE_2024_20240430_WEB)

#CREACIÓN ENTIDAD DE BENEFICIARIOS POTENCIALES

#PARA ELLO PRIMERO CREO UN IDENTIFICADOR.
caracterizacion_20241130 <- caracterizacion_20241130 %>%
  mutate(ID_beneficiarios_potenciales = paste0("ID_", row_number()))

#AHORA CREO LA ENTIDAD.
entidad_beneficiarios_potenciales <- caracterizacion_20241130 %>% 
  select(ID_beneficiarios_potenciales, atendidos, region, procedencia, nacionalidad,  estado_civil,
         etnia, tramo_edad, calidad_procesal, tipo_poblacion, actividad_laboral_ingreso,
         beneficio_intrapenitenciario, instruccion_ingreso, sexo)

#VERIFICACIÓN DE INCONSISTENCIAS. SE COMPRUEBA QUE TODOS ESTÁN BIEN. 
unique(entidad_beneficiarios_potenciales$procedencia)
unique(entidad_beneficiarios_potenciales$nacionalidad)
unique(entidad_beneficiarios_potenciales$estado_civil)
unique(entidad_beneficiarios_potenciales$etnia)
unique(entidad_beneficiarios_potenciales$tramo_edad)
unique(entidad_beneficiarios_potenciales$calidad_procesal)
unique(entidad_beneficiarios_potenciales$tipo_poblacion)
unique(entidad_beneficiarios_potenciales$actividad_laboral_ingreso)
unique(entidad_beneficiarios_potenciales$beneficio_intrapenitenciario)
unique(entidad_beneficiarios_potenciales$instruccion_ingreso)
unique(entidad_beneficiarios_potenciales$sexo)

#FILTRAR POR POBLACIÓN BENEFICIARIA CONDENADA, RECULUIDA 24HRS, Y CON CIERTOS NIVELES EDUCATIVOS
unique(entidad_beneficiarios_potenciales$calidad_procesal)
entidad_beneficiarios_potenciales <- subset(entidad_beneficiarios_potenciales, calidad_procesal == "Condenado")

unique(entidad_beneficiarios_potenciales$tipo_poblacion)
entidad_beneficiarios_potenciales <- subset(entidad_beneficiarios_potenciales, tipo_poblacion == "Recluida 24 Hrs.")

entidad_beneficiarios_potenciales <- entidad_beneficiarios_potenciales %>%
  filter(instruccion_ingreso %in% c("Básica Completa", "Básica Incompleta", "Media Incompleta", "Sin Instrucción"))





#CREO ENTIDAD DE REGION
unique(caracterizacion_20241130$region)  

regiones <- c(
  "Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
  "Región de Atacama", "Región de Coquimbo", "Región de Valparaíso", 
  "Región Metropolitana de Santiago", "Región de O'Higgins", "Región del Maule", 
  "Región de Ñuble", "Región del Biobío", "Región de La Araucanía", 
  "Región de Los Ríos", "Región de Los Lagos", "Región de Aysén", 
  "Región de Magallanes y la Antártica Chilena")

numeros_region <- c(15, 1, 2, 3, 4, 5, 13, 6, 7, 16, 8, 9, 14, 10, 11, 12)

entidad_region <- data.frame(
  COD_REG_RBD = numeros_region,
  region = regiones
)





#CREO ENTIDAD DE ESTABECIMIENTO PENITENCIARIO

#FILTRO PARA OBTENER SOLO LOS ESTABLECIMIENTOS PENITENCIARIOS QUE COINCIDEN CON LAS CARACTERÍSTICAS BUSCADAS
unique(est_general_20241130$subsistema)
unique(est_general_20241130$tipo_poblacion)
unique(est_general_20241130$poblacion)

est_general_20241130 <- est_general_20241130 %>%
  filter (subsistema == "Subsistema Cerrado",
          tipo_poblacion == "Cerrado 24 Horas",
          poblacion == "Condenados (Privados de Libertad 24 Horas)")

#CREAR ENTIDAD
entidad_establecimientos_penitenciarios <- est_general_20241130 %>%
  select(establecimiento, region) %>%
  distinct(establecimiento, .keep_all = TRUE) %>%  
  mutate(ID_establecimiento = paste0("ID_", row_number())) %>%  
  select(ID_establecimiento, everything())




#ENTIDAD CENTRO EDUCATIVO. 
entidad_centro_educativo <- X20240912_Directorio_Oficial_EE_2024_20240430_WEB[, c(
  "RBD",                  
  "NOM_RBD",             
  "COD_REG_RBD",           
  "NOM_REG_RBD_A",         
  "COD_PRO_RBD",           
  "COD_COM_RBD",           
  "NOM_COM_RBD",         
  "COD_DEPROV_RBD",        
  "NOM_DEPROV_RBD",        
  "COD_DEPE2",           
  "RURAL_RBD",            
  "LATITUD",
  "LONGITUD",
  "MAT_TOTAL"
)]
  
 #CAMBIAR NOMBRE DE COLUMNAS (OPTATIVO)
colnames(entidad_centro_educativo) <- c(
  "ID_CentroEducativo",  # RBD
  "Nombre_centro_educativo",  # NOM_RBD
  "ID_Region",  # COD_REG_RBD
  "region",  # NOM_REG_RBD_A
  "Codigo_de_provincia",  # COD_PRO_RBD
  "Codigo_oficial_comuna",  # COD_COM_RBD
  "Nombre_comuna",  # NOM_COM_RBD
  "Codigo_Dpto_Provincial_Educación",  # COD_DEPROV_RBD
  "Nombre_Dpto_Provincial_Educación",  # NOM_DEPROV_RBD
  "Dependencia_establecimiento",  # COD_DEPE2
  "Indicador_ruralidad",  # RURAL_RBD
  "Latitud_establecimiento",  # LATITUD
  "Longitud_establecimiento",  # LONGITUD
  "Matricula_total" # MAT_TOTAL
)

#NORMALIZAR COLUMNA DE REGION CON MISMOS NOMBRES DE LAS OTRAS ENTIDADES.

# Vector con los nombres de las regiones estándar
regiones_estandar <- c("Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
                       "Región de Atacama", "Región de Coquimbo", "Región de Valparaíso", 
                       "Región Metropolitana de Santiago", "Región de O'Higgins", "Región del Maule", "Región de Ñuble", "Región del Biobío", "Región de La Araucanía", 
                       "Región de Los Ríos", "Región de Los Lagos", "Región de Aysén", 
                       "Región de Magallanes y la Antártica Chilena")

# Normalizar los nombres de la variable región en el dataframe
entidad_centro_educativo$region_normalizada <- tolower(trimws(entidad_centro_educativo$region))

# Definir el vector con abreviaturas 
abreviaturas <- c("ayp", "tpca", "antof", "atcma", "coq", "valpo", "rm", "lgbo", "maule", "nuble", "bbio", "arauc", "rios", "lagos", "aysen", "mag")

# Usar match() para mapear las regiones normalizadas a las estándar
entidad_centro_educativo$region_estandarizada <- regiones_estandar[match(entidad_centro_educativo$region_normalizada, abreviaturas)]


#FILTRAR POR ESTABLECIMIENTOS EDUCACIONALES QUE CUMPLEN CON CARACTERÍSITCAS QUE SE NECESITAN
centros_educativos <- c(
  "LICEO REBECA OLIVARES BENITEZ", "LICEO TECNICO PROF. DE ADULTOS PUKARA", "LICEO DE ADULTOS HUMBERTO DÍAZ CASANUEVA",
  "LICEO DE ADULTOS HERBERT VARGAS WALLIS",
  "LICEO SANTA MARIA EUFRASIA", "LICEO C.E.I.A. JOSE ALEJANDRO SORIA VARAS",
  "ANEXO LICEO NOCTURNO", "LICEO DOMINGO LATRILLE LASTAUNOU",
  "ESCUELA PROFESOR LUIS GARRIDO PÁVEZ",
  "ESCUELA ANDRÉS BELLO", "ESCUELA JOSÉ DOMINGO SAAVEDRA",
  "C.E.I.A. DR. OSVALDO ROJAS G",
  "COLEGIO JUAN LUIS VIVES", "COLEGIO ANTUHUE", 
  "ESCUELA ESPECIAL DE ADULTOS F Nº 142", 
  "COLEGIO JUAN LUIS VIVES", "COLEGIO DE ADULTOS PROFESOR MANUEL GUERRERO CEBALLOS", "ESCUELA ALAMEDA LINARES", 
  "ESCUELA PROF.REMIGIO AGURTO FUENTES", "ESCUELA MARIANO LATORRE", "ESC.CAPLLAN JOSE LUIS LOPEZ CARRASCO", 
  "ESCUELA SARGENTO PRIMERO CRISTIAN VENEGAS OJEDA", "COLEGIO NUEVOS HORIZONTES", "ESCUELA DE ADULTOS RIO SUR", 
  "ESCCUELA MUNICIPAL PROFESOR DAVID FERNÁNDEZ PÉREZ", 
  "LICEO BICENTENARIO CIENCIAS Y HUMANIDADES RBD 6223-5", "C.E.I.A. ANTONIO ACEVEDO HERNANDEZ",
  "COLEGIO ANDINO"
)

# Filtrar la base de datos entidad_centro_educativo por los nombres de centros educativos
centros_filtrados <- entidad_centro_educativo %>%
  filter(Nombre_centro_educativo %in% centros_educativos)

# Filtrar centros educativos
centros_filtrados <- entidad_centro_educativo %>%
  filter(Nombre_centro_educativo %in% centros_educativos)









#ESTADÍSTICOS DESCRIPTIVOS 


#OBTENER BENEFICIARIOS POR REGIÓN Y EL PORCENTAJE
beneficiarios_por_region <- entidad_beneficiarios_potenciales %>%
  group_by(region) %>%
  summarise(total_beneficiarios = sum(atendidos, na.rm = TRUE)) %>%
  mutate(porcentaje = format(round((total_beneficiarios / sum(total_beneficiarios) * 100), 2), nsmall = 2))


#Visualización
ggplot(beneficiarios_por_region, aes(x = reorder(region, -total_beneficiarios), y = total_beneficiarios)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Porcentaje Beneficiarios Potenciales por Región", x = "Región", y = "Total Beneficiarios") +
  theme_minimal() +
  geom_text(aes(label = paste0("(", total_beneficiarios, ") ", 
                               round((total_beneficiarios / sum(total_beneficiarios) * 100), 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black")


# TRAMO DE EDAD BENEFICIARIOS
beneficiarios_tramo_edad <- entidad_beneficiarios_potenciales %>%
  group_by(tramo_edad) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(porcentaje = (total / sum(total) * 100))
#Visuaización del tramo de edad de los beneficiarios

#Primero ordenarlos.
orden_tramos <- c("18-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                  "75-79", "80-84", "85-89", "90-94")

beneficiarios_tramo_edad <- beneficiarios_tramo_edad %>%
  mutate(tramo_edad = factor(tramo_edad, levels = orden_tramos))

#Y graficar
ggplot(beneficiarios_tramo_edad, aes(x = reorder(tramo_edad, -total), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Porcentaje Beneficiarios Potenciales por Tramo Etario", x = "Tramo de Edad", y = "Total Beneficiarios") +
  theme_minimal() +
  geom_text(aes(label = paste0("(", total, ") ", round(porcentaje, 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black")




#NACIONALIDAD DE BENEFICIARIOS
beneficiarios_nacionalidad <- entidad_beneficiarios_potenciales %>%
  group_by(nacionalidad) %>%
  summarise(total = n()) %>%
  mutate(porcentaje = round((total / sum(total)) * 100, 2)) %>%
  arrange(desc(total))
#Visualización
ggplot(beneficiarios_nacionalidad, aes(x = reorder(nacionalidad, -total), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Beneficiarios Potenciales por Nacionalidad", x = "Nacionalidad", y = "Total Beneficiarios") +
  theme_minimal() +
  geom_text(aes(label = paste0("(", total, ") ", round(porcentaje, 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black")



#GENERO DE BENEFICAIRIOS
beneficiarios_sexo <- entidad_beneficiarios_potenciales %>%
  group_by(sexo) %>%
  summarise(total = n()) %>%
  mutate(porcentaje = round((total / sum(total)) * 100, 2))
#Visualización
ggplot(beneficiarios_sexo, aes(x = "", y = total, fill = sexo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Porcentaje de Beneficiarios Potenciales por Sexo", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "black")) +
  geom_text(aes(label = paste0("(", total, ") ", round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3.5, color = "black")


#ACTIVIDAD LABORAL AL INGRESO DE BENEFICIARIOS
beneficiarios_actividad_laboral <- entidad_beneficiarios_potenciales %>%
  group_by(actividad_laboral_ingreso) %>%
  summarise(total = n()) %>%
  mutate(porcentaje = round((total / sum(total)) * 100, 2)) %>%
  arrange(desc(total))
#Visualización
ggplot(beneficiarios_actividad_laboral, aes(x = reorder(actividad_laboral_ingreso, -total), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Porcentaje Beneficiarios Potenciales por Actividad Laboral", x = "Actividad Laboral", y = "Total Beneficiarios") +
  theme_minimal() +
  geom_text(aes(label = paste0("(", total, ") ", round(porcentaje, 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black")





#VER LA POBLACION MATRICULADA POR REGION
poblacion_matriculada <- entidad_centro_educativo_filtrado %>%
  group_by(NOM_REG_RBD_A) %>%        
  summarise(suma_MAT_TOTAL = sum(MAT_TOTAL, na.rm = TRUE))

#Visualización 
ggplot(beneficiarios_por_region, aes(x = reorder(region, -total_beneficiarios), y = total_beneficiarios)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Porcentaje Beneficiarios Potenciales por Región", x = "Región", y = "Total Beneficiarios") +
  theme_minimal() +
  geom_text(aes(label = paste0("(", total_beneficiarios, ") ", 
                               round((total_beneficiarios / sum(total_beneficiarios) * 100), 1), "%")), 
            hjust = -0.2, size = 3.5, color = "black")



#VER LA TASA DE MATRICULADOS. 
#UNO EL DATA FRAME DE BENEFICIARIOS POR REGION CON EL DATA FRAME DE LA POBLACION MATRICULADA.

#NORMALIZO NOMBRES DE REGIONES
setdiff(unique(poblacion_matriculada$NOM_REG_RBD_A), unique(beneficiarios_por_region$region))

poblacion_matriculada <- poblacion_matriculada %>%
  mutate(NOM_REG_RBD_A = recode(
    NOM_REG_RBD_A,
    "Región Metropolitana de Santiago" = "Región Metropolitana",
    "Región de Magallanes y la Antártica Chilena" = "Región de Magallanes"
  ))

#UNO LAS TABLAS DE BENEFICIARIOS POR REGION Y DE POBLACION MATRICULADA EN CENTROS EDUCATIVOS. 
regiones_especificas <- unique(poblacion_matriculada$NOM_REG_RBD_A)

data_frame_filtrado <- beneficiarios_por_region %>%
  filter(region %in% regiones_especificas)

Bases_unidas_tasa <- poblacion_matriculada %>%
  left_join(beneficiarios_por_region, by = c("NOM_REG_RBD_A" = "region"))

Bases_unidas_tasa <- Bases_unidas_tasa %>%
  mutate(tasa_matriculados = suma_MAT_TOTAL / total_beneficiarios)

#Y GRAFICO
ggplot(Bases_unidas_tasa, aes(x = reorder(NOM_REG_RBD_A, -tasa_matriculados), y = tasa_matriculados)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Tasa de Matriculados por Región",
    x = "Región",
    y = "Tasa de Matriculados"
  ) +
  theme_minimal()








#MODELO CONCEPTUAL 

grViz("
  digraph modelo_conceptual {
    # Configuración global del grafo
    graph [layout = dot, rankdir = LR]
    
    # Definición de nodos
    node [shape = rectangle, style = filled, fillcolor = lightgoldenrod1, fontname = Arial]

    Atendido [label = 'Atendido\n- ID_atendido\n- ID_Región\n- Procedencia\n- Nacionalidad\n- Estado Civil\n- Etnia\n- tramo_edad\n- calidad procesal\n- tipo de población\n- actividad laboral ingreso\n- beneficio_intrapenitenciario\n- instrucción_ingreso\n- Sexo']
    Region [label = 'Región\n- ID_Región\n- Nombre región']
    EstablecimientoPenitenciario [label = 'Establecimiento penitenciario\n- ID_establecimiento\n- Nombre establecimiento\n- ID_región\n- tipo de población']
    CentroEducativo [label = 'Centro educativo\n- ID_CentroEducativo (RBD)\n- Nombre centro educativo (NOM_RBD)\n- ID_Región (COD_REG_RBD)\n- Nombre Región (NOM_REG_RBD_A)\n- Código de provincia (COD_PROV_RBD)\n- Código oficial comuna (COD_COM_RBD)\n- Nombre comuna (NOM_COM_RBD)\n- Código Departamento Provincial de Educación (COD_DEPROV_RBD)\n- Nombre Departamento Provincial Educación (NOM_DEPROV_RBD)\n- Dependencia del establecimiento (COD_DEPE2)\n- Indicador de ruralidad (RURAL_RBD)\n- Latitud establecimiento (LATITUD)\n- Longitud establecimiento (LONGITUD)']

    # Definición de relaciones
    Atendido -> Region [label = 'se encuentra recluido en']
    EstablecimientoPenitenciario -> Region [label = 'se encuentra ubicado en']
    CentroEducativo -> Region [label = 'pertenece a']
  }
")


