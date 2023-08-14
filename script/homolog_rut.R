
suppressPackageStartupMessages({
library(stringr)
library(haven)
library(dplyr)
library(stringdist)
})

#Usamos libreria lib_Reg_Nombres_dir.R


source("./script/lib_Reg_Nombres_dir.R")

DIRECTORIO <- read_sav("./data/DIRECTORIO.sav")

patron_name <- c("\\(", ")")
patron_name <- str_c(patron_name, collapse = "|")
##ELIMINAR PARENTESIS Y CREAR FILTRO POR RUT##
PRUEBA <- DIRECTORIO %>% 
  select(dir_FOLIO, Comuna = dir_comuna, NOMBRE = dir_nombre_pn, 
         APELL1= dir_ape_pat, APELL2 = dir_ape_mat, RAZONSOCIAL = dir_nombre_pj, 
         Tipo_Persona = dir_tipo_persona, RUT = dir_rut_explotacion, 
         RUT_DV = dir_rut_explotacion_dv) %>% 
  mutate(NOMBRE = str_remove(NOMBRE, pattern = "\\(") %>% str_remove(pattern = "\\)"),
         APELL1 = str_remove(APELL1, pattern = "\\(") %>% str_remove(pattern = "\\)"),
         APELL2 = str_remove(APELL2, pattern = "\\(") %>% str_remove(pattern = "\\)"),
         RAZONSOCIAL = str_remove(RAZONSOCIAL, pattern = "\\(") %>% str_remove(pattern = "\\)")) %>%
  mutate(NEW_Nombre = Reg_Nombre_Base(NOMBRE), 
         NEW_Apellido1 = Reg_Nombre_Base(APELL1), 
         NEW_Apellido2 = Reg_Nombre_Base(APELL2),
         NEW_RAZON = Reg_Nombre_Base(RAZONSOCIAL)) %>%
  mutate(NEW_Nombre = Reg_Tildes(NEW_Nombre), 
         NEW_Apellido1 = Reg_Tildes(NEW_Apellido1), 
         NEW_Apellido2 = Reg_Tildes(NEW_Apellido2),
         NEW_RAZON = Reg_Tildes(NEW_RAZON)) %>%
  mutate(NEW_Nombre = Reg_Nombres(NEW_Nombre), 
         NEW_Apellido1 = Reg_Nombres(NEW_Apellido1), 
         NEW_Apellido2 = Reg_Nombres(NEW_Apellido2),
         NEW_RAZON = Reg_Nombres(NEW_RAZON))


#Verificando RUT


dgv <- function(T) {
  M <- 0
  S <- 1
  while (T > 0) {
    S <- (S + (T %% 10) * (9 - M %% 6)) %% 11
    T <- floor(T / 10)
    M <- M + 1
  }
  
  if (S == 0) {
    return(10)
  } else {
    return(S - 1)
  }
}



PRUEBA_2 <- PRUEBA %>% 
  select(RUT, RUT_DV, NEW_RAZON, NEW_Nombre, NEW_Apellido1, NEW_Apellido2) %>%
  rowwise %>%
  mutate(RUT_DV = toupper(RUT_DV),
         Verif_RUT = dgv(RUT),
         Verif_DV = ifelse(Verif_RUT == 10 & RUT_DV %in% "K", TRUE,
                           ifelse(Verif_RUT == RUT_DV, TRUE, FALSE)),
         NOMBRE_Ap1 = paste(NEW_Nombre, NEW_Apellido1, NEW_Apellido2, NEW_RAZON) %>%
           str_remove(pattern = "^\\s+"),
         NOMBRE_Ap1 = ifelse(str_detect(NOMBRE_Ap1, "SIN INF") %in% TRUE, "SIN INFORMANTE", NOMBRE_Ap1))


nombres <- PRUEBA %>% select(NOMBRE_Ap1) %>% distinct()


rut <- PRUEBA %>%
  filter(Verif_DV %in% FALSE) %>%
  select(RUT, RUT_DV, NOMBRE_Ap1) %>% distinct()