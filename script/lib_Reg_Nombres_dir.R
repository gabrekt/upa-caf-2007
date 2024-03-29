#
# lib_Reg_Nombres.R -- Librer�a regularizar nombres
#
#  source("src/lib_Reg_Nombres.R")
#



library(futile.logger)
library(stringi)
# library(testthat)

#
# Functions
#

Reg_Trim <- function(v) {
  # Nota:
  # todas las formas de espacio en regex es [\h\v], eso es m�s completo que [\\s]
  # En clase Unicode es equivalente a '\\p{Wspace}', pero esa notaci�n no est� soportada por PCRE (usado por sub()/gsub() )
  #
  
  v  %>% str_trim()  %>% 
    stri_replace_all_charclass("\\p{Wspace}", " ", merge=TRUE) %>%  # homologar todas las formas de espacio en espacio normal, incluyendo algunos caracteres de control
    pf_gsub( "^[\\s[:cntrl:]]+|[\\s[:cntrl:]]+$|[[:cntrl:]]+", "")  # Eliminar espacios o caracteres de control al inicio o al final, caracteres de control (restantes) en cualquier parte
  
}


Reg_Tildes <- function(v) {
  
  # "Le�n �u�oa d and �" %>% iconv(to='ASCII//TRANSLIT')
  
  v  %>% 
    iconv(from="UTF8", to='ASCII//TRANSLIT')
}



Reg_Nombre_Base <- function(s, min_len = 2) {
  # b�sicos
  
  ret <- toupper(s) %>%
    pf_gsub( ".[��]", "") %>%                # "�" comunmente presionada por error en lugar de 'BACKSPACE'
    pf_gsub( "N[|]", "N�") %>%               # "|" ocurre si se presiona [�] sin SHIFT
    pf_gsub( "[{|<>^~_?]+", "") %>%           # Caracteres que no parecen tienen utilidad y suelen ser tipeados por error
    pf_gsub( "&YEN;",    "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&AMP;",    "&", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "[��]", "�") %>%
    pf_gsub( "&#39;", "'",    perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&AACUTE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&AUML;",   "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&EACUTE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&IACUTE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&MINUS;",  "-", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&NDASH;",  "-", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&NTILDE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&OACUTE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&OSLASH;", "/", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&UACUTE;", "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&UUML;",   "�", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "&DEG;",    "�", perl=FALSE, fixed=TRUE) %>%     # 'DEGREE SIGN' : '�'
    pf_gsub( "&ORDM;",   "�", perl=FALSE, fixed=TRUE) %>%     # 'MASCULINE ORDINAL INDICATOR' : '�'
    pf_gsub( "&ORDF;",   "�", perl=FALSE, fixed=TRUE) %>%     # 'FEMININE ORDINAL INDICATOR' : '�'
    pf_gsub( "&NBSP;",   " ", perl=FALSE, fixed=TRUE) %>%
    pf_gsub( "[�}:;+�*]+", " ") %>%          # '}' comunmente es presionada por error en lugar de ENTER, preferimos considerar como separadores
    pf_gsub( "[`�''\u201C\u201D]+", "'")   %>%                # 'GRAVE ACCENT', 'ACUTE ACCENT', 'LEFT SINGLE QUOTATION MARK' y 'RIGHT SINGLE QUOTATION MARK', 'LEFT DOUBLE QUOTATION MARK', 'RIGHT DOUBLE QUOTATION MARK' -> 'APOSTROPHE'
    pf_gsub( "'P", "P", perl=FALSE, fixed=TRUE) %>%           # Error de tipeo com�n " `PONCE"
    Reg_Trim() %>%
    pf_gsub( "N\\s?[|!��]", "N�") %>%          # "|" ocurre si se presiona [�] sin SHIFT
    pf_gsub( "\\bO\\s?[']\\s?([[:alpha:]])",   "O'\\1" ) %>%
    pf_gsub( "\\bPEDR0\\b",   "PEDRO"     ) %>%
    pf_gsub( "\\bCOIHAIQUE\\b", "COYHAIQUE" ) %>%
    pf_gsub( "\\bAIS[�E]N\\b",  "AYS�N"     ) %>%
    pf_sub( "^[-:;|.,?!�`'�'/]+\\s*", "")  %>%          # texto comienza con puntuaci�n
    pf_sub( "[-:;��,`'�/[�]$",     "") %>%              # texto termina con ciertos signos de puntuaci�n
    pf_sub( "([[:alpha:]]{5})[.]$", "\\1") %>%          # " PARC.$" -> " PARC." ; " APARC.$" -> "APARC"
    pf_gsub("([[:punct:]])\\s?\\1+", "\\1")   %>%       # "---" -> "-" ; "- -" -> "-"
    pf_gsub( "\\s?[(]\\s?", " (") %>% 
    pf_gsub( "\\s?[)]\\s?", ") ") %>% 
    pf_gsub( "\\s([.,])\\s?", "\\1 ") %>%               # " . X" -> ". X" , " .X" -> ". X"
    pf_sub("^\\(\\s*([^()]+)\\s*\\)$", "\\1") %>%       # "(SUBSECTOR SAN LUIS)" -> "SUBSECTOR SAN LUIS"
    str_trim()
  
  idx <- which(nchar(ret) < min_len)  
  
  ret[idx] <- ""
  
  ret
}


Reg_Nombre_Geo <- function(s) {
  # b�sicos

  s %>%
    Reg_Nombre_Base()
}

Reg_Nom_Region <- function(v) {
  v  %>% 
    gsub("^DEL ", "", .) %>% 
    gsub("^LIBERTADOR B. O'HIGGINS$", "LIBERTADOR GENERAL BERNARDO O'HIGGINS", .) %>% 
    gsub("^B�OB�O$", "BIOB�O", .) %>% 
    gsub("AIS.N DEL GRAL. C. ", "AYS�N DEL GENERAL CARLOS ", .) %>% 
    gsub("^DE ",  "", .)
}

Reg_Nom_Comuna <- function(v) {
  v %>% 
    Reg_Nombre_Base() %>% 
    pf_gsub("&NTILDE;", "�") %>%
    pf_gsub("^CON CON$",            "CONCON"          ) %>%
    pf_gsub("^EST\\.?\\s+CENTRAL$", "ESTACI�N CENTRAL") %>%
    pf_sub("^ALTO B[I�]O B[I�]O$",  "ALTO BIOB�O"  ) %>% 
    pf_sub("^LA CALERA$",           "CALERA"    )  %>% 
    pf_sub("^LLAY\\s*-?\\s*LLAY$",  "LLAILLAY"  )  %>% 
    pf_sub("^OHIGGINS$",            "O'HIGGINS" ) %>% 
    pf_sub("^MARCHIGUE$",           "MARCHIHUE" )  %>% 
    pf_sub("^P AGUIRRE CERDA$",     "PEDRO AGUIRRE CERDA"  ) %>% 
    pf_sub("^PAIHUANO$",            "PAIGUANO"  )  %>% 
    pf_sub("^PTO\\. MONTT$",        "PUERTO MONTT"   )  %>% 
    pf_sub("^PTO\\. NATALES$",      "NATALES"        )  %>% 
    pf_sub("^PTO\\. OCTAY$",        "PUERTO OCTAY"   )  %>% 
    pf_sub("^PTO\\. VARAS$",        "PUERTO VARAS"   )  %>% 
    pf_sub("^PUERTO AYSEN$",        "AYS�N"  ) %>% 
    pf_sub("^PUERTO NATALES$",      "NATALES"        )  %>% 
    pf_sub("^PUERTO SAAVEDRA$",     "SAAVEDRA"  ) %>% 
    pf_sub("^QUINTA TILCOCO$",      "QUINTA DE TILCOCO")  %>% 
    pf_sub("^S.J. DE LA COSTA$",    "SAN JUAN DE LA COSTA"  )  %>% 
    pf_sub("^SAN JOSE MAIPO$",      "SAN JOS� DE MAIPO"  )  %>% 
    pf_sub("^SAN JUAN$",            "SAN JUAN DE LA COSTA"  ) %>% 
    pf_sub("^STA. B[A�]RBARA$",     "SANTA B�RBARA"  ) %>% 
    pf_sub("^TIL\\s*-?\\s*TIL$",    "TILTIL"    ) %>% 
    pf_sub("^TORRES DEL PAYNE$",    "TORRES DEL PAINE"    ) %>% 
    pf_sub("^TREHUACO$",            "TREGUACO"  )
}


Reg_Nombres_Natural <- function(v) {
  v %>% 
    Reg_Nombre_Base() %>%
    pf_sub( "\\s*[-]\\s*",  "-") %>%
    pf_sub( "^SUCE[CS]ION\\b\\s*",       "SUC. "  ) %>% 
    pf_sub( "\\(\\s*SUCE[CS]ION\\s*\\)", "(SUC.)" ) %>%
    pf_sub( "^\\(SUC[.]?\\s?\\)",        "SUC. "  ) %>%
    pf_gsub("^([^()]+)\\(\\s*\\SUC\\s*[.]?\\s*\\)\\s*","SUC. \\1") %>%
    pf_gsub( "([[:alpha:]]+)[.]([[:alpha:]])", "\\1. \\2") %>% 
    Reg_Nombres()
}






Reg_Nombres <- function(s, min_len = 3) {
  # b�sicos

  ret <- Reg_Nombre_Base(s) %>%
    Reg_RZ_Abrev() %>%
    pf_gsub( "\\bCOMUNIDAD\\b[ .,]*",    "COM. ") %>%
    pf_gsub( "\\bLIMITADA\\b[ .,]*",     "LTDA. ") %>%
    pf_gsub( "\\bAGR[I�]COLA\\b[ .,]*",  "AGRIC. ") %>%
    pf_gsub( "\\bSOCIEDAD\\b[ .,]*",     "SOC. ") %>%
    pf_gsub( "\\bINVERSIONES\\b[ .,]*",  "INVER. ") %>%
    pf_gsub( "\\bINMOBILIARIA\\b[ .,]*", "INMOB. ") %>%
    pf_gsub( "\\bLTDA$",          "LTDA.") %>%
    pf_sub( "^\\(SUC[.]?\\s?\\)", "SUC. ") %>%
    str_trim() %>%
    pf_gsub( "^SUCE[CS]ION\\b[ .,]*",  "SUC. ") %>%
    pf_sub( "^(.+\\S)\\s?\\(\\s*SUCE[CS]ION\\s*\\)", "SUC. \\1") %>%
    pf_sub( "^(.+\\S)\\s?\\(\\s*SUC[.]?\\s*\\)",     "SUC. \\1") %>%
    pf_sub( "^(S[UO]C)\\s?\\.",             "\\1.") %>%
    pf_sub( "^(S[UO]C)\\s?\\.(\\S)",        "\\1. \\2") %>%
    pf_sub( "^(.+)\\s?- SUCE[CS]ION$",      "SUC. \\1") %>%
    pf_gsub("\\B([A-Z])\\1{2,}\\B",            "\\1\\1"  ) %>%
    pf_sub( "^[.,`�/]",                     "")
  
  idx <- which(nchar(ret) < min_len)  

  ret[idx] <- ""
  
  ret
}




Reg_Direcc <- function(s, min_len = 2) {
  # b�sicos
  
  s %>%
    pf_gsub( "SIN\\s*N[���]", "S/N", ignore.case = TRUE) %>%
    Reg_Nombre_Base(min_len) %>%
    pf_gsub( "&NTILDE;", "�") %>%
    pf_gsub( "&AACUTE;", "�") %>%
    pf_gsub( "&EACUTE;", "�") %>%
    pf_gsub( "&IACUTE;", "�") %>%
    pf_gsub( "&OACUTE;", "�") %>%
    pf_gsub( "&UACUTE;", "�") %>%
    pf_gsub("[[:space:]]", " ") %>%
    str_trim() %>%
    pf_gsub( '""', "") %>%
    pf_gsub( "[}]", "") %>%
    pf_gsub( "\\s+", " ") %>%
    pf_gsub( "&DEG;", "�") %>%                          # 'DEGREE SIGN' : '�'
    pf_gsub( "&ORDM;","�") %>%                          # 'MASCULINE ORDINAL INDICATOR' : '�'
    pf_gsub( "[�]", "�") %>%
    pf_gsub( "N\\s*º\\s*(\\d*)",  "N� \\1") %>%        # 'EL MANZANITO SN º' , 'PARCELA Nº60'
    pf_gsub( "N\\s*ð\\s*MERO",  "N�MERO") %>%                # 'NðMERO'
    pf_gsub( "\\bNR?[0O]\\.?\\s*(\\d+)", "N� \\1" ) %>% # 'PARCELA N� 1 SAN JUAN EL TRANQUE'
    pf_gsub("\\bN[-.�]\\s*(\\d)",        "N� \\1" ) %>%
    pf_gsub( "�?�", "'") %>%                            # AVENIDA O´HIGGINS
    pf_gsub( '"', "'") %>%
    pf_sub( "S/$", "S/N") %>%
    pf_sub( "[:.,`'�/[]$", "") %>%
    pf_gsub("([[:punct:]])\\1+", "\\1") %>%
    pf_gsub( "\\*",     "�") %>%
    pf_gsub( "N[:_?|]\\s*(\\d)", "N� \\1") %>% 
    pf_gsub( "N\\s*�\\s*(\\d)",  "N� \\1") %>% 
    pf_gsub( "(\\w)[?��](\\w)", "\\1�\\2") %>%
    pf_gsub( "\\W\\?(\\w)", "\\1�\\2") %>%
    pf_sub("BERNARDO O%$", "BERNARDO O'HIGGINS") %>%
    pf_gsub("S\\s*\\\\\\s*N", "S/N")%>% pf_gsub("([[:alpha:]])\\\\([[:alpha:]])", "\\1�\\2") %>% pf_gsub("\\\\", "/") %>%  # backslash
    pf_gsub("\\bS[-_]N\\b", "S/N") %>% pf_gsub("_", " ")  %>%   # underscore, 'S-N'
    pf_gsub("\\bSIN N[�U]MERO\\b", "S/N") %>% pf_gsub("_", " ")  %>%
    pf_gsub( "\\bS/N�", "S/N") %>%                     # 'S/N�' -> 'S/N'
    pf_sub( "^[?:.,`'�/]", "") %>%                     # Signo de puntuaci�n al final del texto
    pf_gsub( "\\s+,\\s*", ", ") %>%                    # '   ,'
    pf_gsub("([A-Z])\\s*-\\s*([A-Z])", "\\1 - \\2") %>% 
    pf_gsub("\\s*#\\s+([0-9])", " \\1") %>%
    str_trim() %>%
    pf_gsub( "\\s+", " ")
}


Reg_Direcc_Rural <- function(s, min_len = 2) {
  # b�sicos
  
  s %>%
    Reg_Direcc(min_len) %>%
    pf_gsub("\\bLOTE(\\d+)",     "LT. \\1" ) %>%
    pf_gsub("\\bSITIO(\\d+)",    "ST. \\1" ) %>%
    pf_gsub("\\bHIJUELA(\\d+)",  "HJ. \\1" ) %>%
    pf_gsub("\\bSITIO\\b\\.*\\s*",    "ST. " ) %>%
    pf_gsub("\\bFUNDO\\b\\.*\\s*",    "FDO. ") %>%
    pf_gsub("\\bLOTE\\b\\.*\\s*",     "LT. " ) %>%
    pf_gsub("\\bESTANCIA\\b\\.*\\s*", "EST. ") %>%
    pf_gsub("\\bSANTA\\b\\.*\\s*", "STA. ") %>%
    pf_gsub("\\bHIJUELAS?\\b\\.*\\s*", "HJ. ") %>%
    pf_gsub("\\bPARCELA\\b\\.*\\s*", "PC. ")
}

Reg_RZ_Abrev <- function(s) {
  # Asume Reg_Nombre_Base() previo
  
  s %>%
    pf_gsub("\\bE[.,]*\\s*[IL][.,]*\\s*R[.,]*\\s*L\\b[.,]*",     "E.I.R.L." ) %>%
    pf_gsub("\\bCOMPA[�N][�I]A\\b[.,]*",     "CIA" ) %>%
    pf_gsub("\\bSOCIEDAD\\s+DE\\s+RESPONSABILIDAD\\s+LIMITADA\\b",     "S.R.L." ) %>%
    pf_gsub("\\bLTDA\\b[.,]*",        "LTDA.") %>%
    pf_gsub("\\bLIMITADA\\b[.,]*",    "LTDA.") %>%
    pf_gsub("\\bSOCIEDAD\\s+ANONIMA\\s+INDUSTRIAL\\s+Y\\s+COMERCIAL\\b",     "S.A.I.C." ) %>%
    pf_gsub("\\bS[., ]*A[., ]*I[., ]*C\\b[.,]*",     "S.A.I.C." ) %>%
    pf_gsub("\\bS[.,]*\\s*R[.,]*\\s*L\\b[.,]*",     "S.R.L." ) %>%
    pf_gsub("\\bSOCIEDAD\\s+ANONIMA\\s+ABIERTA\\b",     "S.A.A." ) %>%
    pf_gsub("\\bSOC[.,]?\\s+ANONIMA\\s+ABIERTA\\b",     "S.A.A." ) %>%
    pf_gsub("\\bS[ .,]+\\s*A[ .,]+\\s*A\\b[ .,]*",     "S.A.A." ) %>%
    pf_gsub("\\bS[.,]*\\s*A\\b[.,]*",     "S.A." )
}



Reg_Direcc_Simil <- function(s) {
  # b�sicos
  
  toupper(s) %>%
    pf_gsub( "\\bHIJ\\.", "HJ") %>%
    gsub("\\s\\s+",   " ", .) %>%
    gsub("[.,-:]+",   " ", .) %>%
    gsub("\\bDEL\\b", "~", .) %>%
    gsub("\\bDE\\b",  "~", .) %>%
    gsub("\\bDE LOS\\b", "~", .) %>%
    gsub("\\bDE LAS?\\b", "~", .) %>%
    gsub("\\bEL\\b",  "~", .) %>%
    gsub("\\bLA\\b",  "~", .) %>%
    gsub("\\bLOS\\b", "~", .) %>%
    gsub("\\bLAS\\b", "~", .) %>%
    gsub("~ ~", "~", .) %>%
    sub( "^\\s+", "", .) %>%
    sub( "\\s+$", "", .) %>%
    sub( "^[?:.,`'�/]", "", .) %>%
    gsub("\\bSITIO\\b", "ST", .) %>%
    gsub("\\bFUNDO\\b", "FDO", .) %>%
    gsub("\\bLOTE[SO]?\\b", "LT", .) %>%
    gsub("\\bESTANCIA\\b", "EST", .) %>%
    gsub("\\bSANTA\\b", "STA", .) %>%
    gsub("\\bHIJUELA\\b", "HJ", .) %>%
    gsub("\\bPARCELA\\b", "PC", .)
}



#
#
#


FlatText <- function(s, sin.tildes=TRUE) {
  s <- toupper(s) %>%
    pf_gsub("[-/.,���]+", " ") %>%
    Reg_Trim() %>%
    pf_gsub( "\\b([[:alpha:]])[ .]([[:alpha:]])[ .]([[:alpha:]])[ .]([[:alpha:]])\\b", "\\1\\2\\3\\4") %>%
    pf_gsub( "\\b([[:alpha:]])[ .]([[:alpha:]])[ .]([[:alpha:]])\\b",                  "\\1\\2\\3") %>%
    pf_gsub( "\\b([[:alpha:]])[ .]([[:alpha:]])\\b",                                   "\\1\\2")
  
  if (sin.tildes) {
    s <- s %>% Reg_Tildes()
  }
  
  idx <- which(is.na(s))
  s[idx] <- ""
  
  s
}


#
# Nros de telefono
#


#
# Versiones "pipe friendly" de funciones de de RegExp
#

pf_grepx <- function(x, pattern, ignore.case = FALSE, perl = TRUE, value = FALSE
                    ,fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  idx <- grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)
  x[idx]
}

pf_grepi <- function(x, pattern, ignore.case = FALSE, perl = TRUE, value = FALSE
                     ,fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)
}

pf_grepl <- function(x, pattern, ignore.case = FALSE, perl = TRUE
                    ,fixed = FALSE, useBytes = FALSE) {
  grepl(pattern, x, ignore.case, perl, fixed, useBytes)
}

pf_gsub <- function(x, pattern, replacement, ignore.case = FALSE, perl = TRUE,
                    fixed = FALSE, useBytes = FALSE) {
  gsub(pattern, replacement, x, ignore.case, perl, fixed, useBytes)
}

pf_sub <- function(x, pattern, replacement, ignore.case = FALSE, perl = TRUE,
                    fixed = FALSE, useBytes = FALSE) {
  sub(pattern, replacement, x, ignore.case, perl, fixed, useBytes)
}


#
# Calculo de similitud, de a pares
#

Simil_Num <- function(x,y) {
  vMax <- pmax(x,y, na.rm=TRUE)
  pmin(x,y) / vMax
}

Simil_Char_Lv <- function(x,y) {
  vMax <- pmax( nchar(x), nchar(y), na.rm=TRUE)
  
  1 - stringdist(x,y, method="lv") / vMax
}

Simil_Char_Bigram <- function(x,y) {
  #plr-: reemplazado por llamada a rutina 'stringdist' equivalente
  # vMax <- (nchar(x) + nchar(y) - 2)
  # 
  # ret <- 1- stringdist(x,y, method='qgram', q=2) / vMax
  # 
  # ret
  
  stringsim(x, y, method = "qgram", q = 2)
}

Simil_Char_Jaro <- function(x,y) {
  1- stringdist(x,y, method='jw', p=0.1)
}

#
# ---
#

Cat_Nombres <- function(sn, sap, sam) {
  
  sn[is.na(sn)]   <- ""
  sap[is.na(sap)] <- ""
  sam[is.na(sam)] <- ""
  
  sep_sap <- if_else( nzchar(sap) & nzchar(sn) , " ", "")
  sep_sam <- if_else( nzchar(sam) & (nzchar(sap) | nzchar(sn)), " ", "")
  
  paste0(sn
         ,sep_sap, sap
         ,sep_sam, sam
  )
}



#
# EOF
#
