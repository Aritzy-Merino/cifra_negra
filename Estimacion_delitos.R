rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyr, tidyverse, data.table, ggplot2, modeest, psych, tidyr, haven, survey)

#### Se fijan parÃ¡metros del Ã¡rea de trabajo
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)
