###############################################################################
## Actualización de la tabla de composición para Ecuador ######################
## nueva compilación ##########################################################
###############################################################################
library(readxl)

######## Cargar Bases #########################################################
#### Base : tabla de composición de Cuenca  
tc <- read_xlsx("tabla_composicion_nova_2018v2.xlsx",sheet="ingredientes")
names(tc)[grep("^_g",names(tc))]<- "prot_aj"

# Match de identificadores Ensanut 2012  - Tc_Cuenca 
tp <- read_xlsx("tp_revise_SRleg_Nov18.xlsx")
names(tp)[3] <- "cd_t4"
tc <- merge(tc,tp[,c(1,3)],by="IngredientesCodigo",all.x=T)

#### Tabla_Composicion_v4 (correct errors not checking 100g)
t4 <- read_xlsx("TABLA_COMPOSICIÓN_v4.xlsx")
names(t4)[which(names(t4)=="cd_tcomp")] <- "cd_t4"

### USDR SR_legacy 
sr <- readRDS("SR-Leg_DB.RDS")

### Base de emparejamiento de nombres : Tc-Ec - USDR 
rnvars <- readxl::read_xlsx("matchvars.xlsx")
rnvars$v_tot <-  rnvars$v_sr
rnvars$v_tot[is.na(rnvars$v_sr)] <- rnvars$v_tec[is.na(rnvars$v_sr)]

######## Emparejar Bases #######################################################

### Censor srleg codes not matching : build Source variable 
tc$id_srleg[tc$IngredientesCodigo %in% tp$IngredientesCodigo] <- NA
tc$fuente_ <- NA 
tc$fuente_[!is.na(tc$id_srleg)] <- 1 # USDA SR_Leg
tc$fuente_[!is.na(tc$cd_t4)] <- 2 # Updated Ec table 
tc$fuente_[is.na(tc$fuente_)] <- 3 # Original Ec table 

### Build dbs Update : SrLeg (sr), Rework Tc_ec (t4), Original Tc_ec (tc)
# Update: Sr Legacy 
tc_sr <- tc[tc$fuente_==1,c("id_srleg","fuente_","IngredientesCodigo","alim")]
tc_sr <- merge(tc_sr,sr,by.x="id_srleg",by.y="food_id",all.x=T)

# Update V4 Tcomp  
tc_t4 <- tc[tc$fuente_==2,c("cd_t4","fuente_","IngredientesCodigo","id_srleg","alim")]
tc_t4 <- merge(tc_t4,t4[,-grep("fuente|alim",names(t4))],by="cd_t4", all.x=T)
for(i in names(tc_t4)[names(tc_t4) %in% rnvars$v_tec]){
  nnam <- rnvars$v_sr[which(rnvars$v_tec==i)]
  ifelse(is.na(nnam),nnam <- i, nnam <- nnam)
  names(tc_t4)[names(tc_t4)==i] <- nnam
} 

# Keep original Tcomp  
tc_tc <- tc[tc$fuente_==3,]
for(i in names(tc_tc)[names(tc_tc) %in% rnvars$v_tec]){
  nnam <- rnvars$v_sr[which(rnvars$v_tec==i)]
  ifelse(is.na(nnam),nnam <- i, nnam <- nnam)
  names(tc_tc)[names(tc_tc)==i] <- nnam
} 

# Rename and join 
ntot <- unique(c(names(tc_tc), names(tc_t4), names(tc_sr)))
tc_sr[,ntot[!ntot %in% names(tc_sr)]] <- NA
tc_t4[,ntot[!ntot %in% names(tc_t4)]] <- NA
tc_tc[,ntot[!ntot %in% names(tc_tc)]] <- NA
tc_up <- rbind(tc_tc,tc_t4,tc_sr)

# Remove / rename: 
tc_up$TablasCodigo <- 1
rmvs <- c("cd_tcomp","prot_aj","cd_t4","chk","food_id","cod_ori_")
tc_up <- tc_up[,!names(tc_up) %in% rmvs]
names(tc_up)[grep("id_srleg",names(tc_up))] <-  "food_id"
tc_up <- tc_up[,rnvars$v_tot[rnvars$v_tot %in% names(tc_up)]] # reorder

# Export
writexl::write_xlsx(tc_up,path = "tabla_composicion_nova_2025.xlsx")



