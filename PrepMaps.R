library(data.table)
library(sf)
library(stars)
library(raster)
library(fasterize)
library(colourvalues)

rast <- raster("./RasterData/cSI.tif")
NAvalue(rast) <- 0
crs(rast) <- 3005
cols <- c("#a86507ff","#e5ed00ff","#99ff00ff","#67d916ff",
          "#4a9911ff","#295708ff","#193605ff")
temp <- RGB(rast,filename = "cSI_RGB.tif", col = cols,breaks = 1:8, 
            alpha = T, overwrite = T)
temp$cSI_RGB.4[temp$cSI_RGB.1 == 255] <- 0
writeRaster(temp,"cSI_RGBA.tif", overwrite = T)

rast <- raster("./RasterData/Disturbance.tif")
NAvalue(rast) <- 0
crs(rast) <- 3005
cols <- c("#cf3f1f","#f09826","#c78306","#72a1ad",
          "#72a1ad","#5c331c","#5c331c")
temp <- RGB(rast, col = cols,breaks = seq(0.5,7.5,by = 1), 
            alpha = T, overwrite = T)
temp$alpha[temp$red == 255] <- 0
writeRaster(temp,"Disturb_RGBA.tif", overwrite = T)

rast <- raster("./RasterData/Protect_Select_2021.tif")
NAvalue(rast) <- 0
crs(rast) <- 3005
cols <- c("#787878")
temp <- RGB(rast, col = cols,breaks = c(0.5,1.5), 
            alpha = T, overwrite = T)
temp$alpha[temp$red == 255] <- 0
writeRaster(temp,"Protected_RGBA.tif", overwrite = T)

cols <- c("#ffecb3","#b2f200", "#78a302","#058f00", "#035700", "#032401")
rast <- raster("./RasterData/TreeHeight.tif")
NAvalue(rast) <- 0
crs(rast) <- 3005
cols <- c("#ffecb3","#b2f200", "#78a302","#058f00", "#035700", "#032401")
temp <- RGB(rast, col = cols,breaks = c(0,10,20,30,40,50,100), 
            alpha = T, overwrite = T)
values(temp$alpha) <- 255
temp$alpha[temp$red == 255 & temp$green == 255 & temp$blue == 255] <- 0
writeRaster(temp,"TreeHt_RGBA.tif", overwrite = T)

rast <- raster("./RasterData/TreeVolume100.tif")
NAvalue(rast) <- 0
crs(rast) <- 3005

cols <- c(viridis(100,alpha = NULL),rep("#000000",156))
colortable(rast) <- cols
temp <- RGB(rast,col = viridis(20),breaks = seq(0,1000,length.out = 21), alpha = T, overwrite = T)
temp$alpha[temp$red == 255 & temp$green == 255 & temp$blue == 255] <- 0
writeRaster(temp,"TreeVol_RGBA.tif", overwrite = T)

#####
rast <- raster("Raster_Template.tif")
vect <- st_read(dsn = "./OG_ShapeFiles/Ancient")
vect <- st_transform(vect, st_crs(rast))
rastTemp <- fasterize(vect, rast, field = "PolyID")
rast2 <- raster("./RasterData/FocalMeanSite.tif")
stk <- stack(rast2,rastTemp)
dat <- as.data.table(as.data.frame(stk))
dat <- na.omit(dat,cols = "layer")
setnames(dat,c("Values","PolyID"))
dat2 <- dat[,.(MeanSite = mean(Values)), by = .(PolyID)]
fwrite(dat2,"Ancient_MeanSite.csv")
# rst <- read_stars("Rare.tif",proxy = F)
# rst$Rare.tif <- as.numeric(rst$Rare.tif)
# st_crs(rst) <- 3005
# r <- as(rst,"Raster")
# writeRaster(r,"temp.tif",format = "GTiff", overwrite = T)

rst <- read_stars("./RasterData/SeralClass_New.tif",proxy = F)
rpoly <- st_as_sf(rst,merge = T,use_integer = T,na.rm = T)
st_write(rpoly,"SeralClass.gpkg")
colnames(rpoly)[1] <- "Seral"
rpoly <- rpoly[rpoly$Seral %in% c(3,4),]
library(rmapshaper)
rp2 <- ms_simplify(rpoly,keep = 0.1, sys = T)
st_write(rp2,dsn = "Seral34.gpkg")

vect <- st_read("./OG_ShapeFiles/Defer")
vect$Area <- st_area(vect)
vect$Area <- units::set_units(vect$Area,"ha")
d1 <- fread("Defer_AgeClass.csv")
d2 <- fread("Defer_MeanSite.csv")
d3 <- fread("Defer_TreeHeight.csv")
d4 <- fread("Defer_TreeVolume.csv")
d1[d2,MeanSite := i.MeanSite, on = "PolyID"]
d1[d3,TreeHeight := i.Height, on = "PolyID"]
d1[d4,TreeVol := i.Volumne, on = "PolyID"]

vect <- st_read("./OG_ShapeFiles/Rare")
vect$Area <- st_area(vect)
vect$Area <- units::set_units(vect$Area,"ha")
d1 <- fread("Rare_MeanSite.csv")
d2 <- fread("Rare_TreeHeight.csv")
d3 <- fread("Rare_TreeVolume.csv")
d1[d2,MeanSite := i.Height, on = "PolyID"]
d1[d3,TreeHeight := i.Volume, on = "PolyID"]


dat <- as.data.table(st_drop_geometry(vect))
dat <- dat[d1,on = "PolyID"]
dat[,Area := units::drop_units(Area)]
rareDat <- dat
save(rareDat,file = "Rare_Info.Rdata")

vect <- st_read("./OG_ShapeFiles/Ancient")
vect$Area <- st_area(vect)
vect$Area <- units::set_units(vect$Area,"ha")
d1 <- fread("Ancient_MeanSite.csv")
d2 <- fread("Ancient_TreeHeight.csv")
d3 <- fread("Ancient_TreeVolume.csv")
d1[d2,MeanSite := i.Height, on = "PolyID"]
d1[d3,TreeHeight := i.Volumne, on = "PolyID"]


dat <- as.data.table(st_drop_geometry(vect))
dat <- dat[d1,on = "PolyID"]
dat[,Area := units::drop_units(Area)]
ancientDat <- dat
save(rareDat,file = "Ancient_Info.Rdata")


###summaries by BGC
bgc <- st_read("~/../Desktop/Work2021/CommonTables/WNA_BGC_v12_12Oct2020.gpkg")
bgc <- bgc[is.na(bgc$State),]
rast <- raster("./RasterData/SeralClass_New.tif")
bgc$ID <- 1:nrow(bgc)
library(fasterize)
BGCInfo <- fread("./All_BGCs_Info_v12_2.csv")
bcBGCs <- BGCInfo[DataSet == "BC",BGC]
bgc <- bgc[bgc$BGC %in% bcBGCs,]
bgc$ID <- 1:nrow(bgc)
bgcLookup <- as.data.table(st_drop_geometry(bgc[,c("BGC","ID")]))
bgcRast <- fasterize(bgc,rast,field = "ID")

##forested part
SIClass <- raster("./RasterData/cSI_New.tif")
SeralSt <- raster("./RasterData/SeralClass_New.tif")
rbrick <- brick(bgcRast,SIClass,SeralSt)

dat <- crosstabDT(rbrick,long = T)
dat[bgcLookup,BGC := i.BGC, on = c(layer = "ID")]
dat[,layer := NULL]
setnames(dat,c("SIClass","Seral","Area","BGC"))
dat <- dat[!SIClass %in% c(1,2),]
allForest <- dat[,.(Area = sum(Area)), by = .(BGC,SIClass)]
allForest[,Var := "AllForest"]
oldForest <- dat[Seral == 4,.(BGC,SIClass,Area)]
oldForest[,Var := "OldForest"]
forestDat <- rbind(allForest,oldForest)
fwrite(forestDat,"ForestByBGC.csv")

##Deferal part
deferRast <- raster("./RasterData/Defer_New.tif")
rbrick <- brick(bgcRast,SIClass,deferRast)

dat <- crosstabDT(rbrick,long = T)
dat[bgcLookup,BGC := i.BGC, on = c(layer = "ID")]
dat[,layer := NULL]
dat <- dat[!cSI_New %in% c(1,2),]
deferLeg <- data.table(Defer_New = c(1,2,3),
                       Defer_Name = c("Best1-3","Best4-10","Ancient"))
dat[deferLeg,DeferName := i.Defer_Name, on = "Defer_New"]
dat[,Defer_New := NULL]
setnames(dat,c("SIClass","Area","BGC","Var"))
deferDat <- copy(dat)

##rare
rareRast <- raster("./RasterData/Rare_New.tif")
rbrick <- brick(bgcRast,SIClass,rareRast)

dat <- crosstabDT(rbrick,long = T)
dat[bgcLookup,BGC := i.BGC, on = c(layer = "ID")]
dat[,layer := NULL]
dat <- dat[!cSI_New %in% c(1,2),]
rareLeg <- data.table(Rare_New = c(1,2),
                       Rare_Name = c("RareVariant","RareLU"))
dat[rareLeg,RareName := i.Rare_Name, on = "Rare_New"]
dat[,Rare_New := NULL]
setnames(dat,c("SIClass","Area","BGC","Var"))
deferAll <- rbind(deferDat,dat)
fwrite(deferAll,"DeferByBGC.csv")


deferDat<- fread("./DeferByBGC.csv")
dat <- deferDat[BGC == "SBSdk",.(SIClass,Area,Var)]
dat <- dcast(dat, Var ~ SIClass, value.var = "Area")

forestDat <- fread("./ForestByBGC.csv")
dat2 <- forestDat[BGC == "SBSdk",.(SIClass,Area,Var)]
dat2 <- dcast(dat2, Var ~ SIClass, value.var = "Area")
