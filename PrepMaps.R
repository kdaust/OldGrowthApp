library(data.table)
library(sf)
library(stars)
library(raster)
library(fasterize)


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
cols <- c("#a86507ff","#e5ed00ff","#99ff00ff","#67d916ff",
          "#4a9911ff","#295708ff","#193605ff")
temp <- RGB(rast,filename = "cSI_RGB.tif", col = cols,breaks = 1:8, 
            alpha = T, overwrite = T)
temp$cSI_RGB.4[temp$cSI_RGB.1 == 255] <- 0
writeRaster(temp,"cSI_RGBA.tif", overwrite = T)

rast <- raster("Raster_Template.tif")
vect <- st_read(dsn = "./OG_ShapeFiles/Defer")
vect <- st_transform(vect, st_crs(rast))
rastTemp <- fasterize(vect, rast, field = "PolyID")
rast2 <- raster("./RasterData/FocalMeanTreeVolumeBEC.tif")
stk <- stack(rast2,rastTemp)
dat <- as.data.table(as.data.frame(stk))
dat <- na.omit(dat,cols = "layer")
setnames(dat,c("Values","PolyID"))
dat2 <- dat[,.(TreeVolume = mean(Values)), by = .(PolyID)]
fwrite(dat2,"Defer_TreeHeight.csv")
# rst <- read_stars("Rare.tif",proxy = F)
# rst$Rare.tif <- as.numeric(rst$Rare.tif)
# st_crs(rst) <- 3005
# r <- as(rst,"Raster")
# writeRaster(r,"temp.tif",format = "GTiff", overwrite = T)

rst <- read_stars("Ancient_Save.tif",proxy = F)
rpoly <- st_as_sf(rst,merge = T,use_integer = T,na.rm = T)
st_write(rpoly,"Ancient.gpkg")

dat <- st_read("./VectorData/Defer.gpkg")
vect$Area <- st_area(vect)
d1 <- fread("Defer_AgeClass.csv")
d2 <- fread("Defer_MeanSite.csv")
d3 <- fread("Defer_TreeHeight.csv")
d4 <- fread("Defer_TreeVol.csv")
d1[d2,MeanSite := i.MeanSite, on = "PolyID"]
d1[d3,TreeHeight := i.TreeHeight, on = "PolyID"]
d1[d4,TreeVol := i.TreeVolume, on = "PolyID"]

dat <- as.data.table(st_drop_geometry(vect))
dat <- dat[d1,on = "PolyID"]
save(dat,file = "Defer_Info.Rdata")


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
