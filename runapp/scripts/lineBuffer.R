kernlUD <- function(data){
  traj <- ltraj(data$lon, data$lat, timestamp = data$datetest, id = 1)
  
  ud <- kernelUD(traj, h = "href")
  # 95% contour home range
  hr_95 <- getverticeshr(ud, percent = 0.95)
  plot(hr_95)
  # 50% contour home range
  hr_50 <- getverticeshr(ud, percent = 0.5)
  plot(hr_50)
}

lineBuffer<- function(){
#data<- as.data.frame(importedDatasetMaster)
data <- data %>%
    
    arrange(newuid,id_yr,dateTest) #order database first
  
  
  
  # data$burst <- CalcBurst(data=data, id = TRUE,
  #                         
  #                         id_name="id_yr",
  #                         
  #                         date_name="dateTest", Tmax = 3600*7) #set Tmax to 7 hours, a little more than double the fix rate
  
  length(unique(data$burst))
  
  data <- CalcMovParams(data=data, id_name = "id_yr", date_name = "dateTest",
                        
                        burst=data$burst)
  
  
  
  # now create a lines from steps, where each row is its own connected step
  
  lns <- Points2Lines(data=data,
                      
                      date_name="dateTest",
                      
                      id_name="id_yr",
                      
                      byid=FALSE,  # put FALSE here so it does one line per step
                      
                      no_cores=11)  # must change this! Try detectCores()-1
  
  head(lns)
  
  nrow(data)
  
  nrow(lns)
  
  
  
  # now, must remove steps with relatively large gaps between them (e.g., > 8 hrs)
  
  # I am just going to use my burst column for this, which I specified as less than 7 hours
  
  table(data$StepFlag)
  
  
  
  lns <- lns %>%
    
    filter(StepFlag == TRUE)
  
  
  
  # now buffer the lines by the differt buff values and
  
  # create a sf database with all the home ranges for each id and the 3 different buff levels
  
  linebuffs %>%
    
    lns %>%
    
    st_buffer(dist=300) %>%   # buffer each step
    
    group_by(id_yr) %>%
    
    summarise(do_union = TRUE)  # st_union each steps' buffered line into a single polygon
  
  
  
  head(linebuffs)
  
  tail(linebuffs)
  
  table(linebuffs$id_yr)
  
  
  
  # now add the sizes
  
  linebuffs$HR_size_km2 <- as.numeric(st_area(linebuffs))/1000000
  
  hist(linebuffs$HR_size_km2)
}

library(raster)
library(sf)
ext <- raster::extent(data)


CalcPopGrid <- function(datasf=d,
                        out.fldr=getwd(),
                        mult4buff=0.3,
                        cell.size=500){
  
  
  multiplyers <- c((ext[2]-ext[1])*mult4buff, (ext[4]-ext[3])*mult4buff)   # add about 20% around the edges of your extent (you can adjust this if necessary)
  ext <- raster::extend(ext, multiplyers)
  grd <- raster(ext)
  res(grd) <- cell.size     
  projection(grd) <- st_crs(datasf)$proj4string
  grd[] <- 0  # put some 0s in there so it isn't empty
  
  rm(ext, multiplyers, datasf) #remove objects
  gc()
  
  #write out grid
  writeRaster(grd, filename=paste0(out.fldr,"/PopGrid_empty.tif"), 
              format = "GTiff", overwrite = TRUE)
  
  return(paste0("Your population grid has ", ncell(grd), " cells! It has been written to your out.fldr"))
}  

grd <- raster(Pop.grid)

