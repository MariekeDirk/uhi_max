#'@title Blurr grid using focal functions
#'@description Focal functions to smooth sky view factor and vegetation fraction
#'for the UHImax calculation of a grid. Smoothings used by Koopmans et al.2018 was
#'with a radius of 500m and a final resolution of 100m.The median value is calculated within
#'the radius.
#'@importFrom raster focalWeight focal
#'@param raster_grid raster grid which needs to be smoothed
#'@param radius radius over which the focal function smooths
#'@param final_res output resolution of the raster
#'@export
blur_svf_fveg<-function(raster_grid,radius=250,final_res=50){
  message(paste0("change resolution to ",final_res,"m"))
  current_res<-res(raster_grid)
  raster_grid<-aggregate(raster_grid,fact=final_res/current_res[1])

  message(paste0("determining weights using a ",radius,"m radius"))
  weight_smooth<-focalWeight(raster_grid,d=radius,type="circle")

  message("calculating new grid")
  grid_smooth<-focal(raster_grid,weight_smooth,fun=function(x){sum(x,na.rm=TRUE)})

  return(grid_smooth)
}
