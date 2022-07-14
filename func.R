# Modelleringsfunktion

gamFunc <- function(data, vars){
  # gam(abbo_noll_tot_konv_PA ~
  #       s(Depth, bs = "tp", k=4) +
  #       s(DistToOffshore250, bs = "tp", k=4) +
  #       s(lgSWM250, bs = "tp", k=4) +
  #       s(Siktdjup250, bs = "tp", k=4) +
  #       s(temp16_19, bs = "tp", k=4) +
  #       s(sal16_19, bs = "tp", k=4) +
  #       s(lek_sötvatten, bs = "tp", k=4) +
  #       s(Storspigg_uppväxt_ostkust_2021, bs = "tp", k=4),
  #     data=x,
  #     method = "REML",
  #     family = binomial("logit"))
  string <- paste0("gam(abbo_noll_tot_konv_PA ~ ", 
                   paste0("s(", vars, ", bs = 'tp', k=4)", collapse = " + "), 
                   ", data=", data, ", method = 'REML', family = binomial('logit'))")
  eval(parse(text = string))
}



# Responskurvor
responsFunc <- function(mod){
  plot(mod, pages=1, shade = T, rug = T)
}

# Validering
precision <- function(c, mod, data) {
  tab1 <- table(fitted(mod) >= c, data)
  out <- diag(tab1) / apply(tab1, 2, sum)
  names(out) <- c('specificity', 'sensitivity')
  list(tab1, out)
}

valideringFunc <- function(mod, data){
  pred1 <- prediction(fitted(mod), data$abbo_noll_tot_konv_PA)
  stats1 <- performance(pred1,'tpr', 'tnr')
  
  mdt_ind <- which.min(abs(stats1@x.values[[1]]-stats1@y.values[[1]]))
  mdt_N_e <- stats1@alpha.values[[1]][mdt_ind]
  mst_ind <- which.max(stats1@x.values[[1]]+stats1@y.values[[1]])
  mst_N_e <- stats1@alpha.values[[1]][mst_ind]
  return(list(mdt_ind = mdt_ind,
              mdt_N_e = mdt_N_e,
              mst_ind = mst_ind,
              mst_N_e = mst_N_e,
              stats = stats1))
}

plotMDTFunc <- function(mdtInd, mstInd, stats){
  plot(stats@alpha.values[[1]], stats@x.values[[1]], xlab = stats@alpha.name, ylab = 'Classification rate', type = 's')
  lines(stats@alpha.values[[1]], stats@y.values[[1]], type = 's', col = 2)
  legend('right', c('specificity', 'sensitivity', 'MDT', 'MST'), col = c(1,2,4,'seagreen'), lty = c(1,1,2,3), lwd = c(1,1,1,2), bty = 'n', cex = .9)
  abline(v = stats@alpha.values[[1]][mdtInd], lty = 2, col = 4)
  abline(v = stats@alpha.values[[1]][mstInd], lty = 3, col = 'seagreen', lwd = 2)
}

plotMSTFunc <- function(mstInd, stats){
  plot(stats@alpha.values[[1]], stats@x.values[[1]] + stats@y.values[[1]], ylab = expression('Sensitivity'+'Specificity'), xlab = 'cutoff', type = 's')
  abline(v = stats@alpha.values[[1]][mstInd], lty = 3, col = 'seagreen', lwd = 2)
  legend('topright', 'MST', lty = 3, lwd = 2, col = 'seagreen', bty = 'n', cex = .9)
}

# # Prediktioner
# predFunc <- function(){
#   p_N_e <- predictFunc(point_N_e + point_N_l, mod_N_e) * 100
#   storage.mode(p_N_e[]) = "integer"
#   p_N_l <- predictFunc(point_N_e + point_N_l, mod_N_l) * 100
#   storage.mode(p_N_l[]) = "integer"
#   p_S_e <- predictFunc(point_S_e + point_S_l, mod_S_e) * 100
#   storage.mode(p_S_e[]) = "integer"
#   p_S_l <- predictFunc(point_S_e + point_S_l, mod_S_l) * 100
#   storage.mode(p_S_l[]) = "integer"
#   
#   point_N_e_wgs84 <- spTransform(point_N_e, "+init=epsg:4326")
#   point_N_l_wgs84 <- spTransform(point_N_l, "+init=epsg:4326")
#   point_S_e_wgs84 <- spTransform(point_S_e, "+init=epsg:4326")
#   point_S_l_wgs84 <- spTransform(point_S_l, "+init=epsg:4326")
#   
#   pal_N_e <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(p_N_e), na.color = "transparent")
#   pal_N_l <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(p_N_l), na.color = "transparent")
#   pal_S_e <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(p_S_l), na.color = "transparent")
#   pal_S_l <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(p_S_e), na.color = "transparent")
#   
#   pal1 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$Depth), na.color = "transparent")
#   pal2 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$DistToOffshore250), na.color = "transparent")
#   pal3 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$lek_sötvatten), na.color = "transparent")
#   pal4 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$lgSWM250), na.color = "transparent")
#   pal5 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$Siktdjup250), na.color = "transparent")
#   pal6 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$sal16_19), na.color = "transparent")
#   pal7 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$Storspigg_uppväxt_ostkust_2021), na.color = "transparent")
#   pal8 <- colorNumeric(brewer.pal(11, 'RdBu')[10:1], values(rst$temp16_19), na.color = "transparent")
#   
#   leaflet() |>
#     addProviderTiles("CartoDB.Positron") |>
#     addRasterImage(p_N_e, colors = pal_N_e, group = "Norr 78-04") |>
#     addRasterImage(p_N_l, colors = pal_N_l, group = "Norr 05-19") |>
#     addRasterImage(p_S_e, colors = pal_S_e, group = "Syd 78-04") |>
#     addRasterImage(p_S_l, colors = pal_S_l, group = "Syd 05-19") |>
#     addRasterImage(rst$Depth, colors = pal1, group = names(rst$Depth), opacity = 0.8) |>
#     addRasterImage(rst$DistToOffshore250, colors = pal2, group = names(rst$DistToOffshore250), opacity = 0.8) |>
#     addRasterImage(rst$lek_sötvatten, colors = pal3, group = names(rst$lek_sötvatten), opacity = 0.8) |>
#     addRasterImage(rst$lgSWM250, colors = pal4, group = names(rst$lgSWM250), opacity = 0.8) |>
#     addRasterImage(rst$Siktdjup250, colors = pal5, group = names(rst$Siktdjup250), opacity = 0.8) |>
#     addRasterImage(rst$sal16_19, colors = pal6, group = names(rst$sal16_19), opacity = 0.8) |>
#     addRasterImage(rst$Storspigg_uppväxt_ostkust_2021, colors = pal7, group = names(rst$Storspigg_uppväxt_ostkust_2021), opacity = 0.8) |>
#     addRasterImage(rst$temp16_19, colors = pal8, group = names(rst$temp16_19), opacity = 0.8) |>
#     addMarkers(data = point_N_e_wgs84, group = "Provtagningspunkter Norr 78-04") |> 
#     addMarkers(data = point_N_l_wgs84, group = "Provtagningspunkter Norr 05-19") |>
#     addMarkers(data = point_S_e_wgs84, group = "Provtagningspunkter Syd 78-04") |> 
#     addMarkers(data = point_S_l_wgs84, group = "Provtagningspunkter Syd 05-19") |>
#     addLayersControl(baseGroups = c("Norr 78-04",
#                                     "Norr 05-19",
#                                     "Syd 78-04",
#                                     "Syd 05-19",
#                                     names(rst)[c(1,2,4,5,7,8,9,10)]),
#                      overlayGroups = c("Provtagningspunkter Norr 78-04",
#                                        "Provtagningspunkter Norr 05-19",
#                                        "Provtagningspunkter Syd 78-04",
#                                        "Provtagningspunkter Syd 05-19"),
#                      options = layersControlOptions(collapsed = FALSE)) |>
#     hideGroup("Provtagningspunkter Norr 78-04") |> 
#     hideGroup("Provtagningspunkter Norr 05-19") |> 
#     hideGroup("Provtagningspunkter Syd 78-04") |> 
#     hideGroup("Provtagningspunkter Syd 05-19") |> 
#     addLegend(pal = pal_N_e, values = values(p_N_e), layerId = "Norr 78-04", position = c("bottomleft")) |>
#     addLegend(pal = pal_N_l, values = values(p_N_l), layerId = "Norr 05-19", position = c("bottomleft")) |>
#     addLegend(pal = pal_S_e, values = values(p_S_e), layerId = "Syd 78-04", position = c("bottomleft")) |>
#     addLegend(pal = pal_S_l, values = values(p_S_l), layerId = "Syd 05-19", position = c("bottomleft")) |>
#     addLegend(pal = pal1, values = values(rst$Depth), layerId = names(rst$Depth), position = c("bottomleft")) |>
#     addLegend(pal = pal2, values = values(rst$DistToOffshore250), layerId = names(rst$DistToOffshore250), position = c("bottomleft")) |>
#     addLegend(pal = pal3, values = values(rst$lek_sötvatten), layerId = names(rst$lek_sötvatten), position = c("bottomleft")) |>
#     addLegend(pal = pal4, values = values(rst$lgSWM250), layerId = names(rst$lgSWM250), position = c("bottomleft")) |>
#     addLegend(pal = pal5, values = values(rst$Siktdjup250), layerId = names(rst$Siktdjup250), position = c("bottomleft")) |>
#     addLegend(pal = pal6, values = values(rst$sal16_19), layerId = names(rst$sal16_19), position = c("bottomleft")) |>
#     addLegend(pal = pal7, values = values(rst$Storspigg_uppväxt_ostkust_2021), layerId = names(rst$Storspigg_uppväxt_ostkust_2021), position = c("bottomleft")) |>
#     addLegend(pal = pal8, values = values(rst$temp16_19), layerId = names(rst$temp16_19), position = c("bottomleft")) |>
#     addFullscreenControl() |> 
#     htmlwidgets::onRender("
#     function(el, x) {
#       var initialLegend = 'Prediktion' // Set the initial legend to be displayed by layerId
#       var myMap = this;
#       for (var legend in myMap.controls._controlsById) {
#         var el = myMap.controls.get(legend.toString())._container;
#         if(legend.toString() === initialLegend) {
#           el.style.display = 'block';
#         } else {
#           el.style.display = 'none';
#         };
#       };
#     myMap.on('baselayerchange',
#       function (layer) {
#         for (var legend in myMap.controls._controlsById) {
#           var el = myMap.controls.get(legend.toString())._container;
#           if(legend.toString() === layer.name) {
#             el.style.display = 'block';
#           } else {
#             el.style.display = 'none';
#           };
#         };
#       });
#     }") # JS för att byta legend när man byter lager
#   
# }


