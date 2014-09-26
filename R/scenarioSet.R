### Choose a scenario with `id`, to filter
### variables of the `vals` dataset

scenarioSet <- function(id, vals){
  colsIn <- c(S01 = '.',
              S02 = '.',
              S03 = 'Pac|point|idw|Bo0|AzS|AlS',
              S04 = 'Pac|point|idw',
              S05 = 'Pac|point|idw|Bo0|AzS|AlS',
              S06 = 'Pac|point|idw',
              S07 = 'Pac|point|idw|Bo0|AzS|AlS',
              S08 = 'Pac|point|idw',
              S09 = 'Pac|point|idw|Bo0|AzS|AlS',
              S10 = 'Pac|point|idw',
              S11 = 'Pac|point|idw|Bo0|AzS|AlS',
              S12 = 'Pac|point|idw',
              S13 = 'Pac|point|idw|Bo0|AzS|AlS',
              S14 = 'Pac|point|idw|Bo0|AzS|AlS',
              S15 = 'Pac|temp.point|temp.idw|Bo0|AzS|AlS',
              S16 = 'Pac|point|idw|Bo0|AzS|AlS|kt',
              S17 = 'Pac|point|idw|kt')
  
  colsOut <- c(S01 = NA,
               S02 = 'AzS|AlS|Bo0|kt',
               S03 = NA,
               S04 = NA,
               S05 = 'cfl|cfm|cfh',
               S06 = 'cfl|cfm|cfh',
               S07 = 'cfl|cfm|cfh|u|v',
               S08 = 'cfl|cfm|cfh|cft|u|v',
               S09 = 'cfl|cfm|cfh|cft|u|v|dir|visibility|mslp',
               S10 = 'cfl|cfm|cfh|cft|u|v|dir|visibility|mslp',
               S11 = 'swflx|u|v|dir|visibility|mslp',
               S12 = 'swflx|u|v|dir|visibility|mslp',
               S13 = 'swflx|cfl|cfm|cfh|cft|u|v|dir|visibility|mslp',
               S14 = 'swflx|cfl|cfm|cfh|u|v|dir|visibility|mslp',
               S15 = NA,
               S16 = 'cfl|cfm|cfh|cft|u|v|dir|visibility|mslp',
               S17 = 'cfl|cfm|cfh|cft|u|v|dir|visibility|mslp')
  
  nms <- names(vals)
  colsSet <- setdiff(grep(colsIn[id], nms), grep(colsOut[id], nms))
  vals[, colsSet]
  
}