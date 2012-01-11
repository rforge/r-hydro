compute_fluxes <- function(deltim,smodl,mppt,mpet,mparam,dparam,state) {
   # Compute Fluxes
   # Author: Claudia Vitolo
   # Date: 30-11-2011
   #
   # Args:
   #   deltim:                        time step
   #   smodl:                         list of model components
   #   mppt:                          rain+snow melt at time "t"
   #   mpet:                          potential evapotranspiration at time "t"
   #   mparam:                        model parameters
   #   dparam:                        derived model parameters
   #   state:                         model states at time "t" 
   #
   # Returns:
   #   List of fluxes at time "t"

   tens_1a <- state[1]
   tens_1b <- state[2]
   tens_1  <- state[3]
   free_1  <- state[4]
   watr_1  <- state[5]
   tens_2  <- state[6]
   free_2a <- state[7]
   free_2b <- state[8]
   watr_2  <- state[9]

   # set all fluxes to zero at the start of time step
   eff_ppt     <- 0
   satarea     <- 0
   qrunoff     <- 0   
   evap_1a     <- 0   
   evap_1b     <- 0  
   evap_1      <- 0   
   evap_2      <- 0   
   rchr2excs   <- 0
   tens2free_1 <- 0
   tens2free_2 <- 0
   qintf_1     <- 0
   qperc_12    <- 0   
   qbase_2     <- 0   
   qbase_2a    <- 0   
   qbase_2b    <- 0   
   oflow_1     <- 0
   oflow_2     <- 0

   # compute effective rainfall
   eff_ppt   <- fluxrain(smodl$rferr,mppt,mparam$rferr_add,mparam$rferr_mlt)        # rainfall

   # compute excess of saturation
   temp      <- fluxqsatexcess(smodl$qsurf,eff_ppt,mparam,dparam,tens_1, watr_1, watr_2)
   satarea   <- temp[1]   # saturated area
   qrunoff   <- temp[2]   # surface runoff

   # compute evaporation
   temp      <- fluxevap(smodl$arch1,smodl$arch2,smodl$esoil,mpet,mparam,dparam,tens_1a, tens_1b, tens_1, tens_2)
   evap_1a   <- temp[1]
   evap_1b   <- temp[2]
   evap_1    <- temp[3]
   evap_2    <- temp[4]

   # compute interflow from free water in the upper layer
   if(smodl$qintf==72) qintf_1 <- mparam$iflwrte * (free_1/dparam$maxfree_1)
   if(smodl$qintf==71) qintf_1 <- 0

   # compute percolation from the upper to lower soil layers
   qperc_12 <- fluxperc(smodl$qperc,mparam,dparam,free_1, watr_1, watr_2)

   # compute baseflow
   temp <- fluxbaseflow(smodl$arch2,mparam,dparam$qbsat,free_2a, free_2b, watr_2)
   qbase_2a <- temp[1]
   qbase_2b <- temp[2]
   qbase_2  <- temp[3]

   # compute overflow (miscellaneous fluxes)
   temp <- fluxqmiscell(deltim,smodl$arch1,smodl$arch2,state,mparam,dparam,eff_ppt,qrunoff,qperc_12)
   rchr2excs   <- temp[1]
   tens2free_1 <- temp[2]
   tens2free_2 <- temp[3]
   oflow_1     <- temp[4]
   oflow_2     <- temp[5]

   flux1 <- list("eff_ppt"     = eff_ppt,
                 "satarea"     = satarea,
                 "qrunoff"     = qrunoff,
                 "evap_1a"     = evap_1a,
                 "evap_1b"     = evap_1b,
                 "evap_1"      = evap_1,
                 "evap_2"      = evap_2,
                 "rchr2excs"   = rchr2excs,
                 "tens2free_1" = tens2free_1,
                 "tens2free_2" = tens2free_2,
                 "qintf_1"     = qintf_1,
                 "qperc_12"    = qperc_12,
                 "qbase_2"     = qbase_2,
                 "qbase_2a"    = qbase_2a,
                 "qbase_2b"    = qbase_2b,
                 "oflow_1"     = oflow_1,
                 "oflow_2"     = oflow_2)

   return(flux1)

}
