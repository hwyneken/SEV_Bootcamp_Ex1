require(targets)
source("Create_Theme_Object.R")
source("Sepien_Distribution.R")
source("Sepien_Distribution_Sim.R")
source("Make_Sepien_Dist_Plot.R")
source("Decision_Points_Sim.R")

tar_option_set(packages = c("ggplot2","ggthemes","scales","gt"))

list(
  tar_target(
    ex1_plot_theme,
    Create_Theme_Object()
  ),
  tar_target(
    sepien_sim_results,
    Sepien_Distribution_Sim()
  ),
  tar_target(
    sepien_dist_plot,
    Make_Sepien_Dist_Plot(sepien_sim_results)
  ),
  tar_target(
    decision_point_sim,
    Decision_Points_Sim(sepien_sim_results)
  )
)