# Simulations for the ERGMito paper

Simulations need to be run in a large cluster. The simulations here took about
200 * 4 * 2 = 1,600 computing hours, i.e. about a month on a two core computer.

The simulations require using: slurmR, ergmito, and of course, ergm.

To execute a simulation with slurmR, you can simply type

```r
slurmR::sourceSlurm("simulations/02-various-sizes-4-5.R")
```

Which will create a slurm job and run the simulation file.

