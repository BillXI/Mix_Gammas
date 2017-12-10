# Mix_Gammas

## How to use the code:

1. Need to geneare the .sh files: run the "GenerateSH.R" and change the parameters inside.
2. Make sure change the save location in the files in Code/
3. Run the "sbatch simulation.sh"

## Progress:
### 2017, Dec 10
* 3 simulations were wrong: Condition 9 settings (9_100.Rd, 9_250.Rd, and 9_500.Rd) 
* Re-done the simulations for Condition 9 and return results:
   - Correct parameters: k=3, alpha=(1, 20, 50), beta = c(2, 4, 3), and lambda = c(0.2, 0.3, 0.5)
   - Results: https://github.com/billchenxi/Mix_Gammas/tree/master/Result/2017_12_8


### 2017, Nov 24
* adding k = k to the gammamixEM.new function.
* Re-install the mixtools to the local library. 
* Re-running the simulation now. (4:25 pm).
* Changed the simulation.sh file. Interestingly the old one not working, probabue due to the sh update.

### 2017 Summer: 
* Finished simulation (see Results/2017_5_11/)
