# Mix_Gammas
## Progress:
### 2018, Aug 24
* Received Dr. Young's "Corrected code", save under "./Code/2018_8_Corrected_Code" 
    - "There are a total of 9 conditions that need to be reran (Conditions 13, 14, 15, for each of the 3 different sample sizes).  I have attached the updated scripts to correct the errors that were in there. Please let me know if you are able to rerun these 9 conditions and if you have any questions."

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


## How to use the code:

1. Need to geneare the .sh files: run the "GenerateSH.R" and change the parameters inside.
2. Make sure change the save location in the files in Code/
3. Run the "sbatch simulation.sh"
4. To check the queue: squeue
