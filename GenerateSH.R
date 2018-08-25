b = "#!/bin/bash
#SBATCH --nodes=1 --ntasks-per-node=1 --mem=2G 
#SBATCH -t 6-00:00
#SBATCH --mail-user=xch234@uky.edu
#SBATCH --mail-type=ALL

# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code/2018_8_Corrected_Code"

a = list.files("./code/2018_8_Corrected_Code/")

# doneJob = gsub(".Rd", "", list.files("./Result/2018_8_16/"))
# previousJob = gsub(".R|Condition_", "", a)
# newJob = a[-which(previousJob %in% doneJob)]
# a = newJob
a = paste0("Rscript ", a)

c = paste0(b, "\n", a)

l = length(c)
for(i in 1:l){
        fname = paste0("sim",i,".sh")
        write(c[i], file = paste0("./sh/",fname))
}
