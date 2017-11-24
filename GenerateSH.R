b = "#!/bin/bash
#SBATCH --nodes=1 --ntasks-per-node=1 --mem=2G 
#SBATCH -t 2-00:00
#SBATCH --mail-user=user@uky.edu
#SBATCH --mail-type=ALL

# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code/2017_11_24"

a = list.files("./Code/2017_11_24/")

a = paste0("Rscript ", a)

c = paste0(b, "\n", a)

l = length(c)
for(i in 1:l){
        fname = paste0("sim",i,".sh")
        write(c[i], file = paste0("./sh/",fname))
}
