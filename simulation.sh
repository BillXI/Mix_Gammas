#!/bin/bash


for filename in sh/*; do
<<<<<<< HEAD
    qsub filename
=======
    qsub $filename
>>>>>>> 715fd9adcdc74003b0ec5b0b8817bb8b34e113dc
done
