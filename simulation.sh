#!/bin/bash


for filename in sh/*; do
    qsub filename
done
