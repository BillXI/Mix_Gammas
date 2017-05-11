#!/bin/bash


for filename in /sh/; do
    echo filename
    qsub filename
done