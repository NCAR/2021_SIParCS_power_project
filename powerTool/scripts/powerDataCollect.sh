#!/bin/bash
while true; do sudo ipmitool dcmi power reading >> ${1}_$(hostname); sleep 1; done 
