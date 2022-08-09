#!/bin/bash
hostid=$(hostname)
while true; do sudo ipmitool dcmi power reading >>$1$hostid; sleep 1; done 
