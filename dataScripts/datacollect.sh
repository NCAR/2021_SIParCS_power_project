#!/bin/bash
hostid=$(hostname)
while true; do sudo ipmitool dcmi power reading >>powerout$1$hostid; sleep 1; done 
