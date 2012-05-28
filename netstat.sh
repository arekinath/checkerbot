#!/bin/sh
netstat -l -p --numeric-ports -t 2>/dev/null | awk '{print $4}' | awk -F: '{print $2}'
