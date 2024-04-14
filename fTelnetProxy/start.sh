#!/bin/bash

echo "Waiting 5 seconds for network to come online"
read -t 5 input;

while [ true ]; do
    # Start fTelnetProxy
    cd /opt/fTelnetProxy
    /usr/bin/mono /opt/fTelnetProxy/fTelnetProxy.exe

    # Log the exit code
    echo "[$(date +"%Y-%m-%d %T")] ExitCode: $?" >> exit_codes.log

    # Wait up to 5 seconds for a keypress to see if admin wants to abort reload
    echo "----- Press enter to prevent the server from restarting in 5 seconds -----";
    read -t 5 input;
    if [ $? == 0 ]; then
        break;
    else
        echo "------------------- SERVER RESTARTS -------------------";
    fi
done
