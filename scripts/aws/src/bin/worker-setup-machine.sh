#!/bin/bash
# This script is going to be executed inside an amazon virtual machine and will
# clone and build a list of required git repositories.

set -e # Any subsequent(*) commands which fail will cause the shell script
       # to exit immediately

# add ESL as a repository
sudo wget -c -O- http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -
sudo echo "deb http://packages.erlang-solutions.com/ubuntu xenial contrib" | sudo tee -a /etc/apt/sources.list.d/erlang_solutions.list > /dev/null

sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes install build-essential autoconf git r-base erlang

# needed to later build the PNG image
sudo chown ubuntu /usr/local/lib/R/site-library/

 # env
 BIN_DIR=`pwd`
 cd $BIN_DIR

 #############################  antidote @############################
 ANTIDOTE_DIR=$HOME/antidote

if [ -d "$ANTIDOTE_DIR" ]; then
    echo "[SCRIPT] Antidote directory has been found in this node. Pulling latest changes..."
    # Control will enter here if $DIRECTORY exists.
    cd $ANTIDOTE_DIR

    git checkout build-local-cluster-aws
    git pull
else
    echo "[SCRIPT] Antidote repository not found. Cloning repository..."
    git clone https://github.com/goncalotomas/antidote

    cd $ANTIDOTE_DIR
    git checkout build-local-cluster-aws

fi
PUBLIC_NODE_IP=`curl checkip.amazonaws.com`
echo "{public_ip, {$PUBLIC_NODE_IP}}" > ./config/node-address.config
sed -ie 's/\./,/g' ./config/node-address.config
echo "." >> ./config/node-address.config

make rel
cd $BIN_DIR

 ############################## FMKe #################################
FMKE_DIR=$HOME/FMKe

 if [ -d "$FMKE_DIR" ]; then
     echo "[SCRIPT] FMKe directory has been found in this node. Pulling latest changes..."

     cd $FMKE_DIR
     git checkout perf-and-errors
     git pull
     make rel
 else
     echo "[SCRIPT] FMKe repository not found. Cloning repository..."

     git clone https://github.com/goncalotomas/FMKe
     cd $FMKE_DIR
     git checkout perf-and-errors
     make rel
 fi

 cd $BIN_DIR

 ########################## basho_bensh ##############################
 BASHO_BENSH_DIR=$HOME/basho_bench

 if [ -d "$BASHO_BENSH_DIR" ]; then
     echo "[SCRIPT] FMKe directory has been found in this node. Pulling latest changes..."

     cd $BASHO_BENSH_DIR
     git checkout antidote_pb_fmk_aws
     git pull
     make all
 else
     echo "[SCRIPT] FMKe repository not found. Cloning repository..."
     git clone https://github.com/SyncFree/basho_bench
     cd $BASHO_BENSH_DIR
     git checkout antidote_pb_fmk_aws
     make all
 fi
