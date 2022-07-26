#!/bin/bash

SERVER_NAME=$1

killall $SERVER_NAME &>/dev/null

stack exec $SERVER_NAME &
