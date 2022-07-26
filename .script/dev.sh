#!/bin/bash

SERVER_NAME=health-exe

if [ -z "$SERVER_NAME" ]; then
  echo "SERVER_NAME is empty. Please set server name first."
  exit 1
fi

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

stack build --file-watch --exec "${SCRIPT_DIR}/restart-server.sh ${SERVER_NAME}"
