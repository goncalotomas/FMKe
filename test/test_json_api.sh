#!/bin/bash
# Some example calls for testing the JSON API

set -e
echo "Writing:"
curl -d '{"id":"10004", "name":"Peter Z", "address":"Hello W"}' http://localhost:9090/patients
echo
echo "Reading:" 
curl http://localhost:9090/patients/10004
echo