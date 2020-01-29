#!/bin/bash

cd ./rankly-core
pulp --watch --then "./postbuild.sh" build
cd ../