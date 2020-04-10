#!/bin/bash

cd ./rankly-core
npm i
bower i
pulp --watch --then "./postbuild.sh" --else "notify-send -i face-angry \"Build Error!\"" build
cd ../
