#!/bin/bash

cd ../rankly-web
parcel build index.html
touch ../rankly-server/server.js
notify-send -i face-smile-big "Build Complete!"
cd ../rankly-core
