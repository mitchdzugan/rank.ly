#!/bin/bash

cd ../rankly-web
parcel build index.html
touch ../rankly-server/server.js
cd ../rankly-core