#!/usr/bin/env bash

 ln -s src/image/ .
elm-live src/Main.elm -- --output=hampus-snake.js --debug
