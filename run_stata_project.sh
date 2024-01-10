#!/usr/bin/env bash

docker run -it --rm \
  -v $(pwd)/stata.lic:/usr/local/stata/stata.lic \
  -v $(pwd):/project stata-project
