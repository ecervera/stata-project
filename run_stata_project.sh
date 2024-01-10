#!/usr/bin/env bash

chmod a+w 4-results

docker run -it --rm \
  -v $(pwd)/stata.lic:/usr/local/stata/stata.lic \
  -v $(pwd):/project stata-project
