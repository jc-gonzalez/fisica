#!/bin/bash
#docker run -it -v $(pwd)/io:/c520.io -v /Volumes/MediaBkp/data/c520/g/1000:/c520.data c520:0.1
docker run -it -v $(pwd)/io:/c520.io -v $(pwd)/data:/c520.data c520:0.1

