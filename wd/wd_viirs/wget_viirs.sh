#!/bin/bash
cd "$(dirname "$0")"
#pwd
wget -e robots=off -m -np -R .html,.tmp -nH --cut-dirs=4\
 "https://nrt3.modaps.eosdis.nasa.gov/archive/allData/1/NISE/Recent" --header\
 "Authorization: Bearer F12F327E-C0A9-11EA-8E59-74090E8F8B39" -P /home/brandon/Desktop/clutter/download_test