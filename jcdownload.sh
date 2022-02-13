#!/bin/bash

echo Title Key or folder of resume: 

read titleKey

java -jar jcdownload.jar PUT_HERE_COMMON_KEY "$titleKey"


read -p "Press [Enter] key to exit..."