#!/bin/bash

if [[ -z $1 ]]
then
	echo Title Key or folder of resume: 
	read titleKey
else
   titleKey=$1
fi

java -jar jcdownload.jar d7b00402659ba2abd2cb0db27fa2b656 "$titleKey"


read -p "Press [Enter] key to exit..."