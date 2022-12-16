#!/bin/bash

# Please replace PUT_HERE_COMMON_KEY with key found over the Internet (use phrases like Wii U Common Key)
commonKey=PUT_HERE_COMMON_KEY

if [ $commonKey == "PUT_HERE_COMMON_KEY" ]
then
	echo "You have not set common key. Open this script and edit one value."
	exit 1
fi

if [ -z $1 ]
then
	echo Folder for decryption:
	read titleKey
else
   titleKey=$1
fi

java -cp jcdownload.jar info.cemu.download.Decrypt $commonKey "$titleKey"


read -p "Press [Enter] key to exit..."
