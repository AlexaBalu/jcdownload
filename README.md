# jcdownload

jcdownload is a Java tool, compatible with Java 1.8 and higher to download Wii U content from N's CDN. It supports games, dlc, updates, just any content.
It requires common key which should be put into bat (on Windows) or sh (on Mac and Linux) startup scripts.

### Usage

To download content edit bat (on Windows) or sh (on Mac and Linux) and full fill appropriate place with common key found over Internet.
Then run desired script from console (on Mac or Linux) or double click that (works on Windows).

It will ask for title key (not title id) of the content to be downloaded. Once you copy/paste title key and hit ENTER, it starts downloading content.
After all you can enter to newly created subfolder of the content in downloads folder. Subfolder is ready to be decrypted with use of jcdecrypt2.

NOTE: Currently jcdownload doesn't create title.tik file. It is needed by jcdecrypt2 program. You can get proper title.tik from the place where you found title key.

