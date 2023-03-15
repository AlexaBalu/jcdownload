# jcdownload

jcdownload is a tool for downloading and decrypting Wii U content from N's CDN. It supports games, dlc, updates, just any content. It requires common key which should be put into bat (on Windows) or sh (on Mac and Linux) startup scripts. It supports resumes, so you can break downloading any time you want.

### Usage

Make sure you have installed Java 1.8 or above on your environment.

To download content edit jcdownload.bat (on Windows) or jcdownload.sh (on Mac and Linux) and fill appropriate place with common key found over Internet.
Then run desired script from console (on Mac or Linux) or double click that (works on Windows).

It will ask for title key (not title id) of the content to be downloaded. Once you copy/paste title key and hit ENTER, it starts downloading and verifying content. After all you can visit newly created folder where you will find encrypted content ready to be decrypted.

To decrypt a content repeat above step with jcdecrypt.bat or jcdecrypt.sh