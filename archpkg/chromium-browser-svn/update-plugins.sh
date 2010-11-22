FILE=google-chrome-unstable_current_i386.deb
URL=http://dl.google.com/linux/direct/
DIR=/tmp/chrome
PWD=`pwd`
PDF=./opt/google/chrome/libpdf.so
SWF=./opt/google/chrome/libgcflashplayer.so
OSSWF=/usr/lib/mozilla/plugins/libflashplayer.so

rm -rf $DIR
mkdir $DIR
cd $DIR

wget ${URL}${FILE}

ar x $FILE data.tar.lzma
tar xf data.tar.lzma $PDF
sudo mv -v $PDF /usr/lib/chromium/
# Instead of extracting the flash player from Chrome, we trust that Arch provides the latest version.
# The point of creating the link is that Chrome will sandbox flash if it's "built-in"
#sudo ln -s $OSSWF /usr/lib/chromium/libgcflashplayer.so

cd $PWD
