FILE=google-chrome-unstable_current_i386.deb
URL=http://dl.google.com/linux/direct/
DIR=/tmp/chrome
PWD=`pwd`
PDF=./opt/google/chrome/libpdf.so
SWF=./opt/google/chrome/libgcflashplayer.so

rm -rf $DIR
mkdir $DIR
cd $DIR

wget ${URL}${FILE}

ar x $FILE data.tar.lzma
# We can also extract and copy the flash player, but I don't find it useful.
tar xf data.tar.lzma $PDF
sudo mv -v $PDF /usr/lib/chromium/

cd $PWD
