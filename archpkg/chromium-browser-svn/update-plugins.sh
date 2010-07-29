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
tar xf data.tar.lzma $PDF $SWF
sudo mv -v $PDF $SWF /usr/lib/chromium/

cd $PWD
