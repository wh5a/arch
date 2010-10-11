# Arch has upgraded to python3, so we need to grab python2
cd /usr/bin
sudo sh -c "mv python python.new; cp -P python2 python"
cd -
makepkg -i
sudo mv /usr/bin/python.new /usr/bin/python
