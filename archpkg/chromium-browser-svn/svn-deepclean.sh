find src/ -type d -name .svn -print0|xargs -0 -i svn cleanup "{}/.."
