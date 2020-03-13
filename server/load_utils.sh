alias watchdir='while true; do find Makefile Emakefile include/ src/ -type f | entr -a -c -d sh -c "make; rsync -az -e ssh --delete . rbpi4:~/projects/pc-1819-tp/server/"; done'
alias erls='erl -pa ebin/'
