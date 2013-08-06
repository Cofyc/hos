
all:
	runghc Setup.hs configure --user --prefix=/usr/local
	runghc Setup.hs build

.PHONY: all install test debug dist-bzip2 clean todo

clean:
	runghc Setup.hs clean
	find . -type f -name "*.o" | xargs rm -rf
	find . -type f -name "*.hi" | xargs rm -rf
	rm -rf hos

install: all
	sudo runghc Setup.hs install

debug: all
	sudo ./dist/build/hos/hos -d

dist-bzip2: all
	git archive --format=tar --prefix=hos-0.1/ HEAD | bzip2 > hos-0.1.tar.bz2
