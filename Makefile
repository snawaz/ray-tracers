
all:
	rm -f image.ppm
	cabal v2-build
	./ray-tracers >> image.ppm
	open image.ppm

build:
	cabal v2-build

