
build:
	cabal v2-build -O2

test:
	cabal v2-build -O2
	 ./ray-tracers

render:
	rm -f image.ppm
	cabal v2-build -O2
	 ./ray-tracers >> image.ppm
	open image.ppm

