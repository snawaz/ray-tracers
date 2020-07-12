
build:
	cabal clean
	# cabal v2-build -O2 --enable-profiling  --enable-executable-profiling
	# cabal v2-build -O2 --verbose=3
	cabal v2-build -O2

render:
	rm -f image.ppm
	cabal v2-build
	time ./ray-tracers >> image.ppm
	open image.ppm

format:
	stylish-haskell -i src/*.hs *.hs
