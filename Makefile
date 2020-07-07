
build:
	cabal v2-build -O2

test:
	cabal v2-build -O2
	 ./ray-tracers

#render:
	sh render.sh 100 100 50
