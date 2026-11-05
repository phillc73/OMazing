.PHONY: all build run clean test lint

all: build

build:
	OMAZING_GRAPHICS=true dune build main.exe

run: build
	./_build/default/main.exe

test:
	dune test

lint:
	zanuda -dir .

clean:
	dune clean
	rm -rf _build
