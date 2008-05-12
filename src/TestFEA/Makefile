all: build

configure:
	runhaskell Setup.lhs configure --prefix=$(HOME) --user

fast-build:
	runhaskell Setup.lhs build

build: configure fast-build

fast-docs:
	runhaskell Setup.lhs haddock --executables

docs: configure fast-docs

install: build docs
	runhaskell Setup.lhs install

clean:
	runhaskell Setup.lhs clean
