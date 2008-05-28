all: build

NAME=TestFEA

configure:
	runhaskell Setup.lhs configure --prefix=$(HOME) --user

fast-build:
	runhaskell Setup.lhs build

build: configure fast-build

fast-run:
	./dist/build/$(NAME)/$(NAME)

run: build fast-run

fast-docs:
	runhaskell Setup.lhs haddock --executables

docs: configure fast-docs

fast-view-docs:
	firefox dist/doc/html/$(NAME)/$(NAME)/index.html

view-docs: docs fast-view-docs

fast-install:
	runhaskell Setup.lhs install

install: configure fast-build fast-docs fast-install

clean:
	runhaskell Setup.lhs clean
