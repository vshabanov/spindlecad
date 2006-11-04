all: #configure build
	ghc -O2 -o ../bin/CourseWorkSpindle.exe --make CourseWorkSpindle

build:
	runghc ../Setup.lhs build

configure: ../.setup-config

../.setup-config: ../SpindleCAD.cabal
	cd ../ && runghc Setup.lhs configure && cd src
