all: #configure build
	ghc -O2 -o ../bin/MK7702Spindle.exe --make MK7702Spindle
#	ghc -O2 -o ../bin/CourseWorkSpindle.exe --make CourseWorkSpindle

build:
	runghc ../Setup.lhs build

configure: ../.setup-config

../.setup-config: ../SpindleCAD.cabal
	cd ../ && runghc Setup.lhs configure && cd src
