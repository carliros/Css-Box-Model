OUT = ./out/
AG  = ./src/ag/
SRC = ./src/

all: 
	ghc -i$(SRC) --make $(SRC)MainGui.hs -outputdir $(OUT) -o box
uuagc:  
	uuagc --module --catas --semfuns --signature --data $(AG)NTree.ag -P$(AG)
	uuagc --module --catas --semfuns --signature --data $(AG)FSTree.ag -P$(AG)
	mv $(AG)*.hs $(SRC)

