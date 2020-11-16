.PHONY : all clean

GRAMA_FOLDER = grama
SRC_FOLDER = src
PARSER_FILES = $(GRAMA_FOLDER)/ErrM.hs $(GRAMA_FOLDER)/LexInstant.hs $(GRAMA_FOLDER)/ParInstant.hs $(GRAMA_FOLDER)/PrintInstant.hs

all : insc_jvm insc_llvm

%.hs : %.y
	happy -gca $<

%.hs : %.x
	alex -g $<

generate_grama:
	bnfc -m $(SRC_FOLDER)/Instant.cf -o $(GRAMA_FOLDER)

insc_jvm : $(PARSER_FILES)
	ghc -i$(SRC_FOLDER):$(GRAMA_FOLDER) --make $(SRC_FOLDER)/parse.hs $(SRC_FOLDER)/insc_jvm.hs $(SRC_FOLDER)/jvm.hs $(PARSER_FILES) -o $@

insc_llvm : $(PARSER_FILES)
	ghc -i$(SRC_FOLDER):$(GRAMA_FOLDER) --make $(SRC_FOLDER)/parse.hs $(SRC_FOLDER)/insc_llvm.hs $(SRC_FOLDER)/llvm.hs $(PARSER_FILES) -o $@

clean :
	-rm -f $(GRAMA_FOLDER)/*.hi $(GRAMA_FOLDER)/*.o $(GRAMA_FOLDER)/*.log $(GRAMA_FOLDER)/*.aux $(GRAMA_FOLDER)/*.dvi
	-rm -f $(SRC_FOLDER)/*.hi $(SRC_FOLDER)/*.o $(SRC_FOLDER)/*.log $(SRC_FOLDER)/*.aux $(SRC_FOLDER)/*.dvi
	-rm -f insc_jvm insc_llvm
