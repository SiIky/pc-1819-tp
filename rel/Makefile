GV := \
    cl_auth.gv          \
    cl_ingame.gv        \
    cl_waiting.gv       \
    supervision_tree.gv \

MD := rel.md
DEPS := $(GV) $(MD) Makefile

ASSETS := $(GV:.gv=.png)
REL := $(MD:.md=.pdf)

all: $(REL) $(ASSETS)

%.pdf: %.md $(ASSETS)
	pandoc -f markdown -t latex $< -o $@

%.png: %.gv
	dot -Gdpi=1000 -Tpng $< -o $@

clean:
	$(RM) $(ASSETS) $(REL)

list_deps:
	@ls -1 $(DEPS)

.PHONY: all clean list_deps
