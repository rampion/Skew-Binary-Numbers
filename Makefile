SRCS = $(wildcard *.hs)
TGTS = $(addsuffix .o,$(basename $(SRCS)))

all: $(TGTS)

%.o : %.hs
	ghc -c $< -o $@
