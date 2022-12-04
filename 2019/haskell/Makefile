.PHONY: top all build clean format

DAYS := 1 2 3 4 5 6 7 8 9 10 11

ANSWERS := $(foreach day,$(DAYS),data/Day$(day)Problem1.answer data/Day$(day)Problem2.answer)

top: build
	$(MAKE) -j all

all: build $(ANSWERS)

build:
	stack build

clean:
	stack clean
	rm -f data/*.answer

format:
	stylish-haskell -i app/* && brittany --indent 4 --write-mode inplace app/*
	stylish-haskell -i src/* && brittany --indent 4 --write-mode inplace src/*

define GEN_RULE
data/Day$(day)Problem1.answer: src/*.hs app/Main.hs app/Day$(day).hs
	stack exec AoC2019 -- --day $(day) --problem 1 > $$@

data/Day$(day)Problem2.answer: src/*.hs app/Main.hs app/Day$(day).hs
	stack exec AoC2019 -- --day $(day) --problem 2 > $$@
endef

$(foreach day,$(DAYS),$(eval $(GEN_RULE)))
