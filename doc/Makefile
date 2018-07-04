TARGET = main

MAKE = make

LATEXMK = latexmk
LATEXMK_OPTS = -xelatex

$(TARGET).pdf: $(TARGET).tex
	$(LATEXMK) $(LATEXMK_OPTS) $<

continue: $(TARGET).tex
	# $(MAKE) clean
	-$(MAKE) LATEXMK_OPTS="$(LATEXMK_OPTS) -pvc"

clean:
	$(LATEXMK) -C
