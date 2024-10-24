# Define the main tex file
MAIN = main

# Find all tex files in the current directory
TEX_FILES := $(wildcard *.tex)

# Define the output PDF file
PDF = $(MAIN).pdf

# Default target
all: $(PDF)

# Rule to make the PDF
$(PDF): $(TEX_FILES) $(MAIN).bbl
	pdflatex $(MAIN)
	pdflatex $(MAIN)

# Rule to make the bibliography if the aux file changes
$(MAIN).bbl: $(MAIN).aux
	bibtex $(MAIN)

# Rule to make the aux file
$(MAIN).aux: $(TEX_FILES)
	pdflatex $(MAIN)

# Phony target for cleaning
.PHONY: clean
clean:
	rm -f *.aux *.log *.bbl *.blg *.toc *.out

# Phony target for forcing a full rebuild
.PHONY: force
force: clean all