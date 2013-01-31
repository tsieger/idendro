#!/bin/sh

NAME=idendro

pdflatex ${NAME} && bibtex ${NAME} && pdflatex ${NAME} && pdflatex ${NAME}
