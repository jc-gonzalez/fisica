#!/bin/bash
pdftex -ini -jobname="texis" "&pdflatex" mylatexformat.ltx texis.tex '\dump'
pdftex -ini -jobname="pdftexis" "&pdflatex" mylatexformat.ltx texis.tex '\dump'
