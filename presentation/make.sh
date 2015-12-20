#!/bin/bash


pandoc -t beamer --include-in-header=header.txt presentation.md -o presentation.pdf
