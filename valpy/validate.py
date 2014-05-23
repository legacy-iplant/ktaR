# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com

"""Dependencies"""
import math, random, os, numpy, csv
from commandline import *

"""Main function and execution"""
def main():
	initializeGraphics()
	checkArgs()
	if analysis == "GWAS":
		GWAS()


if __name__ == "__main__":
	main()