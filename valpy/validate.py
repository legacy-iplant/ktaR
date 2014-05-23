# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com


"""Dependencies"""
from commandline import *
from data import *


"""Main function and execution"""
def main():
	initializeGraphics()
	checkArgs()
	print Data("/users/dustin/ktar/outputplink/plinkstd1.qassoc", "whitespace").data[0]


if __name__ == "__main__":
	main()