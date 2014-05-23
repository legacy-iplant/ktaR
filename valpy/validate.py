# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com


"""Dependencies"""
from commandline import *
from data import *
from fileimport import *
from checkhidden import *


"""Main function and execution"""
def main():
	initializeGraphics()
	folder, analysis, truth, snp, score, beta, filename, threshold, seper, kttype, kttypeseper = checkArgs()
	appOutputList = getList(folder)
	for each in appOutputList:
		if isHidden(each):
			appOutputList.remove(each)
	print appOutputList
	

if __name__ == "__main__":
	main()