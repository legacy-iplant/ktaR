# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com


"""Dependencies"""
from commandline import *
from fileimport import *
from checkhidden import *


"""Main function and execution"""
def main():
	initializeGraphics()
	folder, analysis, truth, snp, score, beta, filename, threshold, seper, kttype, kttypeseper = checkArgs()
	appOutputList = checkList(getList(folder))
	ktFile = loadKT(truth, kttypeseper)
	
	if kttype is "OTE":
		acquiredData = loadFile(folder, appOutputList[0], seper)
		snpColumnNo = acquiredData.header.index(snp)
		snpColumn = list()
		for each in acquiredData.data.iteritems():
			snpColumn.append(each[1][snpColumnNo])
		

	for each in appOutputList:
		acquiredData = loadFile(folder, each, seper)
		snpColumnNo = acquiredData.header.index(snp)
		snpColumn = list()
		for each in acquiredData.data.iteritems():
			snpColumn.append(each[1][snpColumnNo])
		scoreColumnNo = acquiredData.header.index(score)
		scoreColumn = list()
		for each in acquiredData.data.iteritems():
			scoreColumn.append(float(each[1][scoreColumnNo]))
		if beta is not None:
			betaColumnNo = acquiredData.header.index(beta)
			betaColumn = list()
			for each in acquiredData.data.iteritems():
				betaColumn.append(float(each[1][betaColumnNo]))


if __name__ == "__main__":
	main()