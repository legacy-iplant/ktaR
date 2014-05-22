# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com

"""Dependencies"""

import math
import random
import getopt
import sys
import os
import numpy
import csv


"""Functions to be used later in the software"""
# Prints introduction graphics for every time the software is run
def initializeGraphics():
	print "###################################################################"
	print "###                                                            ####"
	print "###      Validate for Python!                                  ####"
	print "###      By Dustin A. Landers                                  ####"
	print "###      Contact: (770) 289-8830 -- dustin.landers@gmail.com   ####"
	print "###                                                            ####"
	print "###################################################################"


# Prints all possible command-line arguments to the screen; also ends the execution of the software
def usage():
	print "\n\n\n"
	print "Command-line usage help menu.\n"
	print "--verbose or -v for verbose mode"
	print "--analysis or -a to specify either 'GWAS' or 'prediction' (if blank, Validate assumes GWAS)"
	print "--folder or -F to input folder of box results (required)"
	print "--class or -C to specify the known-truth file for used simulation (required)"
	print "--snp or -S to specify a string for the name of the SNP column in results file (required)"
	print "--score or -P to specify a string for the name of the scoring column in results file (e.g., p-value; required)"
	print "--beta or -b to specify a string for the name of the estimated SNP effect column in results file"
	print "--severity or -V to specify a severity ratio to use in calculating the H-measure (recommended 1 or pi1/pi0)"
	print "--filename or -f to specify the desired filename for the Validate output file"
	print "--threshold ir -t to specify a desired threshold for classification performetrics where necessary"
	print "--seper or -s to specify either whitespace or comma"
	print "--kttype or -k to specify the type of known-truth file for --class (either OTE or FGS)"
	print "--kttypeseper or -r to specify delimination in known-truth file"
	print "--help or -h to see help menu\n\n"


# Checks for arguments at beginning of the execution of the main function
def checkArgs():
	try:
		opts, args = getopt.getopt(sys.argv[1:], shortopts="vha:F:C:S:P:b:V:f:t:s:k:r", longopts=["verbose", "help", 
			"analysis=", "folder=", "class=", "snp=", "score=", "beta=", "filename=", "threshold=", "seper=", "kttype=",
			"kttypeseper="])

	except getopt.GetoptError as err:
		print(err)
		usage()
		sys.exit()

	# Setting needed variables global
	global verbose, analysis, folder, truth, snp, score, filename, theshold, seper, kttype, kttypeseper

	# Specifiying initial values of needed variables; unneeded specification when desiring defaults
	verbose = False
	analysis = "GWAS"
	filename = "Results.txt"
	threshold = 0.05
	seper = "whitespace"
	kttype = "OTE"
	kttypeseper = "whitespace"

	# Looping through command-line arguments to replace and/or create initialized values
	for o in opts:
		if o[0] in ("--help", "-h"):
			usage()
			sys.exit()
	for o in opts:
		if o[0] in ("--verbose", "-V"):
			verbose = True
			print ("Verbose mode\n")
	for o in opts:
		if o[0] in ("--folder", "-F"):
			folder = str(o[1])
			if verbose:
				print "Folder of results files for validation is located in", folder
		if o[0] in ("--analysis", "-a"):
			analysis = str(o[1])
			if verbose:
				print "Analysis method being validated is specified as", analysis
		if o[0] in ("--class", "-C"):
			truth = str(o[1])
			if verbose:
				print "Truth file is", truth
		if o[0] in ("--snp", "-S"):
			snp = str(o[1])
			if verbose:
				print "SNP column name in results files is specified as", snp
		if o[0] in ("--score", "-P"):
			score = str(o[1])
			if verbose:
				print "Scoring column name (e.g., p-value column) in results files is specified as", score
		if o[0] in ("--beta", "-b"):
			beta = str(o[1])
			if verbose:
				print "Estimated SNP Weight column name (e.g., regression betas) in results files is specified as", beta
		if o[0] in ("--filename", "-f"):
			filename = str(o[1])
			if verbose:
				print "Filename specified as", filename
		if o[0] in ("--threshold", "-t"):
			threshold = float(o[1])
			if verbose:
				print "Theshold is set at", threshold
		if o[0] in ("--seper", "-s"):
			seper = str(o[1])
			if verbose:
				print "Delimination of results files is set as", seper
		if o[0] in ("--kttype", "-k"):
			kttype = str(o[1])
			if verbose:
				print "Known-truth data format is set as", kttype
		if o[1] in ("--kttypeseper", "-r"):
			kttypeseper = str(o[1])
			if verbose:
				print "Known-truth data format delimination is set as", kttypeseper

	# Check to see if needed variables are defined
	try:
		folder
	except NameError:
		print "ERROR: Folder of results files to be validated must be specificed."
		usage()
		sys.exit()
	try:
		truth
	except NameError:
		print "ERROR: Known-truth data file must be supplied in order for results to be validated."
		usage()
		sys.exit()
	try:
		snp
	except NameError:
		print "ERROR: Name of SNP column in results files must be specified."
		usage()
		sys.exit()
	try:
		score
	except NameError:
		print "ERROR: Name of scoring column must be specified in order to validate SNP classifications."
		usage()
		sys.exit()


# Loads in necessary files in as numpy arrays
def loadFiles():
	global data
	data = Data(folder + "/" + appOutputList[0])
	print data.data


# Runs GWAS application Validate analysis
def GWAS():
	global appOutputList
	appOutputList = os.listdir(folder)
	loadFiles()


"""End list of functions; main function and execution below"""
# Executes all necessary functions
def main():
	initializeGraphics()
	checkArgs()
	if analysis == "GWAS":
		GWAS()


# Executes main function; initializes software
if __name__ == "__main__":
	main()