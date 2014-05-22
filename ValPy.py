# ValPy -- Validate.R in Python
# Author: Dustin Landers
# Contact: (770 289-8830 :: dustin.landers@gmail.com


"""Dependencies"""

import math
import random
import getopt
import sys
import numpy


"""Functions to be used later in the software"""
# Prints all possible command-line arguments to the screen; also ends the execution of the software
def usage():
	print "--verbose or -v for verbose mode\n"
	print "--analysis or -a to specify either 'GWAS' or 'prediction' (if blank, Validate assumes GWAS)\n"
	print "--folder or -F to input folder of box results (required)\n"
	print "--class or -C to specify the known-truth file for used simulation (required)\n"
	print "--snp or -S to specify a string for the name of the SNP column in results file (required)\n"
	print "--score or -P to specify a string for the name of the scoring column in results file (e.g., p-value; required)\n"
	#print "--effect or -e to specify a file for just effects aligning with the class file (only required for OTE1 kttype\n"
	print "--beta or -b to specify a string for the name of the estimated SNP effect column in results file\n"
	print "--severity or -V to specify a severity ratio to use in calculating the H-measure (recommended 1 or pi1/pi0)\n"
	print "--filename or -f to specify the desired filename for the Validate output file\n"
	print "--threshold ir -t to specify a desired threshold for classification performetrics where necessary\n"
	print "--seper or -s to specify either whitespace or comma\n"
	print "--kttype or -k to specify the type of known-truth file for --class (either OTE or FGS)\n"
	print "--kttypeseper or -r to specify delimination in known-truth file\n"
	print "--help or -h to see help menu\n"


# Checks for arguments at beginning of the execution of the main function
def checkArgs():
	try:
		opts, args = getopt.getopt(sys.argv[1:], shortopts="vha:F:C:S:P:e:b:V:f:t:s:k:r", longopts=["verbose", "help", 
			"analysis=", "folder=", "class=", "snp=", "score=", "effect=", "filename=", "threshold=", "seper=", "kttype=", "kttypeseper="])

	except getopt.GetoptError as err:
		print(err)
		usage()
		sys.exit()

	# Setting needed variables global
	global verbose, analysis, folder, truth, snp, score, filename, theshold, seper, kttype, kttypeseper

	# Specifiying initial values of needed variables; unneeded specification when desiring defaults
	verbose = False
	analysis = "GWAS"
	filename = "Validate"
	threshold = 0.05
	seper = "whitespace"
	kttype = "OTE"
	kttypeseper = "whitespace"

	for o in opts:
		if o[0] in ("--verbose", "-V"):
			verbose = True
			print ("Verbose mode")
	for o in opts:
		if o[0] in ("--help", "-h"):
			usage()
			sys.exit()
	for o in opts:
		if o[0] in ("--analysis", "-a"):
			analysis = str(o[1])
	for o in opts:
		if o[0] in ("--folder", "-F"):
			folder = str(o[1])
			print folder
		else:
			print "Need to specify folder of results files for validation.\n"
			usage()
			sys.exit()
	for o in opts:
		if o[0] in ("--class", "-c"):
			truth = str(o[1])
	for o in opts:
		if o[0] in ("--snp", "-S"):
			snp = str(o[1])
		else:
			print "Need to specify the name of the SNP column\n"
			usage()
			sys.exit()
	for o in opts:
		if o[0] in ("--score", "-P"):
			score = str(o[1])
		else:
			print "Need to specify the name of the scoring column (e.g., p-value column)\n"
			usage()
			sys.exit()
	for o in opts:
		if o[0] in ("--filename", "-f"):
			filename = str(o[1])
		if verbose:
			print "Filename specified as", filename
	for o in opts:
		if o[0] in ("--threshold", "-t"):
			threshold = float(o[1])
		if verbose:
			print "Theshold is set at", threshold
	for o in opts:
		if o[0] in ("--seper", "-s"):
			seper = str(o[1])
		if verbose:
			print "Delimination of results files is set as", seper
	for o in opts:
		if o[0] in ("--kttype", "-k"):
			kttype = str(o[1])
		if verbose:
			print "Known-truth data format is set as", kttype
	for o in opts:
		if o[1] in ("--kttypeseper", "-r"):
			kttypeseper = str(o[1])
		if verbose:
			print "Known-truth data format delimination is set as", kttypeseper



# Executes all necessary functions
def main():
	checkArgs()


if __name__ == "__main__":
	main()