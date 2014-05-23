"""
Performance measures for testing Validate
"""


import numpy


def RMSE(ktFile, appOutput, kttype, snp, beta):
	if kttype == "OTE":
		