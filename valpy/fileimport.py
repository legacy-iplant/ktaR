"""
Functions to import both class and results folder files
"""


import os, data


def getList(folder):
	return os.listdir(folder)


def loadFile(folder, thisFile, seper):
	return data.Data(folder + "/" + thisFile, seper)