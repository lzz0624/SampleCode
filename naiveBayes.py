#!/usr/bin/python

# Zhangzhang Liu (zl5mb)
import sys
import os
import numpy as np
from sklearn.naive_bayes import MultinomialNB
textDataSetsDirectoryFullPath = "F:\\datasets\\data_sets\\"
testFileDirectoryFullPath = "F:\\datasets\\data_sets\\"
###############################################################################
def transfer(fileDj, vocabulary):
    splited = fileDj.split()
    counted = [[x,splited.count(x)] for x in set(splited)]
    BOWDj = np.zeros((1,len(vocabulary)+1))
    for count in counted:
        word = count[0]
        num = count[1]
        if word in ['loving','loved','loves']:
            word = 'love'
        if word in vocabulary:
            wordnum = vocabulary.index(word) + 1       
            BOWDj[0][wordnum] = BOWDj[0][wordnum] + num
        else:  # add into unknown column
            BOWDj[0][0] = BOWDj[0][0] + num
    return BOWDj

###############################################################################
def loadData(textDataSetsDirectoryFullPath):
    # read in text files
    file1level = ['test_set\\','training_set\\']
    file2level = ['neg\\','pos\\']
    filelists = [textDataSetsDirectoryFullPath + x + y for x in file1level for y in file2level]
    filefullnames = []
    for filelist in filelists:
        newnames  = os.listdir(filelist)
        newnames2 = [filelist + z for z in newnames]
        filefullnames.append(newnames2)
    
    # from fileanems - ytrain, ytest
    ytest = np.concatenate((np.repeat(-1, len(filefullnames[0])),np.repeat( 1, len(filefullnames[1]))), axis = 0)
    ytrain = np.concatenate((np.repeat(-1, len(filefullnames[2])),np.repeat( 1, len(filefullnames[3]))), axis = 0)
    
    # define vocabulary first
    vocabulary = ['love','wonderful','best','great','superb','still','beautiful','bad','worst','stupid','waste','boring','?','!']
    # transform sentences (fileDj) to BOW - for Xtrain, Xtest
    Xtrain = []
    for filefullname in filefullnames[2:]:
        for fullname in filefullname:
            newfile = open(fullname, 'r')
            newcomm = newfile.read()
            newrow = transfer(newcomm, vocabulary)
            Xtrain = np.append(Xtrain, newrow[0], axis = 0)
    
    Xtest = []
    for filefullname in filefullnames[0:2]:
        for fullname in filefullname:
            newfile = open(fullname, 'r')
            newcomm = newfile.read()
            newrow = transfer(newcomm, vocabulary)
            Xtest = np.append(Xtest, newrow[0], axis = 0)
    
    # reshape the array
    Xtrain = Xtrain.reshape(len(ytrain),len(vocabulary)+1)
    Xtest = Xtest.reshape(len(ytest),len(vocabulary)+1)
    return Xtrain, Xtest, ytrain, ytest

###############################################################################
def naiveBayesMulFeature_train(Xtrain, ytrain):
    nvol = Xtrain.shape[1]
    xPos = Xtrain[ytrain == 1]
    xNeg = Xtrain[ytrain == -1]
    thetaPos = (sum(xPos) + 1)/ float(np.sum(xPos) + nvol)
    thetaNeg = (sum(xNeg) + 1)/ float(np.sum(xNeg) + nvol)
    return thetaPos, thetaNeg
###############################################################################

def naiveBayesMulFeature_test(Xtest, ytest,thetaPos, thetaNeg):
    yPredict = []
    pPos = np.sum(Xtest*np.log10(thetaPos), axis = 1)
    pNeg = np.sum(Xtest*np.log10(thetaNeg), axis = 1)
    pmap = pPos>pNeg
    yPredict = np.repeat(0,Xtest.shape[0])
    yPredict[pmap] = 1
    yPredict[-pmap] = -1
    # now calcuate the accuracy
    rightnum = sum(yPredict == ytest)
    Accuracy = rightnum / float(len(ytest))
    return yPredict, Accuracy

###############################################################################

# question 1.4
def naiveBayesMulFeature_sk_MNBC(Xtrain, ytrain, Xtest, ytest):
    clf = MultinomialNB()
    clf.fit(Xtrain, ytrain)
    yPredict = clf.predict(Xtest)
    # now calcuate the accuracy
    rightnum = sum(yPredict == ytest)
    Accuracy = rightnum / float(len(ytest))
    return Accuracy
###############################################################################

# question 1.5
def naiveBayesMulFeature_testDirectOne(path,thetaPos, thetaNeg):
    newfile = open(path, 'r')
    newcomm = newfile.read()
    splited = newcomm.split()
    # define vocabulary first
    vocabulary = ['love','wonderful','best','great','superb','still','beautiful','bad','worst','stupid','waste','boring','?','!']
    
    probPos = 0
    probNeg = 0
    for split in splited:
        if split in vocabulary:
            wordnum = vocabulary.index(split) + 1
            probPos = np.log10(thetaPos[wordnum])+probPos
            probNeg = np.log10(thetaNeg[wordnum])+probNeg
            
    if probPos > probNeg:
        yPredict = 1
    else:
        yPredict = -1
        
    return yPredict

def naiveBayesMulFeature_testDirect(textDataSetsDirectoryFullPath,thetaPos, thetaNeg):
    yPredict = []
    file1level = ['test_set\\']
    file2level = ['neg\\','pos\\']
    filelists = [textDataSetsDirectoryFullPath + x + y for x in file1level for y in file2level]
    filefullnames = []
    for filelist in filelists:
        newnames  = os.listdir(filelist)
        newnames2 = [filelist + z for z in newnames]
        filefullnames.append(newnames2)
    
    yPredict = []
    for filefullname in filefullnames[0]:
        yPredict1 = naiveBayesMulFeature_testDirectOne(filefullname,thetaPos, thetaNeg)
        yPredict.append(yPredict1)

    for filefullname in filefullnames[1]:
        yPredict1 = naiveBayesMulFeature_testDirectOne(filefullname,thetaPos, thetaNeg)
        yPredict.append(yPredict1)
    
    # Calculate the Accuracy again
    rightnum = sum(yPredict == ytest)
    Accuracy = rightnum / float(len(ytest))
    return yPredict, Accuracy
###############################################################################

def naiveBayesBernFeature_train(Xtrain, ytrain):
    nvol = Xtrain.shape[1]
    xPos = Xtrain[ytrain == 1]
    xNeg = Xtrain[ytrain == -1]
    thetaPosTrue = (sum(xPos>0) + 1)/ float(xPos.shape[0] + 2)
    thetaNegTrue = (sum(xNeg>0) + 1)/ float(xNeg.shape[0] + 2)
    return thetaPosTrue, thetaNegTrue

    
def naiveBayesBernFeature_test(Xtest, ytest, thetaPosTrue, thetaNegTrue):
    yPredict = []
    pPos = np.sum((Xtest>0)*np.log10(thetaPosTrue), axis = 1) + np.sum((Xtest==0)*np.log10(1-thetaPosTrue), axis = 1)
    pNeg = np.sum((Xtest>0)*np.log10(thetaNegTrue), axis = 1) + np.sum((Xtest==0)*np.log10(1-thetaNegTrue), axis = 1)
    pmap = pPos>pNeg
    yPredict = np.repeat(0,Xtest.shape[0])
    yPredict[pmap] = 1
    yPredict[-pmap] = -1
    # now calcuate the accuracy
    rightnum = sum(yPredict == ytest)
    Accuracy = rightnum / float(len(ytest))    
    return yPredict, Accuracy


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print "Usage: python naiveBayes.py dataSetPath testSetPath"
        sys.exit()

    print "--------------------"
    textDataSetsDirectoryFullPath = sys.argv[1]
    testFileDirectoryFullPath = sys.argv[2]


    Xtrain, Xtest, ytrain, ytest = loadData(textDataSetsDirectoryFullPath)


    thetaPos, thetaNeg = naiveBayesMulFeature_train(Xtrain, ytrain)
    print "thetaPos =", thetaPos
    print "thetaNeg =", thetaNeg
    print "--------------------"

    yPredict, Accuracy = naiveBayesMulFeature_test(Xtest, ytest, thetaPos, thetaNeg)
    print "MNBC classification accuracy =", Accuracy

    Accuracy_sk = naiveBayesMulFeature_sk_MNBC(Xtrain, ytrain, Xtest, ytest)
    print "Sklearn MultinomialNB accuracy =", Accuracy_sk

    yPredict, Accuracy = naiveBayesMulFeature_testDirect(testFileDirectoryFullPath, thetaPos, thetaNeg)
    print "Directly MNBC tesing accuracy =", Accuracy
    print "--------------------"

    thetaPosTrue, thetaNegTrue = naiveBayesBernFeature_train(Xtrain, ytrain)
    print "thetaPosTrue =", thetaPosTrue
    print "thetaNegTrue =", thetaNegTrue
    print "--------------------"

    yPredict, Accuracy = naiveBayesBernFeature_test(Xtest, ytest, thetaPosTrue, thetaNegTrue)
    print "BNBC classification accuracy =", Accuracy
    print "--------------------"


