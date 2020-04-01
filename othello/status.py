'''
    Created on Mar 30, 2020
    
    @author:  Ye Zhao
'''

import math
import numpy as np
import re
import hashlib

def _status(parmDictionary):
    
    DEFAULT_LIGHT = 1
    DEFAULT_DARK = 2
    DEFAULT_BLANK = 0
    resultDict = {}
    
    # Validate light
    if(not('light' in parmDictionary)):
        parmDictionary['light'] = DEFAULT_LIGHT
    light = parmDictionary['light']
    if(light == ''):
        resultDict['status'] = 'error: null light'
        return resultDict
    try:
        lightNumeric = float(light)
        if(lightNumeric - int(lightNumeric) > 0):
            resultDict['status'] = 'error: non integer light'
            return resultDict
        light = int(light)
    except:
        resultDict['status'] = 'error: non integer light'
        return resultDict
    if(light < 0):
        resultDict['status'] = 'error: below bound light'
        return resultDict
    if(light > 9):
        resultDict['status'] = 'error: above bound light'
        return resultDict
    
    # Validate dark
    if(not('dark' in parmDictionary)):
        parmDictionary['dark'] = DEFAULT_DARK
    dark = parmDictionary['dark']
    if(dark == ''):
        resultDict['status'] = 'error: null dark'
        return resultDict
    try:
        darkNumeric = float(dark)
        if(darkNumeric - int(darkNumeric) > 0):
            resultDict['status'] = 'error: non integer dark'
            return resultDict
        dark = int(dark)
    except:
        resultDict['status'] = 'error: non integer dark'
        return resultDict
    if(dark < 0):
        resultDict['status'] = 'error: below bound dark'
        return resultDict
    if(dark > 9):
        resultDict['status'] = 'error: above bound dark'
        return resultDict
    
    # Validate blank
    if(not('blank' in parmDictionary)):
        parmDictionary['blank'] = DEFAULT_BLANK
    blank = parmDictionary['blank']
    if(blank == ''):
        resultDict['status'] = 'error: null blank'
        return resultDict
    try:
        blankNumeric = float(blank)
        if(blankNumeric - int(blankNumeric) > 0):
            resultDict['status'] = 'error: non integer blank'
            return resultDict
        blank = int(blank)
    except:
        resultDict['status'] = 'error: non integer blank'
        return resultDict
    if(blank < 0):
        resultDict['status'] = 'error: below bound blank'
        return resultDict
    if(blank > 9):
        resultDict['status'] = 'error: above bound blank'
        return resultDict
    
    # Validate board
    if(not('board' in parmDictionary)):
        resultDict['status'] = 'error: missing board'
        return resultDict
    board = parmDictionary['board']
    if(board == ''):
        resultDict['status'] = 'error: null board'
        return resultDict
    # determine the tokens in board
    if (light not in board) or (dark not in board) or (blank not in board):
        resultDict['status'] = 'error: non light/dark/blank tokens board'
        return resultDict
        
    # Validate Size
    size = math.sqrt(len(board))
    sizeNumeric = float(size)
    if(sizeNumeric - int(sizeNumeric) > 0):
        resultDict['status'] = 'error: non square board'
        return resultDict 
    size = int(size)  
    
    # boundary of board
    if(size < 6):
        resultDict['status'] = 'error: below board'
        return resultDict
    if(size > 16):
        resultDict['status'] = 'error: above board'
        return resultDict
             
    # size is odd or not
    halfsize = size / 2
    halfsizeNumeric = float(halfsize)
    if(halfsizeNumeric - int(halfsizeNumeric) > 0):
        resultDict['status'] = 'error: odd board'
        return resultDict
    size = int(size)
    
    # Validate integrity
    if(not('integrity' in parmDictionary)):
        resultDict['status'] = 'error: missing integrity'
        return resultDict
    integrity = parmDictionary['integrity']
    if(integrity == ''):
        resultDict['status'] = 'error: null integrity'
        return resultDict
    integritylength = len(integrity)
    if (integritylength < 64):
        resultDict['status'] = 'error: short integrity'
        return resultDict
    if (integritylength > 64):
        resultDict['status'] = 'error: long integrity'
        return resultDict
    if (ishex(integrity) != 0):
        resultDict['status'] = 'error: non hex characters integrity'
        return resultDict
    
    if(dark == light):
        resultDict['status'] = 'error: dark and light have the same value'
        return resultDict
    if(blank == light):
        resultDict['status'] = 'error: blank and light have the same value'
        return resultDict
    if(dark == blank):
        resultDict['status'] = 'error: blank and dark have the same value'
        return resultDict
    
    integrity_light = calsha256(board, light, dark, blank, light)
    integrity_dark =calsha256(board, light, dark, blank, dark)
    
    if (integrity != integrity_light) and (integrity != integrity_dark):
        resultDict['status'] = 'error: invalid integrity'
        return resultDict
    
    # get the final board shape
    board_array = np.array(board)
    finalboard = np.reshape(board_array, (size, size))

    # get the number of possible ways of light and dark tokens
    light_possibleways = 0
    dark_possibleways = 0
    
    for index1 in range(0,size):
        for index2 in range(0,size):
            # get the light tokens possible ways number
            if (finalboard[index1][index2] == light):
                has0 = closeblock0(finalboard, index1, index2, blank, size)
                has1 = closeblock1(finalboard, index1, index2, blank, size)
                has2 = closeblock2(finalboard, index1, index2, blank, size)
                has3 = closeblock3(finalboard, index1, index2, blank, size)
                has4 = closeblock4(finalboard, index1, index2, blank, size)
                has5 = closeblock5(finalboard, index1, index2, blank, size)
                has6 = closeblock6(finalboard, index1, index2, blank, size)
                has7 = closeblock7(finalboard, index1, index2, blank, size)
                final_has = has0+has1+has2+has3+has4+has5+has6+has7
                light_possibleways +=final_has
            # get the dark tokens possible ways number
            if (finalboard[index1][index2] == dark):
                has0 = closeblock0(finalboard, index1, index2, blank, size)
                has1 = closeblock1(finalboard, index1, index2, blank, size)
                has2 = closeblock2(finalboard, index1, index2, blank, size)
                has3 = closeblock3(finalboard, index1, index2, blank, size)
                has4 = closeblock4(finalboard, index1, index2, blank, size)
                has5 = closeblock5(finalboard, index1, index2, blank, size)
                has6 = closeblock6(finalboard, index1, index2, blank, size)
                has7 = closeblock7(finalboard, index1, index2, blank, size)
                final_has = has0+has1+has2+has3+has4+has5+has6+has7
                dark_possibleways +=final_has
    
    # determine the status
    if light_possibleways >0 and dark_possibleways >0:
        resultDict['status'] = 'ok'
        return resultDict
    
    if light_possibleways == 0 and dark_possibleways >0:
        resultDict['status'] = 'dark'
        return resultDict
    
    if light_possibleways >0 and dark_possibleways == 0:
        resultDict['status'] = 'light'
        return resultDict
      
    if light_possibleways == 0 and dark_possibleways == 0:
        resultDict['status'] = 'end'
        return resultDict     
     
    else:
        resultDict['status'] = 'not ok'
        return resultDict

# determine if the string is hex
def ishex(s):
    lowerchar = re.findall(r'[a-f]',s)
    upperchar = re.findall(r'[A-F]',s)
    num = re.findall(r'[0-9]',s)
    lenstr = len(lowerchar)+len(upperchar)+len(num)
    if lenstr == 64:
        return 0
    if lenstr != 64:
        return 1

# calculate the integrity based on the board
def calsha256(board, light, dark, blank, nextplayer):
    board_new = [str(x) for x in board]
    strboard = ''.join(board_new)

    followinglist = []
    followinglist.append('/')
    followinglist.append(light)
    followinglist.append('/')
    followinglist.append(dark)
    followinglist.append('/')
    followinglist.append(blank)
    followinglist.append('/')
    followinglist_new = followinglist
    followinglist_new.append(nextplayer)
    
    followinglist_new1 = [str(x) for x in followinglist_new]
    strfollowinglist = ''.join(followinglist_new1)
    str_new = strboard + strfollowinglist
    
    h = hashlib.sha256()
    h.update(str_new.encode())
    integrity = h.hexdigest()
    
    return integrity
    
# determine if it is out of boundary                       
def hasposition(x, y, maxsize):
    if x < 0 or x > maxsize-1 or y <0 or y > maxsize-1 :
        return 0
    return 1

# left-up
def closeblock0(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row-1
    column = column-1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row -=1
            column -=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has
    
# up     
def closeblock1(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row-1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row -=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has
    
# right-up
def closeblock2(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row-1
    column = column+1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row -=1
            column +=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has
    
# left
def closeblock3(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    column = column-1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            column -=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has

# right
def closeblock4(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    column = column+1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            column +=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has
           
# left-down
def closeblock5(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row+1
    column = column-1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row = row+1
            column -=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has

# down
def closeblock6(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row+1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row +=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has  

# right-down
def closeblock7(board, row, column, blank, size):
    has = 0
    getchess = board[row][column]
    row = row+1
    column = column+1
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return has
        if board[row][column] == getchess:
            return has
        while board[row][column] != getchess:
            row +=1
            column +=1
            if hasposition(row, column, size) == 1:
                if board[row][column] == blank:
                    has = 1
                    return has
                if board[row][column] == getchess:
                    has = 0
                    return has
            else:
                return has
    else:
        return has            
