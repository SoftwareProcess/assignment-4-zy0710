'''
    Created on Mar 30, 2020
    
    @author:  Ye Zhao
'''

import math
import numpy as np

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
    
    # Validate board
    board = parmDictionary['board']
    
    # Validate integrity
    integrity = parmDictionary['integrity']
    
    # Validate Size
    size = int(math.sqrt(len(board)))

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
                
        
    
    
    
    
