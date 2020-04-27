'''
    Created on Apr 27, 2020
    
    @author:  Ye Zhao
'''
from othello.status import calsha256, getfinalboard, transposeboard, hasposition,ishex
import re
import math

def _place(parmDictionary):
    
    DEFAULT_LIGHT = 1
    DEFAULT_DARK = 2
    DEFAULT_BLANK = 0
    resultDict = {}
    
    #valid light
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
    light = int(light)
    
    #valid dark
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
    dark = int(dark)
    
    #valid blank
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
    blank = int(blank)
    
    #valid board
    if(not('board' in parmDictionary)):
        resultDict['status'] = 'error: missing board'
        return resultDict
    board = parmDictionary['board']
    if(board == ''):
        resultDict['status'] = 'error: null board'
        return resultDict
    
    if(type(board) != list):
        boardnum = re.findall(r'[0-9]',board)
        board_list = list()
        for x in boardnum:
            board_list.append(int(x))
        board = board_list
    
    #validate size
    size = math.sqrt(len(board))
    sizeNumeric = float(size)
    if(sizeNumeric - int(sizeNumeric) > 0):
        resultDict['status'] = 'error: non square board'
        return resultDict 
    size = int(size) 
    
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
    
    #valid location
    location = parmDictionary['location']
    
    a = re.findall(r'\d+\.?\d*',location)
    list1 = list()
    for x in a :
        list1.append(int(x))
    location_row =list1[0]
    location_column = list1[1]
    location_row = int(location_row)-1
    location_column =  int(location_column)-1
    
    #valid integrity
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
    
    
    finalboard = getfinalboard(board, size)
    
    trans_board_list = transposeboard(board, size)
    integrity_light = calsha256(trans_board_list, light, dark, blank, light)
    integrity_dark = calsha256(trans_board_list, light, dark, blank, dark)
    
    #get the direction of the place coin
    leftupdict = direction0(finalboard, location_row, location_column, light, dark, blank, size)
    updict = direction1(finalboard, location_row, location_column, light, dark, blank, size)
    rightupdict = direction2(finalboard, location_row, location_column, light, dark, blank, size)
    leftdict = direction3(finalboard, location_row, location_column, light, dark, blank, size)
    rightdict = direction4(finalboard, location_row, location_column, light, dark, blank, size)
    leftdowndict = direction5(finalboard, location_row, location_column, light, dark, blank, size)
    downdict = direction6(finalboard, location_row, location_column, light, dark, blank, size)
    rightdowndict = direction7(finalboard, location_row, location_column, light, dark, blank, size)
    
    #inverse the coin
    if integrity == integrity_light:
        next_player0 = light
        next_player1 = dark
        if leftupdict['dark'] != 0 :
            finalboard[location_row][location_column] = light
            for i in range (0, leftupdict['dark']):
                location_row -=1
                location_column -=1
                finalboard[location_row][location_column] = light
        elif updict['dark'] != 0 :
            finalboard[location_row][location_column] = light
            for i in range (0, updict['dark']):
                location_row -=1
                finalboard[location_row][location_column] = light
        elif rightupdict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, rightupdict['dark']):
                location_row -=1
                location_column +=1
                finalboard[location_row][location_column] = light
        elif leftdict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, leftdict['dark']):
                location_column -=1
                finalboard[location_row][location_column] = light
        elif rightdict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, rightdict['dark']):
                location_column +=1
                finalboard[location_row][location_column] = light
        elif leftdowndict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, leftdowndict['dark']):
                location_row +=1
                location_column -=1
                finalboard[location_row][location_column] = light
        elif downdict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, downdict['dark']):
                location_row +=1
                finalboard[location_row][location_column] = light
        elif rightdowndict['dark'] != 0:
            finalboard[location_row][location_column] = light
            for i in range (0, rightdowndict['dark']):
                location_row +=1
                location_column +=1
                finalboard[location_row][location_column] = light
        else:
            # location cannot be placed
            return resultDict
        
    elif integrity == integrity_dark:
        next_player0 = dark
        next_player1 = light
        if leftupdict['light'] != 0 :
            finalboard[location_row][location_column] = dark
            for i in range (0, leftupdict['light']):
                location_row -=1
                location_column -=1
                finalboard[location_row][location_column] = dark
        elif updict['light'] != 0 :
            finalboard[location_row][location_column] = dark
            for i in range (0, updict['light']):
                location_row -=1
                finalboard[location_row][location_column] = dark
        elif rightupdict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, rightupdict['light']):
                location_row -=1
                location_column +=1
                finalboard[location_row][location_column] = dark
        elif leftdict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, leftdict['light']):
                location_column -=1
                finalboard[location_row][location_column] = dark
        elif rightdict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, rightdict['light']):
                location_column +=1
                finalboard[location_row][location_column] = dark
        elif leftdowndict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, leftdowndict['light']):
                location_row +=1
                location_column -=1
                finalboard[location_row][location_column] = dark
        elif downdict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, downdict['light']):
                location_row +=1
                finalboard[location_row][location_column] = dark
        elif rightdowndict['light'] != 0:
            finalboard[location_row][location_column] = dark
            for i in range (0, rightdowndict['light']):
                location_row +=1
                location_column +=1
                finalboard[location_row][location_column] = dark
        else:
            # location cannot be placed
            return resultDict 
    else:
        #error integrity
        return resultDict
    
    #get new play board
    finalboard_new = []
    for a in range(0,size):
        for b in range(0,size):
            finalboard_new.append(finalboard[a][b])
            b = b+1
        a = a+1
    
    resultDict['board'] = finalboard_new
    
    #determine the status
    light_possible = 0
    dark_possible =0

    blockdict_thisblock = {}
    for c in range(0,size):
        for d in range(0,size):
            if finalboard[c][d] == blank:
                blockdict0 = direction0(finalboard, c, d, light, dark, blank, size)
                blockdict1 = direction1(finalboard, c, d, light, dark, blank, size)
                blockdict2 = direction2(finalboard, c, d, light, dark, blank, size)
                blockdict3 = direction3(finalboard, c, d, light, dark, blank, size)
                blockdict4 = direction4(finalboard, c, d, light, dark, blank, size)
                blockdict5 = direction5(finalboard, c, d, light, dark, blank, size)
                blockdict6 = direction6(finalboard, c, d, light, dark, blank, size)
                blockdict7 = direction7(finalboard, c, d, light, dark, blank, size)
                
                blockdict_thisblock['dark'] = blockdict0['dark']+blockdict1['dark']+blockdict2['dark']+blockdict3['dark']+blockdict4['dark']+blockdict5['dark']+blockdict6['dark']+blockdict7['dark']
                blockdict_thisblock['light'] = blockdict0['light']+blockdict1['light']+blockdict2['light']+blockdict3['light']+blockdict4['light']+blockdict5['light']+blockdict6['light']+blockdict7['light']
                
                light_possible += blockdict_thisblock['dark']
                dark_possible += blockdict_thisblock['light']
    
    
    if light_possible > 0 and dark_possible >0 :
        resultDict['status'] = 'ok'
    if light_possible ==0 and dark_possible >0 :
        resultDict['status'] = 'ok'
        next_player1 = dark
    if light_possible >0 and dark_possible ==0 :
        resultDict['status'] = 'ok'
        next_player1 = light
    if light_possible ==0 and dark_possible ==0:
        light_token_count =0
        dark_token_count =0
        #determine the count of light and dark
        for s in range(0,len(finalboard_new)):
            if finalboard_new[s] == light:
                light_token_count +=1
            if finalboard_new[s] == dark:
                dark_token_count +=1 
                
        resultDict['status'] = 'end:' + str(light_token_count) + '/' + str(dark_token_count)
        next_player1 = dark

    #get new integrity
    trans_finalboard_new_list = transposeboard(finalboard_new, size)
    integrity_new = calsha256(trans_finalboard_new_list, light, dark, blank, next_player1)
    
    resultDict['integrity'] = integrity_new
    
    return resultDict
    
# left-up
def direction0(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row-1
    column = column-1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row -=1
            column -=1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row -=1
            column -=1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

# up
def direction1(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row-1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row -=1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row -=1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

#right up
def direction2(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row-1
    column = column+1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row -=1
            column = column+1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row -=1
            column = column+1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

#left
def direction3(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    column = column-1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            column = column-1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            column = column-1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

#right
def direction4(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    column = column+1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            column = column+1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            column = column+1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

#left-down
def direction5(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row+1
    column = column-1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row = row+1
            column = column-1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row = row+1
            column = column-1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict
    
#down
def direction6(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row+1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row = row+1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row = row+1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict

#right_down
def direction7(board, row, column, light, dark, blank, size):
    
    blank_dict = {}
    blank_dict['dark'] = 0
    blank_dict['light'] = 0
    blank_dict['has'] = 0
    
    row = row+1
    column = column+1
    i = 0
    a = 0
    if hasposition(row, column, size) == 1:
        if board[row][column] == blank:
            return blank_dict
        while board[row][column] == light:
            row = row+1
            column = column+1
            i += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == dark:
                    blank_dict['light'] += i
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
        while board[row][column] == dark:
            row = row+1
            column = column+1
            a += 1
            if hasposition(row, column, size) == 1:
                if board[row][column] == light:
                    blank_dict['dark'] += a
                    blank_dict['has'] = blank_dict['light'] + blank_dict['dark']
                    return blank_dict
                if board[row][column] == blank:
                    return blank_dict
            else:
                return blank_dict
    else:
        return blank_dict