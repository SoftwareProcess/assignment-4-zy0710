'''
    Created on Apr 27, 2020
    
    @author:  Ye Zhao
'''
from othello.status import calsha256, getfinalboard, transposeboard, hasposition
import re
import math

def _place(parmDictionary):
    
    DEFAULT_LIGHT = 1
    DEFAULT_DARK = 2
    DEFAULT_BLANK = 0
    resultDict = {}
    
    #valid light
    light = parmDictionary['light']
    light = int(light)
    
    #valid dark
    dark = parmDictionary['dark']
    dark = int(dark)
    
    #valid blank
    blank = parmDictionary['blank']
    blank = int(blank)
    
    #valid board
    board = parmDictionary['board']
    if(type(board) != list):
        boardnum = re.findall(r'[0-9]',board)
        board_list = list()
        for x in boardnum:
            board_list.append(int(x))
        board = board_list
    
    #validate size
    size = math.sqrt(len(board))
    size = int(size) 
    
    #valid location
    location_row = parmDictionary['location_row']
    location_row = int(location_row)-1
    location_column = parmDictionary['location_column']
    location_column =  int(location_column)-1
    
    #valid integrity
    integrity = parmDictionary['integrity']
    
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
        
    if integrity == integrity_dark:
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
    
    #get new integrity
    trans_finalboard_new_list = transposeboard(finalboard_new, size)
    integrity_new = calsha256(trans_finalboard_new_list, light, dark, blank, next_player1)
    
    resultDict['integrity'] = integrity_new
    resultDict['status'] = 'ok'
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