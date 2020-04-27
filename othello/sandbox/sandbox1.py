'''
Created on Apr 27, 2020

@author: summer
'''

from othello.status import calsha256, getfinalboard, transposeboard, hasposition
import re
import math

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

    
if __name__ == '__main__':
    board = [3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3]
    light =1
    dark =2
    blank=3
    size =6
    location_row =3-1
    location_column =5-1
    integrity = '66271cbb9037c515e73be3a74a37259a179f2d2861cf4e82130cd579a2141093'
    
    finalboard = getfinalboard(board, size)
    
    trans_board_list = transposeboard(board, size)
    integrity_light = calsha256(trans_board_list, light, dark, blank, light)
    integrity_dark = calsha256(trans_board_list, light, dark, blank, dark)
    
    print(finalboard)
    print(integrity_light)
    print(integrity_dark)
    
    if integrity == integrity_dark:
        print('dark')
    if integrity == integrity_light:
        print('light')
    
    leftupdict = direction0(finalboard, location_row, location_column, light, dark, blank, size)
    downdict = direction6(finalboard, location_row, location_column, light, dark, blank, size)
    rightdowndict = direction7(finalboard, location_row, location_column, light, dark, blank, size)
    leftdict = direction3(finalboard, location_row, location_column, light, dark, blank, size)
    
    print(leftdict)
    print(leftupdict)
    print(downdict)
    print(rightdowndict)
    
    
