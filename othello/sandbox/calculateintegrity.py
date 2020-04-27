'''
Created on Apr 26, 2020

@author: summer
'''
import numpy as np
import hashlib

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

if __name__ == '__main__':
    
    board =  [0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]   
    light = 1
    dark = 2
    blank = 0
    size = 6
    # get the final board shape
    board_array = np.array(board)
    finalboard = np.reshape(board_array, (size, size))
    trans_board = np.transpose(finalboard)
    trans_board_list = []
    for a in range(0,size):
        for b in range(0,size):
            trans_board_list.append(trans_board[a][b])
            b = b+1
        a = a+1
    
    integrity_light = calsha256(trans_board_list, light, dark, blank, light)
    integrity_dark =calsha256(trans_board_list, light, dark, blank, dark)
    
    print(integrity_light)
    print(integrity_dark)