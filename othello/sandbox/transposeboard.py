'''
Created on Apr 26, 2020

@author: summer
'''
def transposeboard(board, size):
    
    finalboard = []
    for x in range(0,size):
        finalboard.append([])
        for y in range(0,size):
            finalboard[x].append(board[x*size+y])
            y += 1
        x += 1
        
    trans_board =[]
    for p in range(0,size):
        trans_board.append([])
        for q in range(0,size):
            trans_board[p].append(finalboard[q][p])
            q += 1
        q += 1
    
    trans_board_list = []
    for a in range(0,size):
        for b in range(0,size):
            trans_board_list.append(trans_board[a][b])
            b = b+1
        a = a+1
    
    return trans_board_list

if __name__=='__main__':
    board =[0,0,0,0,1,1,1,2,3,4,5,4,2,3,4,5]
    size =4
    re = transposeboard(board,size)
    print(re)
    