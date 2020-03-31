import hashlib
import math
import numpy as np
from pip._vendor.pyparsing import col
from _sqlite3 import IntegrityError
# 
# board = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
# board_size = len(board)
# size_float = math.sqrt(len(board))
# size = int(size_float)
# board_array = np.array(board)
# board_array = np.reshape(board_array, (size, size))
#     
# print(board_array[7][2])



# def hasposition(x, y, maxsize):
#     if x < 0 or x > maxsize or y <0 or y > maxsize :
#         return 0
#     return 1
# 
# def closeblock0(board, row, column, blank, size):
#     has = 0
#     getchess = board[row][column]
#     row = row-1
#     column = column-1
#     if hasposition(row, column, size) == 1:
#         if board[row][column] == blank:
#             return has
#         if board[row][column] == getchess:
#             return has
#         while board[row][column] != getchess:
#             row -=1
#             column -=1
#             if hasposition(row, column, size) == 1:
#                 if board[row][column] == blank:
#                     has = 1
#                     return has
#                 if board[row][column] == getchess:
#                     has = 0
#                     return has
#             else:
#                 return has
#     else:
#         return has
# 
# if __name__ == '__main__':
#     size = 4
#     board = np.zeros((size, size))
#     board[1][1]=2
#     board[1][2]=1
#     board[2][1]=2
#     board[2][2]=2
#     re = closeblock0(board, 2, 2, 0, size)
#     print(re)
#     

integrity = 'f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465'
print(len(integrity))
    
