import hashlib
import math
import numpy as np
from pip._vendor.pyparsing import col
from _sqlite3 import IntegrityError
import re
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

# integrity = 'f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465'
# print(len(integrity))

# def ishex(s):
#     list_s = list(s)
#     nohex = 0
#     for i in range (0, len(list_s)):
#         print(list_s[i])
#         if (list_s[i] >= '0' ) and (list_s[i] <= '9'):
#             nohex +=0
#             print(nohex)
#         if (list_s[i] >= 'A') and (list_s[i] <= 'F'):
#             nohex +=0
#             print(nohex)
#         if (list_s[i] >= 'a') and (list_s[i] <= 'f'):
#             nohex +=0
#             print(nohex)
#         else:
#             nohex +=1
#             print(nohex)
#      
#     if nohex == 0:
#         return 0
#     if nohex != 0:
#         return 1
#  
# if __name__ == '__main__':
#     integrity = 'f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465'
#     print(len(integrity))
#     list_i = list(integrity)
#     print(list_i)
#     re = ishex(integrity)
#     print(re)

# a = '3'
# b = '4'
# if a < b:
#     print(1)

# integrity = 'f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465'
# char = re.findall(r'[a-f]',integrity)
# print(char)
# bigchar = re.findall(r'[A-F]',integrity)
# print(bigchar)
# num = re.findall(r'[0-9]',integrity)
# print(num)
# lenstr = len(char)+len(bigchar)+len(num)
# print(lenstr)
# if (lenstr == 64):
#     print(1)

# 
# def calsha256(board, light, dark, blank, nextplayer):
#     board_new = [str(x) for x in board]
#     strboard = ''.join(board_new)
#  
#     followinglist = []
#     followinglist.append('/')
#     followinglist.append(light)
#     followinglist.append('/')
#     followinglist.append(dark)
#     followinglist.append('/')
#     followinglist.append(blank)
#     followinglist.append('/')
#     followinglist_new = followinglist
#     followinglist_new.append(nextplayer)
#      
#     followinglist_new1 = [str(x) for x in followinglist_new]
#     strfollowinglist = ''.join(followinglist_new1)
#     str_new = strboard + strfollowinglist
#      
#     h = hashlib.sha256()
#     h.update(str_new.encode())
#     integrity = h.hexdigest()
#     return integrity
#  
# if __name__ == '__main__':
#     light =1
#     dark=2
#     blank=0
#     board =[1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0, 1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1]
#     h1=calsha256(board, light, dark, blank, light)
#     h2 =calsha256(board, light, dark, blank, dark)
#     print(h1)
#     print(h2)
#     integrity ='8a1c0659575e8cdd01b2e4ff3f431c845e7e7960279bb7abfaa5465e4a755354'
    
#   
# a = '000000002000009200002900000000000000/9/2/0/9'
# h = hashlib.sha256()
# h.update(a.encode())
# integrity = h.hexdigest()
# print(integrity)
# 
# a = '111111112111110211112011111111111111/0/2/1/0'
# h = hashlib.sha256()
# h.update(a.encode())
# integrity = h.hexdigest()
# print(integrity)
# 
# 
# a = '333333333333331233332133333333333333/1/2/3/2'
# h = hashlib.sha256()
# h.update(a.encode())
# integrity = h.hexdigest()
# print(integrity)
     
# a = '1111111111111111111111111111111011111100111111021111111011111111/1/2/0/1'
# b = '1111111111111111111111111111111011111100111111021111111011111111/1/2/0/2'
# h1 = hashlib.sha256()
# h2 = hashlib.sha256()
# h1.update(a.encode())
# h2.update(b.encode())
# s1 = h1.hexdigest()
# s2 = h2.hexdigest()
# print(s1)
# print(s2)

# 
# a = '000000000000001200002100000000000000/1/2/0/2'
# h = hashlib.sha256()
# h.update(a.encode())
# integrity = h.hexdigest()
# print(integrity)

board =[1,2,2,1,2,2,2,1]
boardset = set(board)
boardset_list = list(boardset)
print(boardset_list)

boardset = set(board)
boardset_list = list(boardset)
for i in range(0, len(boardset_list)):
    if (boardset_list[i]!=1 and boardset_list[i]!= 2 and boardset_list[i]!=0):
        print(0)

print(1)
