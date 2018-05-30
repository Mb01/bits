#!/usr/bin/env python

# go to every square on a chess board with a knight
# in a more testable manner this time

from itertools import starmap
from random import shuffle

# we'll use fixed dimensions
dim = 8
### board[row][col]
board = [[0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0]]

# test board
no_corners_board = [[1, 0, 0, 0, 0, 0, 0, 2], 
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [3, 0, 0, 0, 0, 0, 0, 4]]

# all but the last three moves are solved
# we are on move 62 and at position (0,3)
# this should allow us to find the answer in a reasonable amount of time
almost_solved_board =  [[1 ,12 , 9  ,6  ,3 ,14 ,17 ,20],
[10  ,7  ,2 ,13 ,18 ,21  ,4 ,15],
[51 ,48 ,11 , 8  ,5 ,16 ,19 ,22],
[0 ,25 ,50 ,47 ,34 ,23 ,58 ,45],
[49 ,52 ,61 ,24 ,59 ,46 ,33 ,36],
[28 ,0 ,26 ,39 ,30 ,35 ,44 ,57],
[53 ,40 ,29 ,60 ,55 ,42 ,37 ,32],
[0 ,27 ,54 ,41 ,38 ,31 ,56 ,43]]

# all the moves a knight can make
# (delta-x, delta-y)
def all_moves(): 
    moves = [(1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)]
    shuffle(moves)
    return moves

def rotate(li):
    if li:
        tmp = li.pop(0)
        li.append(tmp)


# answers: can we move to a corner?
def corner_available(x,y, board):
    def inner(ax, ay, bx, by, cx, cy):
        # returns a corner coordinate if we can move there else None
        if ((x,y) == (ax,ay) or (x,y) == (bx,by)) and not board[cx][cy]:
            return (cx,cy)

    # the six magic numbers are coordinates for the two positions that can move
    # to a corner and then the coordinates of the corner
    # if we find a corner we can move to, return the coordinates of the corner    
    for res in starmap(inner,  
        ((1,2,2,1,0,0), (1,5,2,6,0,7),(5,6,6,5,7,7),(5,1,6,2,7,0))):
        if res:
            return res

COUNT = 0
# recursively searches for complete tours -> print to stdout 
def tour_optimized(x, y, n, board):
    global COUNT
    COUNT += 1
    if COUNT % 1000000 == 0:
        print COUNT/1000000, "million iterations"
        #print_board(board)
    # valid-move?
    if x < 0 or x >= dim or y < 0 or y >= dim or board[x][y]:
        return
    
    # add-move
    board[x][y] = n

    # 64 moves is a winner
    if n == 64:
        print_board(board)
    
    # if corner is available, go there
    ca = corner_available(x,y,board)
    if ca:
        mx, my = ca
        tour_optimized(mx, my, n+1, board)
    
    # making this next part an else statement means that 
    # tours both starting/ending in corners are skipped
    
    # or try all the moves
    else:
        for mx,my in all_moves():
            tour_optimized(x+mx, y+my,n+1, board)

    # un-add-move
    board[x][y] = 0


def print_board(board):
    for row in board:
        print " ".join([str(col) for col in row])
    print '\n'


print "=TEST=====\n" * 3
# short hand code, long hand tests...
assert(corner_available(1,2, board) == (0,0))
assert(corner_available(2,1, board) == (0,0))
assert(corner_available(1,5, board) == (0,7))
assert(corner_available(2,6, board) == (0,7))
assert(corner_available(6,5, board) == (7,7))
assert(corner_available(5,6, board) == (7,7))
assert(corner_available(5,1, board) == (7,0))
assert(corner_available(6,2, board) == (7,0))

assert(corner_available(1,2, no_corners_board) == None)
assert(corner_available(2,1, no_corners_board) == None)
assert(corner_available(5,1, no_corners_board) == None)
assert(corner_available(6,2, no_corners_board) == None)
assert(corner_available(6,5, no_corners_board) == None)
assert(corner_available(5,6, no_corners_board) == None)
assert(corner_available(1,5, no_corners_board) == None)
assert(corner_available(2,6, no_corners_board) == None)

assert(corner_available(5,1, almost_solved_board))


print "PROVING IT CAN SOLVE AN ALMOST FINISHED BOARD"
tour_optimized(3, 0, 62, almost_solved_board)
print "=END TEST=\n" * 3


print "please wait... a long time perhaps"

tour_optimized(0,0,1,board)
