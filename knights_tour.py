#!/usr/bin/env python

# go to every square on a chess board with a knight
# let's write this in a testable manner this time
from itertools import starmap

# we'll use fixed dimensions
dim = 8
board = [[0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0, 0, 0, 0]]

# all the moves a knight can make
def all_moves(): 
    return [(1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)]

COUNT = 0

# can we move to a corner
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


def tour_optimized(x, y, n, board):
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

print "==========\n" * 3
print "FROM ALMOST SOLVED"
tour_optimized(3, 0, 62, almost_solved_board)
print "==========\n" * 3

tour_optimized(0,0,1,board)
#tour(0,0,1,board)

