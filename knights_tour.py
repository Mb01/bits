#!/usr/bin/env python

from itertools import starmap
from time import sleep
# we'll use fixed dimensions
dim = 8
board = [[0]*dim,[0]*dim,[0]*dim,[0]*dim,[0]*dim,[0]*dim,[0]*dim,[0]*dim]
def all_moves(): 
    return [(1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)]
COUNT = 0

def tour(x, y, n, board):
    #global COUNT
    #print n, COUNT
    #COUNT += 1
    # stop if move not available
    if x < 0 or x >= dim or y < 0 or y >= dim or board[x][y]:
        return
    
    # try moving there
    board[x][y] = n

    # 64 moves is a winner
    if n == 64:
        print_board(board)
    
    # try all the moves
    for mx,my in all_moves():
        tour(x+mx, y+my,n+1, board)

    # reset the square
    board[x][y] = 0



def tour_optimized(x, y, n, board):
    #global COUNT
    #print n, COUNT
    #COUNT += 1
    # like above but moves to corner when possible
    # skipping a corner low in the tree causes exceedingly long run times
    def corner_available():
        def inner(ax, ay, bx, by, cx, cy):
            # returns a corner coordinate if we can move there
            if ((x,y) == (ax,ay) or (x,y) == (bx,by)) and not board[cy][cy]:
                return (cx,cy)
        for res in starmap(inner,  
            ((1,2,2,1,0,0), (5,1,6,2,7,0),(5,6,6,5,7,7),(1,5,2,6,0,7))):
            if res:
                return res
 
    # stop if move not available
    if x < 0 or x >= dim or y < 0 or y >= dim or board[x][y]:
        return
    
    # try moving there
    board[x][y] = n

    # 64 moves is a winner
    if n == 64:
        print_board(board)
    
    # check for corner move
    ca = corner_available()
    if ca:
        print_board(board)
        mx, my = ca
        tour_optimized(mx, my, n+1, board)
        
        # for times sake, we'll skip over tours that would finish in the corner even after skipping one of our two chances to enter the corner
        # this finds more tours much more quickly
    
    # try all the moves 
    else:
        for mx,my in all_moves():
            tour_optimized(x+mx, y+my,n+1, board)

    # always reset the square
    board[x][y] = 0


def print_board(board):
    for row in board:
        print " ".join([str(col) for col in row])
    print '\n'


#tour_optimized(0,0,1,board)
tour(0,0,1,board)
