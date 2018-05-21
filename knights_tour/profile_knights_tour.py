#!/usr/bin/env python
import cProfile
from itertools import starmap
from knights_tour import COUNT, dim, board, all_moves, print_board


# we'll actually need a version that returns in a reasonable time
def profile_tour(x, y, n, board):
    global COUNT
    if COUNT > 2000000:
        return
    COUNT += 1
    # stop if move not available
    if x < 0 or x >= dim or y < 0 or y >= dim or board[x][y]:
        return
    
    # try moving there
    board[x][y] = n

    # 64 moves is a winner
    if n == 63:
        print_board(board)
    
    # try all the moves
    for mx,my in all_moves():
        profile_tour(x+mx, y+my,n+1, board)

    # reset the square
    board[x][y] = 0



def profile_tour_optimized(x, y, n, board):
    global COUNT
    if COUNT > 2000000:
        return
    COUNT += 1
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
    if n == 63:
        print_board(board)
    
    # check for corner move
    ca = corner_available()
    if ca:
        print "corner available"
        print_board(board)
        mx, my = ca
        profile_tour_optimized(mx, my, n+1, board)
        
        # for times sake, we'll skip over tours that would finish in the corner even after skipping one of our two chances to enter the corner
        # this finds more tours much more quickly
    
    # try all the moves 
    else:
        for mx,my in all_moves():
            profile_tour_optimized(x+mx, y+my,n+1, board)

    # always reset the square
    board[x][y] = 0




#cProfile.run('profile_tour(0,0,1,board)')
COUNT = 0
cProfile.run('profile_tour_optimized(0,0,1,board)')

