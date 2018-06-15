#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
/* Find a knight's tour around a chessboard, visit every square exactly once */
/* a complete tour returns to the same square, but this is not what we are looking for today */

/* This is a rehash of a version I wrote 4/5 years ago */

/* surprisingly, the particular move order finds more solutions in less time than others I have tried */

/* simple printer funtion for board aka an array */
void prar(int ** ar, int n);

unsigned long long CALL_COUNT = 0;

/* use backtracking to find solutions */
void _solve(int x, int y, int** board, int n){
    ++CALL_COUNT;
    
    /* all possible components of a legal knight move */
    const int moves[4] = {-2,-1,1,2};
    /* base case: check if solution */
    if(board[x][y] == n * n){
        prar(board, n); /* we found ourselves a winner */
        printf("\n%llu\n\n", CALL_COUNT);
    }
    /* try all move combinations */
    int i,j;
    for(i = 0; i < 4; i++){
        for(j = 0; j < 4; j++){
            /* knight move is assymetrical L shape */
            if(abs(moves[i]) == abs(moves[j])){
                continue;
            }
            /* set position to try */
            int newx = x + moves[i];
            int newy = y + moves[j];
            /* check if move is on the board or onto an already used square */
            if(newx < 0 || newx >= n || newy < 0 ||  newy >= n || board[newx][newy] != 0){
                continue;
            }
            /* set the co-ordinates of the new position to current move + 1 */
            board[newx][newy] = board[x][y] + 1;
            /* recurse and propogate solution if found */
            _solve(newx, newy, board, n);

            /* undo change to board */
            board[newx][newy] = 0;
        }
    }
    /* all paths exhausted */
    return;
}

/* wrapper for solving function */
void solve(int n){
    /* allocate a board */
    int i;
    int ** board = malloc(sizeof(int*) * n);
    for(i = 0; i < n; i++){
        board[i] = malloc(sizeof(int) * n);
    }
    /* start at position 0,0 */
    int x = 0;
    int y = 0;
    /* start move index as 1 (either this or change _solve's base case) */
    board[x][y] = 1;
    _solve(x,y, board, n);
    /* free some stuff ... */
}

/* simple printer funtion for board aka an array */
void prar(int ** ar, int n){
    int i,j;
    for(i = 0; i < n; i++){
        for(j = 0; j < n; j++){
            if(j != 0) putchar(' ');
            if(ar[i][j] < 10) putchar(' ');
            printf("%d", ar[i][j]);
            if(j == n - 1) putchar('\n');
        }
    }
}






/* driver */
int main(int argc, char** argv){
    /* printf("%llu", ULLONG_MAX); */
    int n = 8;
    if(argc >= 2){
        n = atoi(argv[1]);
    }
    solve(n);
}



