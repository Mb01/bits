#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
/* Find a knight's tour around a chessboard, visit every square exactly once */
/* a complete tour returns to the same square, but this is not what we are looking for today */

/* Trying better algorithm  */


/* simple printer funtion for board aka an array */
void prar(int ** ar, int n);

unsigned long long CALL_COUNT = 0;

/* use backtracking to find solutions */
void _solve(int x, int y, int** board, int n){
  ++CALL_COUNT;

  int i; /* iterator */
  int newx;
  int newy; /* new position on board to call function */
  
  /* all knight moves */
  /* strangley enough, not generating the moves on the fly resulted in slower times */
  const int moves_x[8] = {2, 2,-2,-2, 1, 1,-1,-1};
  const int moves_y[8] = {1,-1, 1,-1, 2,-2, 2,-2};


  /* base case: check if solution */
  if(board[x][y] == n * n){
    prar(board, n); /* we found ourselves a winner */
    printf("\naprox %llu million tries]\n", CALL_COUNT / 1000000);
  }

  /* move to corner if available */
  if(((x==1&&y==2) || (x==2&&y==1)) && board[0][0] == 0){ /* top left corner */
    board[0][0] = board[x][y] + 1;
    _solve(0, 0, board, n);
  }
  if(((x==5&&y==1) || (x==6&&y==2)) && board[7][0] == 0){ /* top right corner */
    board[7][0] = board[x][y] + 1;
    _solve(7, 0, board, n);
  }
  if(((x==1&&y==5) || (x==2&&y==6)) && board[0][7] == 0){ /* bot left corner */
    board[0][7] = board[x][y] + 1;
    _solve(0, 7, board, n);
  }
  if(((x==5&&y==6) || (x==6&&y==5)) && board[7][7] == 0){ /* bot right corner */
    board[7][7] = board[x][y] + 1;
    _solve(7, 7, board, n);
  }
  /* continue to try all move combinations (at this stage including corners again) */
  
  for(i = 0; i < 8; i++){
    /* set position to try */
    newx = x + moves_x[i];
    newy = y + moves_y[i];
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
  /* free board ... */
}


/* driver */
int main(int argc, char** argv){
  int n = 8;
  if(argc >= 2){
    n = atoi(argv[1]);
  }
  solve(n);
}

void prar(int ** ar, int n){
  int i,j;
  for(i = 0; i < n; i++){
    for(j = 0; j < n; j++){
      if(j != 0) putchar(' '); /* no space at start of line  */
      if(ar[j][i] < 10) putchar(' '); /* compensate for single digits */
      printf("%d", ar[j][i]); /* inversion means now x,y in sensible order */
      if(j == n - 1) putchar('\n');
    }
  }
}
