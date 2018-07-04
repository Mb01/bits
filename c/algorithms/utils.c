#include <stdio.h>

/* our basic integer printing function */
void prar(int ar[], int length){
  int x;
  for(x = 0; x < length - 1; x++){
    printf("%d ", ar[x]);
  }
  printf("%d\n", ar[length - 1]);
} 
