#include <stdio.h>
#include "utils.h"

void insertion_sort(int* ar, int length){
  int i,j,key;
  for(j = 1; j < length; j++){ 	/* start with the second element */
    key = ar[j]; 		
    i = j - 1;			/* begin comparing with the elemet to the left */
    while(i > -1 && ar[i] > key){ /* if it's bigger and we haven't run off the array */
      ar[i + 1] = ar[i];	  /* move the left element right */
      i--;
    }
    ar[i+1] = key;		/* insert *before* the condition that ended the while loop */
  }
}

int main(int argc, char** argv){
  int to_sort[] = {5, 7, 14, 27, 2, 3, 11, 13, 17, 23, 19};
  prar(to_sort, 11);
  insertion_sort(to_sort, 11);
  prar(to_sort, 11);
  return 0;
}
