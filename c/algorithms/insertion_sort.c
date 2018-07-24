#include <stdio.h>
#include "utils.h"

void insertion_sort(int* ar, int length){
  int i,j,key;
  for(j = 1; j < length; j++){ 	/* start with the second element, continue for all of j */
    key = ar[j]; 		/* store the element to insert */
    i = j - 1;			/* begin comparing with the element to the left */
    while(i > -1 && ar[i] > key){ /* if ar[i] is within the array and that element bigger */
      ar[i + 1] = ar[i];	  /* move the left element right */
      i--;
    }
    ar[i+1] = key;		/* insert *before* the condition that ended the while loop */
    /* now ar is sorted up to j */
  }
  /* ar is sorted up to length, so therefore sorted */
}

void insertion_sort_descending(int* ar, int length){
  int i,j,key;
  for(j = 1; j < length; j++){ 	/* start with the second element, continue for all of j */
    key = ar[j]; 		/* store the element to insert */
    i = j - 1;			/* begin comparing with the element to the left */
    while(i > -1 && ar[i] < key){ /* if ar[i] is within the array and that element bigger */
      ar[i + 1] = ar[i];	  /* move the left element right */
      i--;
    }
    ar[i+1] = key;		/* insert *before* the condition that ended the while loop */
    /* now ar is sorted up to j */
  }
  /* ar is sorted up to length, so therefore sorted */
}

int main(int argc, char** argv){
  int to_sort[] = {5, 7, 14, 27, 2, 3, 11, 13, 17, 23, 19};
  prar(to_sort, 11);
  insertion_sort(to_sort, 11);
  insertion_sort_descending(to_sort, 11);
  prar(to_sort, 11);
  return 0;
}
