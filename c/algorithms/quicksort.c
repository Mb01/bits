#include <stdio.h>
#include "utils.h"

void swap(int* a, int* b)
{
  *a ^= *b;
  *b ^= *a;
  *a ^= *b;
}

void quicksort(int* ar, int len)
{
  if(len < 2) return;
  /* this implementation requires that the pivot not be the first element when length is greater than 2 */
  /* otherwise a sorted array or subsection thereof will never return */
  int pivot = ar[len / 2]; 	
  int l,r;
  for (l = 0, r = len - 1;; l++, r--){
    while (ar[l] < pivot) l++;
    while (ar[r] > pivot) r--;
    if (l >= r) break;
    swap(ar + l,ar + r);
  }
    quicksort(ar, l);
    quicksort(ar + l, len - l);
}

int main(int argc, char** argv)
{
  int to_sort[] = {27, 5, 5, 7, 17, 27, 27, 2, 2, 2, 27, 3, 11, 13, 17, 23, 19, 5, 2};
  prar(to_sort, 19);
  quicksort(to_sort, 19);
  prar(to_sort, 19);
  return 0;
}


