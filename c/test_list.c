#include <stdlib.h>
#include <stdio.h>
#include "list.h"






int main(int argc, char** argv){
  int ar[] = {1,2,3,4,5,6,22,25};

  int* ar2 = malloc(sizeof(int) * 8);
  ar2[0] = 55;

  node* head = array_to_list(ar, 8);
  node* head2 = array_to_list(ar2, 8);
  list_print(head);
  list_print(head2);
  printf("finished %s\n", argv[0]);
}

