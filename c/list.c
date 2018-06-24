/* list.c */
/* a linked list implementation */

#include <stdlib.h>
#include <stdio.h>
#include "list.h"

/* free a list */
void list_free(node* head){
    node* next = head;
    while(next != NULL){
        next = head->next;
        free(head);
    }
}

/* append a new node to end of list */
node* add_node(node* end, int data){
    node *new_node = malloc(sizeof(node));
    if(!new_node){
      printf("Out of memory? Couldn't allocate.");
      fflush(stdout); /*make sure error prints before crash */
        return new_node;
    }
    new_node->next = NULL;
    end->next = new_node;
    new_node->data = data;
    return new_node;
}

/* read a file of integers into a linked list */
node* filetolist(const char* filename){
    FILE *file = fopen(filename, "r");
    node *end, *head = malloc(sizeof(node));
    end = head;
    int input = 0;
    /* return NULL if we can't get at least one node put into the list */
    if(!head || !file || !fscanf(file, "%d ", &input)){
        return NULL;
    }
    /* make node for first input */ 
    head->data = input;
    /* scan in the rest of the list */
    while(fscanf(file, "%d", &input) != EOF){
        end = add_node(end, input);
    }
    return head;
}

/* print the data held by a list from node head */
void list_print(node* head){
    while(head != NULL){
        printf("%d ", head->data);
        head = head->next;
    }
    printf("\n");
}

/*array->list, but not free array */
node* array_to_list(int *ar, int length){
  if(length == 0){return NULL;}
  int x;
  node *tail, *head = malloc(sizeof(node));
  head->data = ar[0];
  tail = head;
  for(x = 1; x < length; x++){
    tail = add_node(tail, ar[x]);
  }
  return head;
}

/* list->array, but not free list, sets length */
int* list_to_array(node* head, int* length){
    node *copy = head;
    for((*length) = 0; copy != NULL; copy = copy->next, (*length)++);
    int i, *ar = malloc(sizeof(int) * (*length));
    for(i = 0; head != NULL; head = head->next, i++){
        ar[i] = head->data;
    }
    return ar;
}


