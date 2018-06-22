* list.c */
/* a linked list implementation */

#include <stdlib.h>
#include <stdio.h>
#include "list.h"


/* append a new node to end */
node* add_node(node* end, int data){
    node *new_node = malloc(sizeof(node));
    if(!new_node){
        return new_node;
        /* we could try again ... forever */
        /* return add_node(end, data); */
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

/* list->array, but not free a list */
int* list_to_array(node* head, int* length){
    node *copy = head;
    for((*length) = 0; copy != NULL; copy = copy->next, (*length)++);
    int i, *ar = malloc(sizeof(int) * (*length));
    for(i = 0; head != NULL; head = head->next, i++){
        ar[i] = head->data;
    }
    return ar;
}

/* free a list */
int list_free(node* head){
    node* next = head;
    while(next != NULL){
        next = head->next;
        free(head);
    }
}
