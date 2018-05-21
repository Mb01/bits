#include <stdlib.h>
#include <stdio.h>
#include "linkedlist.h"

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

/* flatten but not free a list */
int* list_flatten(node* head, int* length){
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


/* returns pointer to head of sorted list */
/* it might be simpler to have a reference to left and set that to head at the end */
/* e.g. void list_merge(node** head){ left = *head; *head = NULL; ... continue in the same manner but don't return */
node* list_merge_sort(node* left){

    /* one element by itself is sorted */
    if(!left->next){
        return left;
    }
    node *middle = left, *end = left;
    /* find a suitable middle node at which to split */
    while(end){
        end = end->next;
        middle = middle->next;
        if(end){
            end = end->next;
        }
    }
    /* break the list up */
    node *right;
    if(middle->next != end){
        right = middle->next;
        middle->next = NULL;
    }else{
        right = left->next;
        left->next = NULL;
    }

    /* call recursively on halves */
    left = list_merge_sort(left);
    right = list_merge_sort(right);

    /* put back together in order */
    /* the first element of the list will change we need to track what that is and return it */
    node *head = NULL, *pos = NULL;

    while(left || right){
        /* check if we can use from left */
        node* use = NULL;
        if(!right || (left && left->data <= right->data)){
            use = left;
            left = left->next;
        }
        /* else use right as next */
        else{
            use = right;
            right = right->next;
        }
        /* the head is the first node used */
        if(!head){
            pos = head = use;
        }
        /* set the next node in the new list to use and move position to that */
            pos->next = use;
            pos = use;
    }
    return head;
}
