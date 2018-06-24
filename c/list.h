/* list.h a linked list implementation */
#ifndef LIST_H_
#define LIST_H_

typedef struct node{
    int data;
    struct node *next;
}node;

void list_free(node* head);

node* add_node(node* end, int data);

node* filetolist(const char* filename);

void list_print(node* head);

node* array_to_list(int *ar, int length);

int* list_to_array(node* head, int* length);

#endif
