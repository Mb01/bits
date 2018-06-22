/* list.h a linked list implementation */
#ifndef LIST_H_
#define LIST_H_

typedef struct node{
    int data;
    struct node *next;
}node;

node* add_node(node* end, int data);

node* filetolist(const char* filename);

void list_print(node* head);

int* list_to_array(node* head, int* length);

int list_free(node* head);

#endif
