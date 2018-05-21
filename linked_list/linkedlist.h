/* list.h a linked list implementation */

typedef struct node{
    int data;
    struct node *next;
}node;

node* add_node(node* end, int data);

node* filetolist(const char* filename);

void list_print(node* head);

int* list_flatten(node* head, int* length);

int list_free(node* head);

node* list_merge_sort(node* left);

