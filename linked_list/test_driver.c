#include "linkedlist.h"

int main(int argc, char** argv){
    node* list = filetolist(argv[1]);
    list = list_merge_sort(list);
    list_print(list);
}
