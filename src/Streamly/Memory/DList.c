// A complete working C program to demonstrate all insertion methods 
#include <stdio.h> 
#include <stdlib.h> 

struct DList {
  struct Node* head;
  struct Node* tail;
};

// A linked list node 
struct Node { 
  int data; 
  struct Node* next; 
  struct Node* prev; 
};

struct DList* dlist = NULL;

void clean () {
  struct Node* ptr = dlist->head;
  struct Node* ptr0;
  dlist->head = NULL;
  dlist->tail = NULL;
  if (ptr == NULL) {
    return;
  }
  while (ptr != NULL) {
    ptr0 = ptr;
    ptr = ptr->next;
    free (ptr0);
  }
  return;
}

int singleton (int val) {
  struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
  new_node->data = val;
  new_node->next = NULL;
  new_node->prev = NULL;
  if (dlist == NULL) {
    struct DList* dlist0 = (struct DList*)malloc(sizeof(struct DList));
    dlist = dlist0;
  }
  clean (dlist);
  dlist->head = new_node;
  dlist->tail = new_node;
  return 0;
}

int cons (int val) {
  struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
  new_node->data = val;
  new_node->next = dlist->head;
  new_node->prev = NULL;
  dlist->head->prev = new_node;
  dlist->head = new_node;
  return 0;
}

int snoc (int val) {
  struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
  new_node->data = val;
  new_node->next = NULL;
  new_node->prev = dlist->tail;
  dlist->tail->next = new_node;
  dlist->tail = new_node;
  return 0;
}

void print_dlist () {
  struct Node* ptr = dlist->head;
  if (ptr == NULL) {
    return;
  }
  while (ptr != NULL) {
    printf ("%d ", ptr->data);
    ptr = ptr->next;
  }
  return;
}

int uncons () {
  
}

void main () {
  singleton(3);
  cons(10);
  snoc(15);
  print_dlist();
  clean();
  print_dlist();
}

