#ifndef __VM_H__
#define __VM_H__

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdbool.h>

#define STACK_MAX (1 << 16)

// Objects in the HEAP
typedef enum {
    OBJ_INT
  , OBJ_PAIR
} ObjectType;

// Tagged Union - Type Puning
typedef struct sObject {
    // type of object in heap
    ObjectType type;

    // mark bit
    bool marked;

    // Linked list
    struct sObject *next;

    // type punning
    union {
        // OBJ_INT
        int value;

        // OBJ_PAIR
        struct {
            struct sObject *head;
            struct sObject *tail;
        };
    };

} Object;

// VM State
typedef struct VM {
  Object *stack[STACK_MAX];
  unsigned int size;
  // root item
  Object *root;
  // Total items allocated in HEAP
  unsigned int total;
  // Trigger GC
  int threshold;
} VM;

// Return an instance of VM
VM *CREATE_VM (void);

// Push element in stack
void VM_PUSH (VM *vm, Object *obj);

// Pop element from stack
Object *VM_POP (VM *vm);

// Push OBJ_INT
void PUSH_INT (VM *vm, int val);

// Push OBJ_PAIR
Object *PUSH_PAIR (VM *vm);

// Destroy VM and free resources
void DESTROY_VM (VM *vm);

// Create an object in memory (HEAP)
Object *newObject (VM *vm, ObjectType type);

// Object\'s value
int objectValue (Object *obj);

// MARK ACTION
void MARK (Object *obj);

// MARK PHASE
void MARK_ALL (VM *vm);

// SWEEP PHASE
void SWEEP (VM *vm);

// Run Garbage Collector
void RUN_GC (VM *vm);

#endif