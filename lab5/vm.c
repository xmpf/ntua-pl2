#include <errno.h>
#include "vm.h"

extern int errno;

// Return an instance of VM
VM *CREATE_VM (void)
{
    errno = 0;
    VM *vm = (VM *)malloc(1 * sizeof(VM));
    if  (vm == NULL ) { // if error abort
        perror ("Unable to create VM");
        exit (EXIT_FAILURE);
    }
    // initial values
    vm->size = 0;
    vm->root = NULL;
    vm->threshold = 8;
    vm->total = 0;
    // return address of VM structure
    return vm;
}

// Push element in stack
void VM_PUSH (VM *vm, Object *obj)
{
    if (vm->size >= STACK_MAX) {
        errno = ENOMEM;
        perror ("Stack Overflow!");
        exit (EXIT_FAILURE);
    }
    vm->stack[vm->size] = obj;
    vm->size += 1;
}

// Pop element from stack
Object *VM_POP (VM *vm)
{
    vm->size -= 1;
    if (vm->size < 0) {
        errno = ERANGE;
        perror("Stack underflow!");
        exit (EXIT_FAILURE);
    }
    return vm->stack[vm->size];
}

// Push OBJ_INT
void PUSH_INT (VM* vm, int val)
{
    Object *object = newObject (vm, OBJ_INT);
    object->value = val;
    VM_PUSH (vm, object);
}

// Push OBJ_PAIR
Object* PUSH_PAIR (VM* vm)
{
  Object* object = newObject(vm, OBJ_PAIR);
  object->tail = VM_POP (vm);
  object->head = VM_POP (vm);

  VM_PUSH (vm, object);
  return object;
}

// Destroy VM and free resources
void DESTROY_VM (VM *vm)
{
    vm->size = 0;
    RUN_GC (vm);
    free (vm);
    vm = NULL;
}

// Create an object in memory (HEAP)
Object *newObject (VM *vm, ObjectType type)
{
    errno = 0;
    Object *object = (Object *)malloc(sizeof(Object));
    if (object == NULL) {
        perror ("Unable to allocate memory for Object");
        exit (EXIT_FAILURE);
    }
    object->type = type;
    object->next = (Object *)vm->root;
    vm->root = (Object *)object;
    object->marked = false;
    vm->total += 1;

    return object;
}

// Object\'s value
int objectValue (Object *obj)
{
    if (obj->type == OBJ_INT) {
        return obj->value;
    } else {
        printf ("Warning: Object is of type \\OBJ_PAIR\\ => HEAD\n");
        objectValue (obj->head);
    }
    return -1;
}

// MARK ACTION
void MARK (Object *obj)
{
    // if marked return
    if ( obj->marked ) { return; }
    else { obj->marked = true; }

    // if CELL mark HEAD and TAIL too
    if ( obj->type == OBJ_PAIR ) {
        MARK (obj->head);
        MARK (obj->tail);
    }
}

// MARK PHASE
void MARK_ALL (VM *vm)
{
    int i = 0;
    for (i = 0; i < vm->size; i++) {
        MARK (vm->stack[i]);
    }
}

// SWEEP PHASE
void SWEEP (VM *vm)
{
    Object **obj = (Object **)&(vm->root);

    while ( *obj != NULL ) {
        if ( (*obj)->marked == false ) {
            // Unreachable object => remove
            Object *unreachable = *obj;
            *obj = unreachable->next;
            free (unreachable);
            unreachable = NULL;
            vm->total -= 1;
        } else {
            // Unmark it and continue
            (*obj)->marked = false;
            obj = (Object **)&((*obj)->next);
        }
    }
}

// Run Garbage Collector
void RUN_GC (VM *vm)
{
    int total = vm->total;
    MARK_ALL (vm);
    SWEEP (vm);

    // Inrease capacity
    vm->threshold = (vm->total << 1);
    printf ("Collected: %d\nTotal: %d\n", (total - vm->total), vm->total);
}