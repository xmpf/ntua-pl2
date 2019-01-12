/**
 *  Author: Michalis Papadopoullos (03114702)
 *  Homework #5: VM Interpreter with Garbage Collection
 * 
 *  30-bit signed numbers -> int32_t
 *  cons-cells -> dynamic memory allocation
 * 
 *  References: 
 *  [1] https://github.com/munificent/mark-sweep/
 *  [2] https://youtu.be/74s0m4YoHgM
 */

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <assert.h>

#define NEXT_INSTR goto *(vm->code[vm->pc])

/* OPCODES */
typedef enum {
    OP_HALT = 0x00,     /* HALT MACHINE */
    OP_JMP,             /* UNCONDITIONAL JUMP */
    OP_JNZ,             /* JUMP IF NOT ZERO */
    OP_DUP,             /* DUPLICATES ELEMENT */
    OP_SWAP,            /* SWAP TOP ELEMENT WITH iTH ELEMENT */
    OP_DROP,            /* REMOVE AND IGNORE STACK[TOP] */
    OP_PUSH4,           /* PUSH 4 BYTES */
    OP_PUSH2,           /* PUSH 2 BYTES */
    OP_PUSH1,           /* PUSH 1 BYTE */
    OP_ADD,             /* ADDITION */
    OP_SUB,             /* SUBTRACTION */
    OP_MUL,             /* MULTIPLICATION */
    OP_DIV,             /* INTEGER DIVISION */
    OP_MOD,             /* MODULUS */
    OP_EQ,              /* EQUAL TO */
    OP_NE,              /* NOT EQUAL TO */
    OP_LT,              /* LESS THAN */
    OP_GT,              /* GREATER THAN */
    OP_LE,              /* LESS OR EQUAL TO */
    OP_GE,              /* GREATER OR EQUAL TO */
    OP_NOT,             /* LOGICAL NOT */
    OP_AND,             /* LOGICAL AND */
    OP_OR,              /* LOGICAL OR */
    OP_IN,              /* INPUT */
    OP_OUT,             /* OUTPUT */
    OP_CLOCK = 0x2A,    /* CLOCK */
    OP_CONS = 0x30,     /* CONS */
    OP_HD,              /* HEAD */
    OP_TL               /* TAIL */
} OPCODE;
/******************/

typedef enum {
    HEAP_INT,
    HEAP_CELL
} hObjType;

typedef struct heap_object_t {
    // type of object in heap
    hObjType    type;
    // mark bit
    bool        mark;

    // next object in heap
    struct heap_object_t *next;

    // type punning
    union {
        // HEAP_INT
        int val;

        // HEAP_CELL
        struct {
            struct heap_object_t *hd;
            struct heap_object_t *tl;
        };
    };

} hObject;

/* VM STRUCTURE */
#define STACK_MAX (1 << 16)
typedef struct vm_instance_t {
    // array of pointers to hObject structs
    hObject *stack[STACK_MAX];
    // stack size
    size_t  size;
    // root object
    hObject *fst;
    // total allocated objects
    size_t  total;
    // trigger GC
    size_t  bound;

    // program counter
    uint32_t pc;
    // pointer to code section
    uint64_t *code;
} VM;

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
    vm->fst = NULL;
    vm->total = 0;
    vm->bound = 100;

    // program counter
    vm->pc = 0;

    // return address of VM structure
    return vm;
}

void VM_STACK_PUSH (VM *vm, hObject *obj)
{
    assert (vm->size < STACK_MAX);
    vm->stack[vm->size] = obj;
    vm->size += 1;
}

hObject *VM_STACK_POP (VM *vm)
{
    vm->size -= 1;
    assert (vm->size >= 0);
    return vm->stack[vm->size];
}

void MARK (hObject *obj)
{
    // if marked return
    if ( obj->mark ) { return; }
    // else mark it
    obj->mark = 1;
    // if CELL mark HEAD and TAIL too
    if ( obj->type == HEAP_CELL ) {
        MARK (obj->hd);
        MARK (obj->tl);
    }
}

void MARK_ALL (VM *vm)
{
    static size_t i = 0;
    for (i = 0; i < vm->size; i++) {
        MARK (vm->stack[i]);
    }
}

void SWEEP (VM *vm)
{
    hObject **obj = &(vm->fst);
    while ( *obj != NULL ) {
        if ( (*obj)->mark == false ) {
            // Unreachable object => remove
            hObject *unreachable = *obj;
            *obj = unreachable->next;
            free (unreachable);
            unreachable = NULL;
            vm->total -= 1;
        } else {
            // Unmark it and continue
            (*obj)->mark = false;
            obj = &(*obj)->next;
        }
    }
}

void RUN_GC (VM *vm)
{
    size_t total = vm->total;
    MARK_ALL (vm);
    SWEEP (vm);
    // Inrease capacity
    vm->bound = 4 * vm->total;
}

hObject *newObject (VM *vm, hObjType type)
{
    // create an object with type
    if (vm->total == vm->bound) { RUN_GC (vm); }

    errno = 0;
    hObject *obj = (hObject *)malloc(1 * sizeof (hObject));
    if ( obj == NULL ) {
        perror ("Unable to create object");
        exit (EXIT_FAILURE);
    }
    obj->type = type;
    obj->next = vm->fst;
    vm->fst = obj;
    obj->mark = false;
    vm->total += 1;

    return obj;
}

void VM_STACK_PUSH_INT (VM *vm, int value)
{
    hObject *obj = newObject (vm, HEAP_INT);
    obj->val = value;
    VM_STACK_PUSH (vm, obj);
}

hObject* VM_STACK_PUSH_CELL (VM *vm)
{
    hObject *obj = newObject (vm, HEAP_CELL);
    obj->tl = VM_STACK_POP (vm);
    obj->hd = VM_STACK_POP (vm);

    VM_STACK_PUSH (vm, obj);
    return obj;
}

void DESTROY_VM (VM *vm)
{
    vm->size = 0;
    RUN_GC (vm);
    free (vm);
    vm = NULL;
}

void STACK_PUSH (int64_t *stack, int32_t esp, int64_t elem)
{
    assert (esp < STACK_MAX);
    stack[++esp] = elem;
}

int64_t STACK_POP (int64_t *stack, int32_t esp)
{
    assert (esp > 0);
    return stack[esp--];
}

/* Function Prototypes */
static inline uint8_t  get_byte(VM *);
static inline uint16_t get_2bytes(VM *);
static inline uint32_t get_4bytes(VM *);
/***********************/

void usageMsg (char *vmExecName)
{
    printf("Usage: %s <file to run> \n", vmExecName);
    printf("Example: ./vm example.b\n\n");
}

int main (int argc, char *argv[])
{

    if (argc != 2) {
        usageMsg (argv[0]);
        exit (EXIT_FAILURE);
    }

    /** You can get the address of a label defined in the current function 
     *  (or a containing function) with the unary operator ‘&&’. The value has type void *.
     *  This value is a constant and can be used wherever a constant of that type is valid.  */
    static const void *labels[] = {
        &&L_HALT, /* 0x00 */
        &&L_JMP, &&L_JNZ, /* 0x01, 0x02 */
        &&L_DUP, &&L_SWAP, /* 0x03, 0x04 */
        &&L_DROP, &&L_PUSH4, &&L_PUSH2, &&L_PUSH, /* 0x05 .. 0x08 */
        &&L_ADD, &&L_SUB, &&L_MUL, &&L_DIV, &&L_MOD, /* 0x09 .. 0x0D */
        &&L_EQ, &&L_NE, &&L_LT, &&L_GT, &&L_LE, &&L_GE, /* 0x0E .. 0x13 */
        &&L_NOT, &&L_AND, &&L_OR, /* 0x14 .. 0x16 */
        &&L_IN, &&L_OUT, /* 0x17, 0x18 */
        &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, /* 0x19 .. 0x1C */
        &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, /* 0x1D .. 0x20 */
        &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, /* 0x21 .. 0x24 */
        &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, /* 0x25 .. 0x28 */
        &&L_UNDEFINED, /* 0x29 */
        &&L_CLOCK, /* 0x2A */
        &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, /* 0x2B .. 0x2F */
        &&L_CONS, &&L_HD, &&L_TL  /* 0x30 .. 0x32 */
    };

    /**
     * Defines how many bytes contain data for each opcode
     */
    static const uint8_t op_bytes[] = {
        0, 2, 2, 1, 1, 0, 4, 2, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0
    };
    
    /**
     *  Create VM and more ...
     **/
    VM *vm = CREATE_VM ();
    uint64_t text[1 << 16];     /* source code */
    
    int64_t  stack[1 << 16];    /* stack */
    register int32_t esp;       /* stack top index */

    vm->code = text;
    static uint16_t len = 0;
    uint8_t byte, op_byte;
    uint8_t opcode;

    /**
     *  Parse source-code
     */
    errno = 0;
    FILE *code = fopen (argv[1], "rb");
    if ( code == NULL || errno != 0 ) {
        perror ("Unable to open file...");
        exit (EXIT_FAILURE);
    }
    while( fscanf(code, "%c", &opcode) == 1 ) { // WHILE NOT EOF OR ERROR
        vm->code[len] = (uint64_t)labels[opcode];
        len += 1;
        
        /* GET THE DATA */
        for (byte = 0; byte < op_bytes[opcode]; byte++) {
            if ( fscanf(code, "%c", &op_byte) != 1 ) { return EXIT_FAILURE; }
            vm->code[len] = op_byte;
            len += 1;
        }
    }

/* VM STARTED INTERPRETING BYTECODE */
    clock_t vm_start = clock();

// --------------------------[INTERPRETER]--------------------------------
    NEXT_INSTR;     /* SEGMENTATION FAULT */
                    /* OUT OF BOUNDS MEMORY ACCESS */

/* HALT MACHINE */
L_HALT:
{
    #if DEBUG
    printf ("HALT\n");
    #endif
    goto L_CLEANUP;
}
/* UNCONDITIONAL JUMP */
L_JMP:
{
    #if DEBUG
    printf ("JMP\n");
    #endif
    vm->pc = (uint32_t)get_2bytes(vm);

    NEXT_INSTR;
}
/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ\n");
    #endif
    if ( stack[esp] ) {
        vm->pc = (uint32_t)get_2bytes(vm);
    } else { 
        vm->pc += 3;
    }
    NEXT_INSTR;
}
/* DUPLICATES ELEMENT */
L_DUP:
    #if DEBUG
    printf ("DUP\n");
    #endif
    vm->pc += 2;

    uint32_t offset = (uint32_t)get_byte(vm);
    uint32_t loc = esp - offset;
    if (offset > esp) { loc = 0; }
    int32_t  element = stack[loc];
    STACK_PUSH(stack, esp, element);
    NEXT_INSTR;

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP\n");
    #endif
    vm->pc += 2;

    uint32_t offset = (uint32_t)get_byte(vm);
    uint32_t loc = esp - offset;
    if (offset > esp) { loc = 0; }
    int32_t element = stack[loc];
    int32_t temp = stack[esp];
    stack[loc] = temp;
    stack[esp] = element;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    #if DEBUG
    printf ("DROP\n");
    #endif
    vm->pc += 1;

    esp -= 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    #if DEBUG
    printf ("PUSH4\n");
    #endif
    vm->pc += 5;

    int32_t element32 = (int32_t)get_4bytes(vm);
    STACK_PUSH(stack, esp,  element32);
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    #if DEBUG
    printf ("PUSH2\n");
    #endif
    vm->pc += 3;

    int16_t element16 = (int16_t)get_2bytes(vm);
    STACK_PUSH(stack, esp,  (int32_t)element16);
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH\n");
    #endif
    vm->pc += 2;

    int8_t element8 = (int8_t)get_byte(vm);
    STACK_PUSH(stack, esp,  (int32_t)element8);
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    #if DEBUG
    printf ("ADD\n");
    #endif
    vm->pc += 1;
    
    stack[esp - 1] += stack[esp];
    esp -= 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    #if DEBUG
    printf ("SUB\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] -= stack[esp];
    esp -= 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    #if DEBUG
    printf ("MUL\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] *= stack[esp];
    esp -= 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    #if DEBUG
    printf ("DIV\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] /= stack[esp];
    esp -= 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    #if DEBUG
    printf ("MOD\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] %= stack[esp];
    esp -= 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    #if DEBUG
    printf ("EQ\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] == stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    #if DEBUG
    printf ("NE\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] != stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    #if DEBUG
    printf ("LT\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] > stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    #if DEBUG
    printf ("GT\n");
    #endif    
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] < stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    #if DEBUG
    printf ("LE\n");
    #endif
    vm->pc += 1    ;
    // current opcode
    stack[esp - 1] = ( stack[esp] >= stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    #if DEBUG
    printf ("GE\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] <= stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    #if DEBUG
    printf ("NOT\n");
    #endif
    vm->pc += 1;

    stack[esp] = !(stack[esp]);
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    #if DEBUG
    printf ("AND\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] && stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    #if DEBUG
    printf ("OR\n");
    #endif
    vm->pc += 1;

    stack[esp - 1] = ( stack[esp] || stack[esp - 1] );
    esp -= 1;
    NEXT_INSTR;
}

/* INPUT */
L_IN:   
{
    #if DEBUG
    printf ("INPUT\n");
    #endif
    vm->pc += 1;

    int c;
    if ( fscanf(stdin, "%d", &c) != 1 ) {
        fprintf (stderr, "Unable to read from stdin\n");
        return EXIT_FAILURE;
    }
    STACK_PUSH(stack, esp, (int32_t)c);
    NEXT_INSTR;
}

/* OUTPUT */
L_OUT:
{
    #if DEBUG
    printf ("OUTPUT\n");
    #endif
    vm->pc += 1;

    int32_t c = STACK_POP(stack, esp);
    fprintf (stdout, "%c", (char)c);
    NEXT_INSTR;
}

/* UNDEFINED */
L_UNDEFINED:
{
    #if DEBUG
    printf ("UNDEFINED\n");
    #endif
    goto L_CLEANUP;
}

/*  CLOCK */
L_CLOCK:
{
    #if DEBUG
    printf ("CLOCK\n");
    #endif
    vm->pc += 1;

    double time_spent = (double)( clock() - vm_start ) / CLOCKS_PER_SEC;
    printf("%.6lf\n", time_spent);
    NEXT_INSTR;
}

L_CONS:
{
    #ifdef __DEBUG__
        printf("CONS\n");
    #endif    

}

L_HD:
{
    #ifdef __DEBUG__
        printf("HD\n");
    #endif    
    

}

L_TL:
{
    #ifdef __DEBUG__
        printf("TL\n");
    #endif    


}

// -----------------------------------------------------------------

L_CLEANUP:
{
    /* VM ENDED INTERPRETING */
    clock_t vm_end = clock();
    printf ("Interpreted in %lf\n", ((double)(vm_end - vm_start) / CLOCKS_PER_SEC));

	#if DEBUG
		if (esp <= 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = esp; j >= 0; j--){
				printf("stack[%d] = %d\n", j, stack[j]);
			}
		}
	#endif

    DESTROY_VM(vm);
}


    return 0;
}

/**
 * Helper functions to retrieve group of bytes
*/
static inline uint8_t get_byte(VM *vm)
{
    return (uint32_t)(vm->code[vm->pc + 1]);
}

static inline uint16_t get_2bytes(VM *vm)
{
    return (uint16_t)((vm->code[vm->pc + 2]) << 8 | 
                      (vm->code[vm->pc + 1]));
}

static inline uint32_t get_4bytes(VM *vm)
{
    return (uint32_t)((vm->code[vm->pc + 4] << 24) | 
                      (vm->code[vm->pc + 3] << 16) | 
                      (vm->code[vm->pc + 2] << 8)  |
                      (vm->code[vm->pc + 1]));
}