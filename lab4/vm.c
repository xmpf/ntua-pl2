/**
 *  Author: Michalis Papadopoullos (03114702)
 *  Homework #4: VM Interpreter
 * 
 *  Resources:
 *  [1] https://youtu.be/DUNkdl0Jhgs
 *  [2] 
 *  
 *  Questions:
 *      + Better to have global VM, STACK structures instead of passing a pointer?
 *      + Dynamic memory allocation
 */

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <assert.h>

/* DEBUGGING OPTIONS */
#ifndef DEBUG
#define DEBUG 1
#endif

/* MACROS */
#define NEXT_INSTR  goto *(vm.code[vm.pc])

#define TEST printf("I AM HERE %d\n", __LINE__)

#define STACK_MAX (1 << 16)     /* 64 KB */
#define CODE_MAX  (1 << 16)     /* 64 KB */

/* Data Structures */

/* ARRAY CODE SECTION */
typedef struct {
    uint64_t  text[CODE_MAX];   /* CODE_MAX * 64 bits */ 
} TEXT;

typedef struct {
    uint32_t  pc;        /* VM's PROGRAM COUNTER */
    uint64_t *code;      /* BYTECODE PTR */
    uint8_t   opcode;    /* current opcode */
} VM;

/* ARRAY STACK */
typedef struct {
    int32_t   stack[STACK_MAX];  /* 2^16 * 32 bits */
    uint32_t  top;               /* stack pointer index */
} STACK;

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
    OP_UNDEFINED,       /* UNDEFINED */
    OP_CLOCK = 0x2A     /* CLOCK */
} OPCODE;
/******************/

/* Function Prototypes */
static inline void VM_INIT(VM *);                     /* ALLOCATE RESOURCES */
static inline void VM_DESTROY(VM *);                  /* FREE RESOURCES */
static inline void STACK_INIT(STACK *);               /* ALLOCATE MEMORY */
static inline void STACK_PUSH(STACK *, int32_t);      /* MODIFIES ERRNO */
static inline int32_t STACK_POP(STACK *);             /* MODIFIES ERRNO */
static inline int32_t STACK_PEEK(STACK *);            /* MODIFIES ERRNO */
static inline uint8_t  get_byte(VM *);
static inline uint16_t get_2bytes(VM *);
static inline uint32_t get_4bytes(VM *);
/***********************/

void usageMsg (char *vmExecName)
{
    printf("Usage: %s <file to run> \n", vmExecName);
    printf("Example: vm example.b\n\n");
}

int main (int argc, char *argv[])
{
    if (argc != 2) {
        usageMsg (argv[0]);
        exit (EXIT_FAILURE);
    }
    
    /* declare our VM */
    VM vm;
    vm.pc = 0;      /* PROGRAM COUNTER */
    vm.opcode = 0;  /* DEFAULT OPCODE: HALT */

    /* open file with bytecode */
    errno = 0;
    FILE *code = fopen(argv[1], "rb");
    if (code == NULL || errno != 0) {
        fprintf (stderr, "Unable to open file %s\n", argv[1]);
        fprintf(stderr, "%s\n", strerror(errno));
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
        &&L_CLOCK /* 0x2A */
    };

    /**
     * Defines how many bytes contain data for each opcode
     */
    static const uint8_t op_bytes[] = {
        0, 2, 2, 1, 1, 0, 4, 2, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0
    };

    /**
     *  Load bytecode into array
     */
    TEXT src;
    vm.code = src.text;         /* pointer to source code array */
    static uint16_t len = 0;    /* 16 bits */
    uint8_t byte, op_byte;
    while( fscanf(code, "%c", &vm.opcode) == 1 ) { // WHILE NOT EOF OR ERROR

        vm.code[len] = (uint64_t)labels[vm.opcode];
        len += 1;
        
        /* GET THE DATA */
        for (byte = 0; byte < op_bytes[vm.opcode]; byte++) {
            if ( fscanf(code, "%c", &op_byte) != 1 ) { return EXIT_FAILURE; }
            vm.code[len] = op_byte;
            len += 1;
        }
    }
    #if DEBUG
    for (uint16_t i = 0; i < len; i++) {
        printf ("0x%x: ", i);
        for (int j = 0; j < 4; j++) {
            printf ("%lx \t", vm.code[i*4 + j]);
        }
        printf ("\n");
    }
    #endif
    fclose (code);

    /* Initialize Stack */
    STACK stack;
    STACK_INIT(&stack);

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
    vm.pc = (uint32_t)get_2bytes(&vm);

    NEXT_INSTR;
}
/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ\n");
    #endif
    if ( stack.stack[stack.top] ) {
        vm.pc = (uint32_t)get_2bytes(&vm);
    } else { 
        vm.pc += 3;
    }
    NEXT_INSTR;
}
/* DUPLICATES ELEMENT */
L_DUP:
    #if DEBUG
    printf ("DUP\n");
    #endif
    vm.pc += 2;

    uint32_t offset = (uint32_t)get_byte(&vm);
    uint32_t loc = stack.top - offset;
    if (offset > stack.top) { loc = 0; }
    int32_t  element = stack.stack[loc];
    STACK_PUSH(&stack, element);
    NEXT_INSTR;

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP\n");
    #endif
    vm.pc += 2;

    uint32_t offset = (uint32_t)get_byte(&vm);
    uint32_t loc = stack.top - offset;
    if (offset > stack.top) { loc = 0; }
    int32_t element = stack.stack[loc];
    int32_t temp = stack.stack[stack.top];
    stack.stack[loc] = temp;
    stack.stack[stack.top] = element;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    #if DEBUG
    printf ("DROP\n");
    #endif
    vm.pc += 1;

    stack.top -= 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    #if DEBUG
    printf ("PUSH4\n");
    #endif
    vm.pc += 5;

    int32_t element32 = (uint32_t)get_4bytes(&vm);
    STACK_PUSH(&stack, element32);
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    #if DEBUG
    printf ("PUSH2\n");
    #endif
    vm.pc += 3;

    int16_t element16 = (int16_t)get_2bytes(&vm);
    STACK_PUSH(&stack, (int32_t)element16);
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH\n");
    #endif
    vm.pc += 2;

    int8_t element8 = (int8_t)get_byte(&vm);
    STACK_PUSH(&stack, (int32_t)element8);
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    #if DEBUG
    printf ("ADD\n");
    #endif
    vm.pc += 1;
    
    stack.stack[stack.top - 1] += stack.stack[stack.top];
    stack.top -= 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    #if DEBUG
    printf ("SUB\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] -= stack.stack[stack.top];
    stack.top -= 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    #if DEBUG
    printf ("MUL\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] *= stack.stack[stack.top];
    stack.top -= 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    #if DEBUG
    printf ("DIV\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] /= stack.stack[stack.top];
    stack.top -= 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    #if DEBUG
    printf ("MOD\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] %= stack.stack[stack.top];
    stack.top -= 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    #if DEBUG
    printf ("EQ\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] == stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    #if DEBUG
    printf ("NE\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] != stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    #if DEBUG
    printf ("LT\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] > stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    #if DEBUG
    printf ("GT\n");
    #endif    
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] < stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    #if DEBUG
    printf ("LE\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] >= stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    #if DEBUG
    printf ("GE\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] <= stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    #if DEBUG
    printf ("NOT\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top] = !(stack.stack[stack.top]);
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    #if DEBUG
    printf ("AND\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] && stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    #if DEBUG
    printf ("OR\n");
    #endif
    vm.pc += 1;

    stack.stack[stack.top - 1] = ( stack.stack[stack.top] || stack.stack[stack.top - 1] );
    stack.top -= 1;
    NEXT_INSTR;
}

/* */
L_IN:   
{
    #if DEBUG
    printf ("INPUT\n");
    #endif
    vm.pc += 1;

    int c;
    if ( fscanf(stdin, "%d", &c) != 1 ) {
        fprintf (stderr, "Unable to read from stdin\n");
        return EXIT_FAILURE;
    }
    STACK_PUSH(&stack, (int32_t)c);
    NEXT_INSTR;
}

/* */
L_OUT:
{
    #if DEBUG
    printf ("OUTPUT\n");
    #endif
    vm.pc += 1;

    int32_t c = STACK_POP(&stack);
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

/*  */
L_CLOCK:
{
    #if DEBUG
    printf ("CLOCK\n");
    #endif
    vm.pc += 1;

    double time_spent = (double)( clock() - vm_start ) / CLOCKS_PER_SEC;
    printf("%.6lf\n", time_spent);
    NEXT_INSTR;
}

// -----------------------------------------------------------------

L_CLEANUP:
{
    /* VM ENDED INTERPRETING */
    clock_t vm_end = clock();
    printf ("Interpreted in %lf\n", ((double)(vm_end - vm_start) / CLOCKS_PER_SEC));

	#if DEBUG
		if (stack.top <= 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = stack.top; j >= 0; j--){
				printf("stack[%d] = %d\n", j, stack.stack[j]);
			}
		}
	#endif

    VM_DESTROY(&vm);
}

    return 0;
}

// --------------------------[STACK]--------------------------------
/** 
 * Initialize STACK s
 * (in case of dynamic memory allocation)
 */   
static inline void STACK_INIT(STACK *s)
{
    s->top = 0;
    return;
}

/**
 *  Push Element into STACK s
 */
static inline void STACK_PUSH(STACK *s, int32_t v)
{
    errno = 0;
    if (s->top == STACK_MAX) {
        fprintf(stderr, "STACK FULL :(\n");
        errno = EPERM;
        return;
    }
    s->stack[s->top++] = v;
}

/** 
 * Pop the top element from STACK s
 */
static inline int32_t STACK_POP(STACK *s)
{
    errno = 0;
    if (s->top == 0) {
        fprintf(stderr, "STACK EMPTY :(\n");
        errno = EPERM;
        return EXIT_FAILURE;
    }
    return s->stack[--(s->top)];
}

/**
 *  Read the top element from STACK s
 */
static inline int32_t STACK_PEEK(STACK *s)
{
    errno = 0;
    if (s->top == 0) {
        fprintf(stderr, "STACK EMPTY :(\n");
        errno = EPERM;
        return EXIT_FAILURE;
    }
    return s->stack[s->top - 1];
}

/**
 *  ALLOCATE RESOURCES
 *  (in case of dynamic memory allocation)
 */
static inline void VM_INIT(VM *vm)
{
    vm->pc = 0;
    return;
}

/**
 *  RETURN RESOURCES TO OS
 *  (in case of dynamic memory allocation)
 */
static inline void VM_DESTROY(VM *vm)
{
    /* FREE RESOURCES */
    vm->code = NULL;
    vm->opcode = 0;
    vm->pc = -1;
    return;
}
// ----------------------------------------------------------

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