/**
 *  Author: Michalis Papadopoullos (03114702)
 *  Homework #4: VM Interpreter
 * 
 *  Resources:
 *  [1] https://youtu.be/DUNkdl0Jhgs
 *  
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
#define DEBUG (0)
#endif

#if DEBUG
#define TEST() ((fprintf(stderr, "DEBUG: LINE @ %d\n", __LINE__)))
#else
#define TEST()
#endif

/* MACROS */
#define NEXT_INSTR  goto *(labels[*pc])

/* CONSTANTS */
#define STACK_MAX (1 << 16)     /* 64 KB */
#define CODE_MAX  (1 << 16)     /* 64 KB */

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

static inline void STACK_PUSH(int32_t *stack, int32_t *stackTop, int32_t value);
static inline int32_t STACK_POP(int32_t *stack, int32_t *stackTop);
static inline int32_t STACK_PEEK(int32_t *stack, int32_t stackTop);

/******************/
static inline uint32_t uf1b(int8_t *mpos) {
    return (uint32_t)( *( (int8_t *) mpos ) );
}

static inline uint32_t uf2b(int8_t *mpos) {
    return (uint32_t)( *( (int16_t *) mpos ) );
}

static inline int32_t sf1b(int8_t *mpos) {
    return (int32_t)( *( (int8_t *) mpos ) );
}

static inline int32_t sf2b(int8_t *mpos) {
    return (int32_t)( *( (int16_t *) mpos ) );
}

static inline int32_t sf4b(int8_t *mpos) {
    return (int32_t)( *( (int32_t *) mpos ) );
}
/***********************/

void usageMsg (char *vmExecName)
{
    printf("Usage: %s <file to run> \n", vmExecName);
    printf("Example: vm example.b\n\n");
}

int main (int argc, char *argv[])
{
    /* Handle correct parameters */
    if (argc != 2) {
        usageMsg (argv[0]);
        exit (EXIT_FAILURE);
    }
    
    /* ACCESS BYTES */
    int8_t *pc = NULL;

    /* open file with bytecode */
    errno = 0;
    FILE *file = fopen(argv[1], "rb");
    if (file == NULL || errno != 0) {
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

    /* Defines how many bytes contain data for each opcode
    
    static const uint8_t op_bytes[] = {
        0, 2, 2, 1, 1, 0, 4, 2, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0
    };
    
    */

    /* Load bytecode */
    errno = 0;
    int8_t *code = (int8_t *)calloc(sizeof(int8_t), (int64_t)CODE_MAX * CODE_MAX);
    if (code == NULL || errno != 0) {
        TEST();
        fprintf(stderr, "%s", strerror(errno));
        exit(EXIT_FAILURE);
    }
    int32_t len = 0;
    while(!feof(file)) {
        len += fread(code, sizeof(int8_t), CODE_MAX - 1, file);
        if (len >= CODE_MAX - 1) { TEST(); exit(0); }
    }
    fclose(file);
    assert(len > 0);

    /* Initialize Stack */
    static int32_t stack[STACK_MAX] = {0};
    int32_t stackTop = 0;
    
    /* Start from beginning */
    pc = &code[0];

    /* VM STARTED INTERPRETING BYTECODE */
    clock_t vm_start = clock();

// --------------------------[INTERPRETER]--------------------------------
    NEXT_INSTR;

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
    printf ("JMP %x\n", code[  uf2b(pc + 1) ]);
    #endif
    pc =  &code[ uf2b(pc + 1) ];
    NEXT_INSTR;
}
/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ %x\n", code[ uf2b(pc + 1) ]);
    #endif
    if ( stack[stackTop - 1] != 0 ) {
        pc = &code[ uf2b(pc + 1) ];
    } else { 
        pc += 3;
    }
    NEXT_INSTR;
}
/* DUPLICATES ELEMENT */
L_DUP:
    #if DEBUG
    printf ("DUP %d\n", sf1b (pc + 1));
    #endif
    assert(true);
    int32_t offset = sf1b (pc + 1);
    int32_t loc = (offset >= stackTop) ? 0 : stackTop - offset - 1;
    int32_t  element = stack[loc];
    STACK_PUSH(stack, &stackTop, element);
    pc += 2;
    NEXT_INSTR;

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP %d\n", sf1b (pc + 1));
    #endif
    int32_t offset = sf1b (pc + 1);
    int32_t loc = stackTop - offset;
    if (offset > stackTop) { loc = 0; }
    int32_t element = stack[loc];
    int32_t temp = stack[stackTop - 1];
    stack[loc] = temp;
    stack[stackTop - 1] = element;
    pc += 2;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    #if DEBUG
    printf ("DROP\n");
    #endif
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    #if DEBUG
    printf ("PUSH4 %d\n", sf4b (pc + 1));
    #endif
    int32_t element32 = sf4b (pc + 1);
    STACK_PUSH(stack, &stackTop, element32);
    pc += 5;
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    #if DEBUG
    printf ("PUSH2 %d\n", sf2b(pc + 1));
    #endif
    int32_t element16 = sf2b(pc + 1);
    STACK_PUSH(stack, &stackTop, (int32_t)element16);
    pc += 3;
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH1 %d\n", sf1b (pc + 1));
    #endif
    int32_t element8 = sf1b (pc + 1);
    STACK_PUSH(stack, &stackTop, element8);
    pc += 2;
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    #if DEBUG
    printf ("ADD %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] += stack[stackTop - 1];
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    #if DEBUG
    printf ("SUB %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] -= stack[stackTop - 1];
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    #if DEBUG
    printf ("MUL %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] *= stack[stackTop - 1];
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    #if DEBUG
    printf ("DIV %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] /= stack[stackTop - 1];
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    #if DEBUG
    printf ("MOD %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = stack[stackTop - 2] % stack[stackTop - 1];
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    #if DEBUG
    printf ("EQ %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = ( stack[stackTop - 1] == stack[stackTop - 2] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    #if DEBUG
    printf ("NE %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = !( stack[stackTop - 1] == stack[stackTop - 2] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    #if DEBUG
    printf ("LT %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = ( stack[stackTop - 2] > stack[stackTop - 1] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    #if DEBUG
    printf ("GT %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif    
    stack[stackTop - 2] = ( stack[stackTop - 2] < stack[stackTop - 1] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    #if DEBUG
    printf ("LE %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = ( stack[stackTop - 2] >= stack[stackTop - 1] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    #if DEBUG
    printf ("GE %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    stack[stackTop - 2] = ( stack[stackTop - 2] <= stack[stackTop - 1] );
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    #if DEBUG
    printf ("NOT %d\n", stack[stackTop - 1]);
    #endif
    stack[stackTop - 1] = !(stack[stackTop - 1]);
    pc += 1;
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    #if DEBUG
    printf ("AND %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    if (stack[stackTop - 1] != 0 && stack[stackTop - 2] != 0)
        stack[stackTop - 2] = 1;
    else
        stack[stackTop - 2] = 0;
    stackTop -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    #if DEBUG
    printf ("OR %d %d\n", stack[stackTop - 1], stack[stackTop - 2]);
    #endif
    pc += 1;
    if (stack[stackTop - 1] == 0 && stack[stackTop - 2] == 0)
        stack[stackTop - 2] = 0;
    else
        stack[stackTop - 2] = 1;
    stackTop -= 1;
    NEXT_INSTR;
}

/* INPUT */
L_IN:   
{
    #if DEBUG
    printf ("INPUT\n");
    #endif
    int32_t c;
    if ( fscanf(stdin, "%d", &c) != 1 ) {
        fprintf (stderr, "Unable to read from stdin\n");
        return EXIT_FAILURE;
    }
    STACK_PUSH(stack, &stackTop, c);
    pc += 1;
    NEXT_INSTR;
}

/* OUTPUT */
L_OUT:
{
    #if DEBUG
    printf ("OUTPUT\n");
    #endif
    int32_t c = STACK_POP(stack, &stackTop);
    fprintf (stdout, "%c", c);
    pc += 1;
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
    double time_spent = (double)( clock() - vm_start ) / CLOCKS_PER_SEC;
    printf("%.6lf\n", time_spent);
    pc += 1;
    NEXT_INSTR;
}

// -----------------------------------------------------------------

L_CLEANUP:
{
    /* VM ENDED INTERPRETING */
    clock_t vm_end = clock();
    printf ("Interpreted in %lf\n", ((double)(vm_end - vm_start) / CLOCKS_PER_SEC));

	#if DEBUG
    for (int32_t j = stackTop - 1; j >= 0; j--) {
        printf("stack[%d] = %d\n", j, stack[j]);
    }
	#endif
}

    return 0;
}

// --------------------------[STACK]--------------------------------

/**
 *  Push Element into STACK
 */
static inline void STACK_PUSH(int32_t *stack, int32_t *stackTop, int32_t value)
{
    errno = 0;
    if (*stackTop + 1 == STACK_MAX) {
        fprintf(stderr, "STACK FULL :(\n");
        errno = EPERM;
        exit(EXIT_FAILURE);
    }
    stack[*stackTop] = value;
    *stackTop += 1;
}

/** 
 * Pop the top element from STACK
 */
static inline int32_t STACK_POP(int32_t *stack, int32_t *stackTop)
{
    errno = 0;
    if (*stackTop == 0) {
        fprintf(stderr, "STACK EMPTY :(\n");
        errno = EPERM;
        return EXIT_FAILURE;
    }
    *stackTop -= 1;
    return stack[*stackTop];
}

/**
 *  Read the top element from STACK
 */
static inline int32_t STACK_PEEK(int32_t *stack, int32_t stackTop)
{
    errno = 0;
    if (stackTop == 0) {
        fprintf(stderr, "STACK EMPTY :(\n");
        errno = EPERM;
        return EXIT_FAILURE;
    }
    return stack[stackTop - 1];
}
// ----------------------------------------------------------