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
#define NEXT_INSTR  goto *(code[vm.pc])

#define TEST printf("I AM HERE %d\n", __LINE__)

#define STACK_MAX (1 << 16)     /* 64 KB */
#define CODE_MAX  (1 << 16)     /* 64 KB */

/* Data Structures */

/* ARRAY CODE SECTION */
typedef struct {
    uint32_t  pc;        /* VM's PROGRAM COUNTER */
} VM;

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
static inline void STACK_PUSH(int32_t *, uint16_t *, int32_t);      /* MODIFIES ERRNO */
static inline int32_t STACK_POP(int32_t *, uint16_t *);             /* MODIFIES ERRNO */
static inline int32_t STACK_PEEK(int32_t *, uint16_t);            /* MODIFIES ERRNO */
static inline uint8_t  get_byte(uint64_t[], uint16_t);
static inline uint16_t get_2bytes(uint64_t[], uint16_t);
static inline uint32_t get_4bytes(uint64_t[], uint16_t);
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
    vm.pc = 0;

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
    
    static uint64_t code[CODE_MAX] = {0};
    static uint16_t len = 0;    /* 16 bits */
    uint8_t byte, op_byte;
    
    while( fscanf(file, "%c", &byte) == 1 ) { // WHILE NOT EOF OR ERROR

        /* GET BYTECODE */
        code[len] = (uint64_t)labels[byte];
        len += 1;
        
        /* GET THE DATA */
        for (int ix = 0; ix < op_bytes[byte]; ix++) {
            if ( fscanf(file, "%c", &op_byte) != 1 ) { return EXIT_FAILURE; }
            code[len] = op_byte;
            len += 1;
        }
    }
    fclose (file);

    /* Initialize Stack */
    static int32_t stack[STACK_MAX] = {0};
    static uint16_t stackTop = 0;

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
    vm.pc = (uint32_t)get_2bytes(code, vm.pc);

    NEXT_INSTR;
}
/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ\n");
    #endif
    if ( stack[stackTop] ) {
        vm.pc = (uint32_t)get_2bytes(code, vm.pc);
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

    uint32_t offset = (uint32_t)get_byte(code, vm.pc);
    uint32_t loc = stackTop - offset;
    if (offset > stackTop) { loc = 0; }
    int32_t  element = stack[loc];
    STACK_PUSH(stack, &stackTop, element);
    NEXT_INSTR;

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP\n");
    #endif
    vm.pc += 2;

    uint32_t offset = (uint32_t)get_byte(code, vm.pc);
    uint32_t loc = stackTop - offset;
    if (offset > stackTop) { loc = 0; }
    int32_t element = stack[loc];
    int32_t temp = stack[stackTop];
    stack[loc] = temp;
    stack[stackTop] = element;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    #if DEBUG
    printf ("DROP\n");
    #endif
    vm.pc += 1;

    stackTop -= 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    #if DEBUG
    printf ("PUSH4\n");
    #endif
    vm.pc += 5;

    int32_t element32 = (uint32_t)get_4bytes(code, vm.pc);
    STACK_PUSH(stack, &stackTop, element32);
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    #if DEBUG
    printf ("PUSH2\n");
    #endif
    vm.pc += 3;

    int16_t element16 = (int16_t)get_2bytes(code, vm.pc);
    STACK_PUSH(stack, &stackTop, (int32_t)element16);
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH\n");
    #endif
    vm.pc += 2;

    int8_t element8 = (int8_t)get_byte(code, vm.pc);
    STACK_PUSH(stack, &stackTop, (int32_t)element8);
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    #if DEBUG
    printf ("ADD\n");
    #endif
    vm.pc += 1;
    
    stack[stackTop - 1] += stack[stackTop];
    stackTop -= 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    #if DEBUG
    printf ("SUB\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] -= stack[stackTop];
    stackTop -= 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    #if DEBUG
    printf ("MUL\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] *= stack[stackTop];
    stackTop -= 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    #if DEBUG
    printf ("DIV\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] /= stack[stackTop];
    stackTop -= 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    #if DEBUG
    printf ("MOD\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] %= stack[stackTop];
    stackTop -= 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    #if DEBUG
    printf ("EQ\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] == stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    #if DEBUG
    printf ("NE\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] != stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    #if DEBUG
    printf ("LT\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] > stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    #if DEBUG
    printf ("GT\n");
    #endif    
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] < stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    #if DEBUG
    printf ("LE\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] >= stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    #if DEBUG
    printf ("GE\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] <= stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    #if DEBUG
    printf ("NOT\n");
    #endif
    vm.pc += 1;

    stack[stackTop] = !(stack[stackTop]);
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    #if DEBUG
    printf ("AND\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] && stack[stackTop - 1] );
    stackTop -= 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    #if DEBUG
    printf ("OR\n");
    #endif
    vm.pc += 1;

    stack[stackTop - 1] = ( stack[stackTop] || stack[stackTop - 1] );
    stackTop -= 1;
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
    STACK_PUSH(stack, &stackTop, (int32_t)c);
    NEXT_INSTR;
}

/* */
L_OUT:
{
    #if DEBUG
    printf ("OUTPUT\n");
    #endif
    vm.pc += 1;

    int32_t c = STACK_POP(stack, &stackTop);
    fprintf (stdout, "%c\n", c);
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
		if (stackTop <= 0) printf("stack[-1] = ...\n");
		else {
			for (int32_t j = stackTop; j >= 0; j--){
				printf("stack[%d] = %d\n", j, stack[j]);
			}
		}
	#endif
}

    return 0;
}

// --------------------------[STACK]--------------------------------

/**
 *  Push Element into STACK
 */
static inline void STACK_PUSH(int32_t *stack, uint16_t *stackTop, int32_t value)
{
    errno = 0;
    if (*stackTop == STACK_MAX) {
        fprintf(stderr, "STACK FULL :(\n");
        errno = EPERM;
        return;
    }
    stack[*stackTop] = value;
    *stackTop += 1;
}

/** 
 * Pop the top element from STACK
 */
static inline int32_t STACK_POP(int32_t *stack, uint16_t *stackTop)
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
static inline int32_t STACK_PEEK(int32_t *stack, uint16_t stackTop)
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

/**
 * Helper functions to retrieve group of bytes
*/
static inline uint8_t get_byte(uint64_t code[], uint16_t pc)
{
    return (uint8_t)(code[pc + 1] & 0xFF);
}

static inline uint16_t get_2bytes(uint64_t code[], uint16_t pc)
{
    return (uint16_t)((code[pc + 2] & 0xFF) << 8 | 
                      (code[pc + 1] & 0xFF));
}

static inline uint32_t get_4bytes(uint64_t code[], uint16_t pc)
{
    return (uint32_t)(((code[pc + 4] & 0xFF) << 24) | 
                      ((code[pc + 3] & 0xFF) << 16) | 
                      ((code[pc + 2] & 0xFF) << 8)  |
                      (code[pc + 1] & 0xFF));
}