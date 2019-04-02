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

#include "vm.h"
#include "gc.h"

extern int errno;

#ifndef DEBUG
#define DEBUG 0
#endif

#define NEXT_INSTR  goto *(labels[*pc])

static inline uint32_t uf1b(char *mpos){
    return (uint32_t)( *( (uint8_t *)mpos ) );
}

static inline uint32_t uf2b(char *mpos){
    return (uint32_t)( *( (uint16_t *)mpos ) );
}

static inline int32_t sf1b(char *mpos){
    return (int32_t)( *( (int8_t *)mpos ) );
}

static inline int32_t sf2b(char *mpos){
    return (int32_t)( *( (int16_t *)mpos ) );
}

static inline int32_t sf4b(char *mpos){
    return *( (int32_t *)mpos );
}

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

void runTests (void)
{
    VM *vm = CREATE_VM ();
    for (int i = 0; i < STACK_MAX; i++) {
        PUSH_INT (vm, i);
        if (i % 4 == 0) { VM_POP (vm); };
    }
    DESTROY_VM (vm);
}

int main (int argc, char *argv[])
{

    if (argc != 2) {
        runTests();
        return 0;
    }

    #if DEBUG
    printf ("Main...\n");
    #endif

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

    /* Defines how many bytes contain data for each opcode */
    static const int op_bytes[] = {
        0, 2, 2, 1, 1, 0, 4, 2, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0
    };
    
    /* Create VM and more ... */
    VM *vm = CREATE_VM();
    #if DEBUG
    printf ("VM created...\n");
    #endif
    unsigned char code[STACK_MAX];
    memset (code, 0, STACK_MAX);

    unsigned int len = 0;
    char *pc = NULL;

    /* Parse source-code */
    errno = 0;
    FILE *f = fopen (argv[1], "rb");
    if ( f == NULL || errno != 0 ) {
        perror ("Unable to open file...");
        exit (EXIT_FAILURE);
    }
    #if DEBUG
    printf ("File opened...\n");
    #endif

    len = 0;
    while(!feof(f))
        len += fread(code, sizeof(char), STACK_MAX, f);
    fclose(f);

    #if DEBUG
    printf ("Parsing done...\n");
    #endif

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
    printf ("JMP\n");
    #endif
    pc = &code[uf2b(pc+1)];
    NEXT_INSTR;
}

/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ\n");
    #endif
    int a = objectValue( VM_POP(vm) );
    if ( a != 0 ) { pc = &code[uf2b(pc + 1)]; } 
    else { pc += 3; }
    NEXT_INSTR;
}

/* TODO: DUPLICATES ELEMENT */
L_DUP:
{
    #if DEBUG
    printf ("DUP\n");
    #endif
    int offset = vm->size - uf1b(pc + 1) - 1;
    if (offset < 0) { 
        printf ("Warning: Negative offset");
        offset = 0; 
    }
    Object *obj = vm->stack[offset];
    VM_PUSH (vm, obj);
    pc += 2;
    NEXT_INSTR;
}

/* TODO: SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP\n");
    #endif
    int offset = vm->size - uf1b(pc + 1) - 1;
    if (offset < 0) { 
        printf ("Warning: Negative offset");
        offset = 0; 
    }
    Object *obj = vm->stack[offset];
    Object *tmp = vm->stack[vm->size];
    vm->stack[vm->size] = obj;
    obj = tmp;
    pc += 2;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    #if DEBUG
    printf ("DROP\n");
    #endif
    VM_POP (vm);
    pc += 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    #if DEBUG
    printf ("PUSH4\n");
    #endif
    int element32 = sf4b(pc + 1);
    PUSH_INT (vm, element32);
    pc += 5;
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    #if DEBUG
    printf ("PUSH2\n");
    #endif
    /* TODO */
    int element16 = sf2b(pc + 1);
    PUSH_INT (vm, element16);
    pc += 3;
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH\n");
    #endif
    int element8 = sf1b(pc + 1);
    PUSH_INT (vm, element8);
    pc += 2;
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    #if DEBUG
    printf ("ADD\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a + b);
    pc += 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    #if DEBUG
    printf ("SUB\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    
    #if DEBUG
    printf ("a = %d , b = %d\n", a, b);
    #endif

    PUSH_INT (vm, a - b);
    pc += 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    #if DEBUG
    printf ("MUL\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a * b);
    pc += 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    #if DEBUG
    printf ("DIV\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a / b);
    pc += 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    #if DEBUG
    printf ("MOD\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a % b);
    pc += 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    #if DEBUG
    printf ("EQ\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a == b);
    pc += 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    #if DEBUG
    printf ("NE\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a != b);
    pc += 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    #if DEBUG
    printf ("LT\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a < b);
    pc += 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    #if DEBUG
    printf ("GT\n");
    #endif    
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a > b);
    pc += 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    #if DEBUG
    printf ("LE\n");
    #endif
    int b = objectValue( VM_POP(vm) );
    int a = objectValue( VM_POP(vm) );
    PUSH_INT (vm, a <= b);
    pc += 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    #if DEBUG
    printf ("GE\n");
    #endif
    int b = objectValue ( VM_POP(vm) );
    int a = objectValue ( VM_POP(vm) );
    PUSH_INT (vm, a >= b);
    pc += 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    #if DEBUG
    printf ("NOT\n");
    #endif
    int a = objectValue ( VM_POP(vm) );
    PUSH_INT (vm, !a);
    pc += 1;
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    #if DEBUG
    printf ("AND\n");
    #endif
    int b = objectValue ( VM_POP(vm) );
    int a = objectValue ( VM_POP(vm) );
    PUSH_INT (vm, a && b);
    pc += 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    #if DEBUG
    printf ("OR\n");
    #endif
    int b = objectValue ( VM_POP(vm) );
    int a = objectValue ( VM_POP(vm) );
    PUSH_INT (vm, a || b);
    pc += 1;
    NEXT_INSTR;
}

/* INPUT */
L_IN:   
{
    #if DEBUG
    printf ("INPUT\n");
    #endif
    int c;
    if ( fscanf(stdin, "%d", &c) != 1 ) {
        fprintf (stderr, "Unable to read from stdin\n");
        return EXIT_FAILURE;
    }
    PUSH_INT (vm, c);
    pc += 1;
    NEXT_INSTR;
}

/* OUTPUT */
L_OUT:
{
    #if DEBUG
    printf ("OUTPUT\n");
    #endif
    int c = objectValue( VM_POP(vm) );
    fprintf (stdout, "%c", (char)c);
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

/*  CLOCK */
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

L_CONS:
{
    #ifdef __DEBUG__
        printf("CONS\n");
    #endif    
    PUSH_PAIR (vm);
}

L_HD:
{
    #ifdef __DEBUG__
        printf("HD\n");
    #endif    
    Object *obj = VM_POP (vm);
    int val = objectValue (obj->head);
    PUSH_INT (vm, val);
}

L_TL:
{
    #ifdef __DEBUG__
        printf("TL\n");
    #endif    
    Object *obj = VM_POP (vm);
    VM_PUSH (vm, obj->tail);
}

L_CLEANUP:
{
    /* VM ENDED INTERPRETING */
    clock_t vm_end = clock();
    printf ("Interpreted in %lf\n", ((double)(vm_end - vm_start) / CLOCKS_PER_SEC));

    DESTROY_VM(vm);
}

    #if DEBUG
    printf ("Done...\n");
    #endif
    return 0;
}