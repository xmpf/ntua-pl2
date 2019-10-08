/**
 *  Author: Michalis Papadopoullos (03114702)
 *  Homework #4: VM Interpreter
 *  Homework #5: VM Extension w/ Garbage Allocation
 *  
 *  For debugging messages compile with:
 *      g++ -Wall -DDEBUG=1 vm.cpp -o vm_debug
 *
 *  make lab5:
 *      g++ -Wall -DGC=1 vm.cpp -o vm_gc
 *
 *  Notes:
 *      Lab5(GC) is not implemented.
 *
 *  Resources:
 *  [1] https://youtu.be/DUNkdl0Jhgs  
 *
 */


#include <iostream>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <cstring>
#include <cerrno>
#include <climits>

/* DEBUGGING OPTIONS */
#ifndef DEBUG
#define DEBUG (0)
#endif

#if DEBUG
#define DBG(msg) ((fprintf(stderr, "DEBUG(%d): %s\n", __LINE__, (msg))))
#else
#define DBG(msg)
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

    /* GC EXTENSION */
  , OP_CONS = 0x30      /* CONS */
  , OP_HEAD             /* HEAD */
  , OP_TAIL             /* TAIL */
} OPCODE;

/* GC Extension Specific Code */

// Include Vector container
#include <vector>

/* Function Declarations */
void assert_msg(int, const char *);

// MSB => VALUE || POINTER
// BIT30 => MARKED || NOT_MARKED
#define BIT31 (1 << 31)
#define BIT30 (1 << 30)

// MAKE_ADDRESS => BIT31 = 1, BIT30 = 0
#define MAKE_ADDRESS(x) (((x) | BIT31) & ~(BIT30))

// CLEAR_FLAGS => BIT31 = BIT30 = 0
#define CLEAR_FLAGS(x) ((x) & (BIT30 - 1))

// IS_MARKED => CHECK IF BIT30 == 1
#define IS_VISITED(x) ((x) & BIT30)

// GET OBJ_TYPE BY LOOKING THE MSB
#define OBJ_TYPE(x) ((x) & BIT31)

// SET MSB => BIT31 = 1
#define SET_MSB(x)  ((x) | BIT31)

// Cell structure
typedef struct cell_t {
    bool is_atom;
    bool is_marked;

    cell_t * next;

    union {
        // ATOM
        int32_t data;

        // CELL
        struct {
            cell_t * head;
            cell_t * tail;
        };
    };

    // Constructors
    cell_t () {}

    // copy an object
    cell_t(cell_t * obj) {
        this->is_atom = obj->is_atom;
        this->is_marked = obj->is_marked;
        this->next = obj->next;
        if(obj->is_atom) { this->data = obj->data; }
        else {
            this->head = obj->head;
            this->tail = obj->tail;
        }
        
    }

    // create an atom object
    cell_t(int32_t data) {
        DBG("Constructor => atom");
        this->is_atom = true;
        this->is_marked = false;
        this->next = NULL;
        this->data = data;
    }

    // create a cell object
    cell_t(cell_t * a, cell_t * b) {
        DBG("Constructor => cell");
        this->is_atom = false;
        this->is_marked = false;
        this->next = NULL;
        this->head = a;
        this->tail = b;
    }

    // Destructors
    ~cell_t() {
    }

    // Getters
    int32_t get_data() {
        if( this->is_atom ) {
            return this->data;
        }
        fprintf(stderr, "cell_t::get_value() FAILED\n");
        exit(EXIT_FAILURE);
    }

    // Setters
    void set_data(int32_t v) {
        if( this->is_atom ) {
            this->data = v;
            return;
        }
        fprintf(stderr, "cell_t::set_value() FAILED\n");
        exit(EXIT_FAILURE);
    }

} cell_t;

// Type of stack to hold objects
typedef struct stack_t {
    cell_t * mem[STACK_MAX];
    cell_t * root;
    size_t   size;
    size_t   sp;
    size_t   allocated;
    size_t   threshold;

    // Constructors
    stack_t(size_t size) {
        for(size_t i = 0; i < size; i++) { 
            this->mem[i] = NULL;
        }
        this->root = NULL;
        this->size = size;
        this->sp = 0;
        this->allocated = 0;
        this->threshold = size - 2;
    }

    // Destructor
    ~stack_t() {
        if( this->mem == NULL ) { return; }
        for(size_t i = 0; i < this->size; i++) {
            delete this->mem[i];
            this->mem[i] = NULL;
        }
    }

    // Methods
    cell_t * pop() {
        assert_msg(this->sp > 0, "stack_t::pop() => Stack Empty!");
        this->sp -= 1;
        return this->mem[this->sp];
    }

    void push(cell_t * object) {

        if( this->allocated == this->threshold ) {
            DBG("==== Running GC... ====");
            this->mark_all();
            this->sweep();
        }

        assert_msg(this->sp < this->size - 1, "stack_t::push() => Stack Full!");
        this->mem[this->sp] = object;
        
        // GC
        
        object->next = this->root;
        this->root = object;
        
        this->sp += 1;
        this->allocated += 1;
        
        if( this->allocated == this->threshold ) {
            DBG("==== Running GC... ====");
            this->mark_all();
            this->sweep();
        }
    }

    void mark_all() {
        for(size_t i = 0; i < this->sp; i++) {
            this->mem[i]->is_marked = true;
        }
    }
    
    // https://github.com/munificent/mark-sweep
    void sweep() {
        cell_t ** object = &this->root;
        while (*object) {
            if ((*object)->is_marked == false) {
                /* This object wasn't reached, so remove it from the list and free it. */
                cell_t * unreached = *object;

                *object = unreached->next;
                delete unreached;
                unreached = NULL;

                this->allocated -= 1;

            } else {
                /* This object was reached, so unmark it (for the next GC) and move on to the next. */
                (*object)->is_marked = false;
                object = &(*object)->next;
            }
        }
    }

} stack_t;

// Garbage Collection

/* Inline Function Definitions */
static inline uint32_t uf1b(int8_t *mpos) {
    // unsigned int from byte
    return (uint32_t)( *( (int8_t *) mpos ) );
}

static inline uint32_t uf2b(int8_t *mpos) {
    // unsigned int from two bytes
    return (uint32_t)( *( (int16_t *) mpos ) );
}

static inline int32_t sf1b(int8_t *mpos) {
    // signed int from byte
    return (int32_t)( *( (int8_t *) mpos ) );
}

static inline int32_t sf2b(int8_t *mpos) {
    // signed int from byte
    return (int32_t)( *( (int16_t *) mpos ) );
}

static inline int32_t sf4b(int8_t *mpos) {
    // signed int from four bytes
    return (int32_t)( *( (int32_t *) mpos ) );
}

/* Assert with error message */
void assert_msg(int condition, const char* message) {
  if (!condition) {
    fprintf(stderr, "%s\n", message);
    exit(EXIT_FAILURE);
  }
}

/* Usage Helper Function */
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
        exit(EXIT_FAILURE);
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
    /* GC EXTENSION */
      , &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED /* 0x2B .. 0x2F */
      , &&L_CONS  /* 0x30 */
      , &&L_HEAD  /* 0x31 */
      , &&L_TAIL  /* 0x32 */
    };

    /* Load bytecode */
    errno = 0;
    int8_t *code = (int8_t *)calloc(sizeof(int8_t), (int64_t)CODE_MAX * CODE_MAX);
    if (code == NULL || errno != 0) {
        fprintf(stderr, "%s", strerror(errno));
        exit(EXIT_FAILURE);
    }
    int32_t len = 0;
    while(!feof(file)) {
        len += fread(code, sizeof(int8_t), CODE_MAX - 1, file);
        if (len >= CODE_MAX - 1) {
            fprintf(stderr, "CODE SIZE EXCEEDED!\n");
            exit(EXIT_FAILURE);
        }
    }
    fclose(file);
    assert_msg(len > 0, "Error while loading bytecode from file...");

    /* Initialize Stack */
    stack_t * stack = new stack_t(STACK_MAX);

    /* Start from beginning */
    pc = &code[0];

    /* VM STARTED INTERPRETING BYTECODE */
    clock_t vm_start = clock();

// --------------------------[INTERPRETER]--------------------------------
    NEXT_INSTR;

/* HALT MACHINE */
L_HALT:
{
    DBG("HALT");
    goto L_CLEANUP;
}

/* UNCONDITIONAL JUMP */
L_JMP:
{
    DBG("JMP");
    pc =  &code[ pc[1] | (pc[2] << 8) ];
    NEXT_INSTR;
}

/* JUMP IF NOT ZERO */
L_JNZ:
{
    DBG("JNZ");
    int32_t v = stack->pop()->get_data();
    if ( v != 0 ) {
        pc = &code[ pc[1] ];
    } else { 
        pc += 3;
    }
    NEXT_INSTR;
}

/* DUPLICATES ELEMENT */
L_DUP:
{
    DBG("DUP");
    int32_t offset = pc[1];

    // check offset range
    if( (size_t)offset >= stack->sp ) { offset = 0; }
    offset = stack->sp - offset - 1;
    cell_t * element = new cell_t( stack->mem[offset] );
    stack->push( element );
    pc += 2;
    NEXT_INSTR;
}

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    DBG("SWAP");
    int32_t offset = pc[1];

    // check offset range
    if( (size_t)offset >= stack->sp ) { offset = 0; }
    offset = stack->sp - offset - 1;
    cell_t * element = stack->mem[offset];
    cell_t * temp = stack->mem[stack->sp];
    stack->mem[offset] = temp;
    stack->mem[stack->sp] = element;
    pc += 2;
    NEXT_INSTR;
}

/* REMOVE AND IGNORE STACK[TOP] */
L_DROP:
{
    DBG("DROP");
    stack->sp -= 1;
    pc += 1;
    NEXT_INSTR;
}

/* PUSH 4 BYTES */
L_PUSH4:
{
    DBG("PUSH4");

    int32_t element32 = pc[1] | (pc[2] << 8) | (pc[3] << 16) | (pc[4] << 24);
    stack->push( new cell_t(element32) );
    pc += 5;
    NEXT_INSTR;
}

/* PUSH 2 BYTES */
L_PUSH2:
{
    DBG("PUSH2");

    int32_t element16 = pc[1] | (pc[2] << 8);
    stack->push( new cell_t(element16) );
    pc += 3;
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    DBG("PUSH");

    int32_t element8 = pc[1];
    stack->push( new cell_t(element8) );
    pc += 2;
    NEXT_INSTR;
}

/* ADD 2 ELEMENTS */
L_ADD:
{
    DBG("ADD");
    
    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    stack->push( new cell_t(a + b) );
    pc += 1;
    NEXT_INSTR;
}

/* SUBTRACT 2 ELEMENTS */
L_SUB:
{
    DBG("SUB");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    stack->push( new cell_t(a - b) );
    pc += 1;
    NEXT_INSTR;
}

/* MULTIPLY 2 ELEMENTS (A * B) */
L_MUL:  
{
    DBG("MUL");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    stack->push( new cell_t(a * b) );
    pc += 1;
    NEXT_INSTR;
}

/* INTEGER DIVISION OF 2 ELEMENTS (A / B) */
L_DIV:
{
    DBG("DIV");

    int32_t b = stack->pop()->get_data();
    assert_msg(b != 0, "L_DIV: Division by zero!");
    int32_t a = stack->pop()->get_data();
    stack->push(new cell_t(a / b) );
    pc += 1;
    NEXT_INSTR;
}

/* MODULO OF 2 ELEMENTS (A % B) */
L_MOD:  
{
    DBG("MOD");

    int32_t b = stack->pop()->get_data();
    assert_msg(b != 0, "L_MOD: Modulo by zero!");
    int32_t a = stack->pop()->get_data();
    stack->push( new cell_t(a % b) );
    pc += 1;
    NEXT_INSTR;
}

/* EQUALITY TEST (A == B) */
L_EQ:   
{
    DBG("EQ");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    if( a == b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );
    
    pc += 1;
    NEXT_INSTR;
}

/* NON-EQUALITY TEST (A != B) */
L_NE:   
{
    DBG("NE");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();   
    if( a != b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );
    
    pc += 1;
    NEXT_INSTR;
}

/* (A < B) */
L_LT:   
{
    DBG("LT");
    
    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();   
    if( a < b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );

    pc += 1;
    NEXT_INSTR;
}

/* (A > B) */
L_GT:   
{
    DBG("GT");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();   
    if( a > b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );

    pc += 1;
    NEXT_INSTR;
}

/* (A <= B) */
L_LE:   
{
    DBG("LE");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();   
    if( a <= b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );

    pc += 1;
    NEXT_INSTR;
}

/* (A >= B) */
L_GE:   
{
    DBG("GE");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();   
    if( a >= b ) 
        stack->push( new cell_t(1) );
    else
        stack->push( new cell_t(0) );

    pc += 1;
    NEXT_INSTR;
}

/* ( !A ) */
L_NOT:  
{
    DBG("NOT");

    int32_t result = stack->pop()->get_data();
    stack->push( new cell_t(!result) );
    pc += 1;
    NEXT_INSTR;
}

/* (A && B) */
L_AND:  
{
    DBG("AND");

    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    if (a != 0 && b != 0)
        stack->push(new cell_t(1));
    else
        stack->push(new cell_t(0));
    
    pc += 1;
    NEXT_INSTR;
}

/* (A || B) */
L_OR:   
{
    DBG("OR");
    
    int32_t b = stack->pop()->get_data();
    int32_t a = stack->pop()->get_data();
    if (a == 0 && b == 0)
        stack->push(new cell_t(0));
    else
        stack->push(new cell_t(1));
    
    pc += 1;
    NEXT_INSTR;
}

/* INPUT */
L_IN:   
{
    DBG("INPUT");

    int32_t c;
    if ( fscanf(stdin, "%d", &c) != 1 ) {
        fprintf (stderr, "Unable to read from stdin\n");
        return EXIT_FAILURE;
    }
    cell_t * cell = new cell_t(c);
    stack->push( cell );
    pc += 1;
    NEXT_INSTR;
}

/* OUTPUT */
L_OUT:
{
    DBG("OUTPUT");

    int32_t c = stack->pop()->get_data();
    fprintf (stdout, "%c", c);
    pc += 1;
    NEXT_INSTR;
}

/* UNDEFINED */
L_UNDEFINED:
{
    DBG("UNDEFINED");
    goto L_CLEANUP;
}

/* CLOCK */
L_CLOCK:
{
    DBG("CLOCK");

    double time_spent = (double)( clock() - vm_start ) / CLOCKS_PER_SEC;
    printf("%.6lf\n", time_spent);
    pc += 1;
    NEXT_INSTR;
}

/* CONS */
L_CONS:
{
    DBG("CONS");

    cell_t * b = stack->pop();
    cell_t * a = stack->pop();
    cell_t * cons = new cell_t(a, b);
    stack->push( cons );

    // advance to next instruction
    pc += 1;
    NEXT_INSTR;
}

/* HEAD */
L_HEAD:
{
    #if DEBUG
    printf("HEAD\n");
    #endif
    ;
    // take element from stack
    cell_t * object = stack->pop();

    // make sure it is an address before doing anything
    assert_msg(object->is_atom == false, "L_HEAD: Element is not a cell.");

    // push head into stack
    stack->push( object->head );
    
    // advance to next instruction
    pc += 1;
    NEXT_INSTR;
}

/* TAIL */
L_TAIL:
{
    #if DEBUG
    printf("TAIL\n");
    #endif
    ;
    // take element from stack
    cell_t * object = stack->pop();

    // make sure it is an address before doing anything
    assert_msg(object->is_atom == false, "L_TAIL: Element is not a cell.");

    // push tail into stack
    stack->push( object->tail );

    // advance to next instruction
    pc += 1;
    NEXT_INSTR;
}

// -----------------------------------------------------------------

L_CLEANUP:
{
    DBG("CLEANUP");
    delete stack;
    stack = NULL;
}

    return 0;
}