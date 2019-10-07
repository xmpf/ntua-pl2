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

#ifndef GC
#define GC (0)
#endif

#if DEBUG
#define DBG(msg) ((fprintf(stderr, "DEBUG(%d): %s\n", __LINE__, (msg))))
#else
#define DBG()
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
#if GC
  , OP_CONS = 0x30      /* CONS */
  , OP_HEAD             /* HEAD */
  , OP_TAIL             /* TAIL */
#endif
} OPCODE;

/* GC Extension Specific Code */
#if GC

// Include Vector container
#include <vector>

// MSB => VALUE || POINTER
// BIT30 => MARKED || NOT_MARKED
#define BIT31 (1 << 31)
#define BIT30 (1 << 30)

// MAKE_ADDRESS
#define MAKE_ADDRESS(x) (((x) | BIT31) & ~(BIT30))

// CLEAR_FLAGS
#define CLEAR_FLAGS(x) ((x) & (BIT30 - 1))

// IS_MARKED
#define IS_MARKED(x) ((x) & BIT30)

// GET OBJ_TYPE BY LOOKING THE MSB
#define OBJ_TYPE(x) ((x) & BIT31)

// SET MSB TO 1
#define SET_MSB(x)  ((x) | BIT31)

// Cell structure
typedef struct cell_t {
    int32_t next;
    int32_t head;
    int32_t tail;

    // Constructors
    cell_t(int32_t a, int32_t b) {
        #if DEBUG
        DBG("cell_t: Constructor called...");
        #endif
        this->next = -1;
        this->head = a;
        this->tail = b;
    }

    // Destructor
    ~cell_t() {
        #if DEBUG
        DBG("~cell_t: Destructor called...");
        #endif
    }

    // Getters
    int32_t cell_head() { // get first element
        return this->head;
    }
    int32_t cell_tail() { // get second element
        return this->tail;
    }
    int32_t get_next() { // get next
        return this->next;
    }
    // Setters
    void set_head(int32_t v) { // set first element
        this->head = v;
    }
    void set_tail(int32_t v) { // set second element
        this->tail = v;
    }
    void set_next(int32_t v) { // set next
        this->next = v;
    }
    void mark_head() { // mark head
        this->head |= BIT31;
    }
    void mark_tail() { // mark tail
        this->tail |= BIT31;
    }
    void unmark_head() { // unamrk head
        this->head &= ~(BIT31);
    }
    void unmark_tail() { // unmark tail
        this->tail &= ~(BIT31);
    }
    void unmark_and_uncheck() {
        this->head &= ~(BIT30);
        this->tail &= ~(BIT30);
    }
} cell_t;

// Heap structure
typedef struct heap_t {
    size_t   size;              /* maybe support to dynamically resize heap */
    cell_t * cells;             /* memory space for cells */
    // std::vector<int32_t> pool;  /* pool of indexes of memory locations on stack */

    // Constructor
    heap_t() {
        this->size = 0;
        cells = NULL;
    }

    heap_t(size_t size) {
        #if DEBUG
        DBG("heap_t: Constructor called...");
        #endif

        this->size = size;
        errno = 0;
        this->cells = (cell_t *)malloc(size * sizeof(cell_t));
        if (cells == NULL || errno != 0) {
            fprintf(stderr, "heap_t: Unable to allocate memory!\n \
                             %s\n", strerror(errno));
            exit(EXIT_FAILURE);
        }
        // Initially, every cell in the heap, points to its next one.
        for(size_t i = 0; i < size - 1; i++) {
            this->cells[i].next = i + 1;
        }
        // last cell in the heap
        this->cells[size - 1].next = -1;
    }

    // Destructor
    ~heap_t() {
        #if DEBUG
        DBG("~heap_t: Destructor called...");
        #endif

        if( this->cells != NULL ) { free(this->cells); }
        this->cells = NULL;
    }

    // Getters
    size_t get_size() { return this->size; }

    // Setters

    // DFS
    void DFS(int32_t ix) {
        // TODO
    }

    // Garbage Collection
    int32_t collect() {
        #if DEBUG
        DBG("heap_t::collect");
        #endif

        // TODO

        return 0;
    }

} heap_t;

/* Functions Prototypes */
static inline bool is_cell(int32_t);

#endif // GC

/* Function Declarations */
void    STACK_PUSH(int32_t *stack, int32_t *stackTop, int32_t value);
int32_t STACK_POP(int32_t *stack, int32_t *stackTop);
static inline int32_t STACK_PEEK(int32_t *stack, int32_t stackTop);

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
    #if GC
      , &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED, &&L_UNDEFINED /* 0x2B .. 0x2F */
      , &&L_CONS  /* 0x30 */
      , &&L_HEAD  /* 0x31 */
      , &&L_TAIL  /* 0x32 */
    #endif // GC
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
    static int32_t stack[STACK_MAX] = {0};
    int32_t stackTop = 0;

    #if GC
    /* Create Heap */
    #define HEAP_SIZE (1 << 10)
    // Allocate space for heap
    heap_t * heap = new heap_t(HEAP_SIZE);
    int32_t next_free = 0;
    #endif

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
    pc =  &code[ pc[1] | (pc[2] << 8) ];
    NEXT_INSTR;
}

/* JUMP IF NOT ZERO */
L_JNZ:
{
    #if DEBUG
    printf ("JNZ %x\n", code[ uf2b(pc + 1) ]);
    #endif
    if ( stack[stackTop - 1] != 0 ) {
        pc = &code[ pc[1] ];
    } else { 
        pc += 3;
    }
    stackTop -= 1;
    NEXT_INSTR;
}

/* DUPLICATES ELEMENT */
L_DUP:
{
    #if DEBUG
    printf ("DUP %d\n", sf1b (pc + 1));
    #endif
    ;
    int32_t offset = pc[1];
    int32_t loc = (offset >= stackTop) ? 0 : stackTop - offset - 1;
    int32_t element = stack[loc];
    STACK_PUSH(stack, &stackTop, element);
    pc += 2;
    NEXT_INSTR;
}

/* SWAP TOP ELEMENT WITH iTH ELEMENT */
L_SWAP:
{   
    #if DEBUG
    printf ("SWAP %d\n", sf1b (pc + 1));
    #endif
    int32_t offset = pc[1];
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
    int32_t element32 = pc[1] | (pc[2] << 8) | (pc[3] << 16) | (pc[4] << 24);
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
    int32_t element16 = pc[1] | (pc[2] << 8);
    STACK_PUSH(stack, &stackTop, element16);
    pc += 3;
    NEXT_INSTR;
}

/* PUSH 1 BYTE */
L_PUSH:
{
    #if DEBUG
    printf ("PUSH1 %d\n", sf1b (pc + 1));
    #endif
    int32_t element8 = pc[1];
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
    printf ("MUL %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("DIV %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("MOD %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("EQ %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("NE %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("LT %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("GT %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("LE %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("GE %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("AND %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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
    printf ("OR %d %d\n", stack[stackTop - 2], stack[stackTop - 1]);
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

/* CLOCK */
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

#if GC
/* CONS */
L_CONS:
{
    #if DEBUG
    printf("CONS\n");
    #endif
    ;

    // if not enough memory => run GC
    if (next_free == -1) {
        #if DEBUG
        DBG("L_CONS: Run out of memory...");
        #endif
        next_free = heap->collect();
    }
    assert_msg(next_free >= 0, "L_CONS: Unable to find some memory!");
    
    // make sure its marked as an address before pushing into stack
    int32_t loc = MAKE_ADDRESS(next_free);
    
    // get top 2 elements from stack
    int32_t b = STACK_POP(stack, &stackTop);
    int32_t a = STACK_POP(stack, &stackTop);

    // create a cell and put it into heap
    heap->cells[next_free] = cell_t(a, b);
    
    // update next_free index
    next_free = heap->cells[next_free].get_next();

    // append location of memory index in memory pool
    // heap->pool.push_back(stackTop);

    // push the location of cell into stack
    STACK_PUSH(stack, &stackTop, loc);

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
    int32_t loc = STACK_POP(stack, &stackTop);

    // make sure it is an address before doing anything
    assert_msg(is_cell(loc), "L_HEAD: Element is not an address.");

    // remove according location from pools
    // heap->pool.pop_back();

    // push head into stack
    STACK_PUSH(stack, &stackTop, heap->cells[loc].cell_head());

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
    int32_t loc = STACK_POP(stack, &stackTop);

    // make sure it is an address before doing anything
    assert_msg(is_cell(loc), "L_TAIL: Element is not an address.");

    // remove according location from pools
    // heap->pool.pop_back();

    // push tail into stack
    STACK_PUSH(stack, &stackTop, heap->cells[loc].cell_tail());

    // advance to next instruction
    pc += 1;
    NEXT_INSTR;
}
#endif // GC

// -----------------------------------------------------------------

L_CLEANUP:
{
    #if DEBUG
    /* VM ENDED INTERPRETING */
    clock_t vm_end = clock();
    printf ("Interpreted in %lf\n", ((double)(vm_end - vm_start) / CLOCKS_PER_SEC));
    #endif

    #if GC
    // Deallocate memory used for "heap"

    #if DEBUG
    DBG("Deallocating memory for heap...");
    #endif

    delete heap;
    #endif

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
void STACK_PUSH(int32_t *stack, int32_t *stackTop, int32_t value)
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
int32_t STACK_POP(int32_t *stack, int32_t *stackTop)
{
    errno = 0;
    if (*stackTop <= 0) {
        fprintf(stderr, "STACK EMPTY :(\n");
        exit(EXIT_FAILURE);
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
        exit(EXIT_FAILURE);
    }
    return stack[stackTop - 1];
}


// --------------------------[CELL_T]--------------------------------

#if GC

// check if it a cell
static inline bool is_cell(int32_t val) {
    return (val & BIT31);
}

// --------------------------[HEAP_T]--------------------------------

#endif // GC