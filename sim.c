/* CPSC 3300, Computer Systems Organization, Clemson University
 *
 * cache statistics for a 4 KiB, 4-way set associative, write-back
 *   data cache with 32 bytes/line and pseudo-LRU replacement
 *
 * note that this simulation does not include the contents of the
 *   cache lines - instead, the cache directory bits (valid, dirty,
 *   and tag) and the per-set replacement state are used to determine
 *   the hit, miss, and write-back counts based on the addresses used
 *   to access the cache and the types of accesses (reads or writes)
 *
 * routines
 *
 *   void cache_init( void );
 *   void cache_access( unsigned int address, unsigned int type );
 *   void cache_stats( void );
 *
 * for each call to cache_access() address is the byte address, and
 *   type is either read (=0) or write (=1)
 *
 *
 * 4 KiB four-way set-associative cache, 32 bytes/line
 *   => 128 total lines, 4 banks, 32 lines/bank
 *   => 32-bit address partitioned into
 *         22-bit tag
 *          5-bit index         [ 5 = log2( 32 lines/bank ) ]
 *          5-bit byte offset   [ 5 = log2( 32 bytes/line ) ]
 *
 * index            bank 0          bank 1          bank 2          bank 3
 * (set) PLRU   v d tag cont    v d tag cont    v d tag cont    v d tag cont
 *       +--+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+
 *   0   |  |  | | |   |////|  | | |   |////|  | | |   |////|  | | |   |////|
 *       +--+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+
 *   1   |  |  | | |   |////|  | | |   |////|  | | |   |////|  | | |   |////|
 *       +--+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+
 *       ...        ...             ...             ...             ...
 *       +--+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+
 *  31   |  |  | | |   |////|  | | |   |////|  | | |   |////|  | | |   |////|
 *       +--+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+  +-+-+---+----+
 *
 *
 * pseudo-LRU replacement using three-bit state scheme for 4-way s.a.
 *
 *  each bit represents one branch point in a binary decision tree
 *
 *  let 1 represent that the left side has been referenced more
 *  recently than the right side, and 0 vice-versa
 *
 *             are all 4 lines valid?
 *                  /       \
 *                yes        no, use an invalid line
 *                 |
 *                 |
 *                 |
 *            bit_0 == 0?           state | replace    ref to | next state
 *             /       \            ------+--------    -------+-----------
 *            y         n            00x  |  line_0    line_0 |    11_
 *           /           \           01x  |  line_1    line_1 |    10_
 *    bit_1 == 0?    bit_2 == 0?     1x0  |  line_2    line_2 |    0_1
 *      /    \          /    \       1x1  |  line_3    line_3 |    0_0
 *     y      n        y      n
 *    /        \      /        \       ('x' means     ('_' means unchanged)
 *  line_0  line_1  line_2  line_3     don't care)
 *
 * see Figure 3-7, p. 3-18, in Intel Embedded Pentium Processor Family Dev.
 *   Manual, 1998,
 *   Working Link
 *   https://www.ardent-tool.com/CPU/docs/Intel/Pentium/embedded/273204-001.pdf
 *
 * note that there is separate state kept for each set (i.e., index value)
 */

#include<stdio.h>
#include<stdlib.h>

#define LINES_PER_BANK 16
// 512 bytes cache means
// 5 bits offset Address, log2(cache line size in bytes) = log2(32) = 5
// 16 Cache lines total (memory size / cache line size)
// 4 sets               (number of cache lines / Associativity)
// 2 set address width = log2(4) = log2(# of sets)
// 9 Tag width = 16 - 2 - 5 = 9 = (Full address width - SET width - Offset width)
unsigned int
plru_state[LINES_PER_BANK],  /* current state for each set */
valid[4][LINES_PER_BANK],    /* valid bit for each line    */
dirty[4][LINES_PER_BANK],    /* dirty bit for each line    */
tag[9][LINES_PER_BANK],      /* tag bits for each line     */

plru_bank[4] /* table for bank replacement choice based on state */

= { 0, 0, 1, 1},

        next_state[32] /* table for next state based on state and bank ref */
        /* index by 5-bit (4*state)+bank [=(state<<2)|bank] */

        /*  bank ref  */
        /* 0  1  2  3 */

        /*         0 */  = {  6, 4, 1, 0,
        /*         1 */       7, 5, 1, 0,
        /*         2 */       6, 4, 3, 2,
        /* current 3 */       7, 5, 3, 2,
        /*  state  4 */       6, 4, 1, 0,
        /*         5 */       7, 5, 1, 0,
        /*         6 */       6, 4, 3, 2,
        /*         7 */       7, 5, 3, 2  };

unsigned int
cache_reads,  /* counter */
cache_writes, /* counter */
hits,         /* counter */
misses,       /* counter */
write_backs;  /* counter */

void cache_init( void ){
    int i;
    for( i=0; i<LINES_PER_BANK; i++ ){
        plru_state[i] = 0;
        valid[0][i] = dirty[0][i] = tag[0][i] = 0;
        valid[1][i] = dirty[1][i] = tag[1][i] = 0;
        valid[2][i] = dirty[2][i] = tag[2][i] = 0;
        valid[3][i] = dirty[3][i] = tag[3][i] = 0;
    }
    cache_reads = cache_writes = hits = misses = write_backs = 0;
}

void cache_stats( void ){
    printf( "cache statistics (in decimal):\n" );
    printf( "  cache reads       = %d\n", cache_reads );
    printf( "  cache writes      = %d\n", cache_writes );
    printf( "  cache hits        = %d\n", hits );
    printf( "  cache misses      = %d\n", misses );
    printf( "  cache write backs = %d\n", write_backs );
}


/* address is byte address, type is read (=0) or write (=1) */

void cache_access( unsigned int address, unsigned int type ){

    unsigned int
            addr_tag,    /* tag bits of address     */
    addr_index,  /* index bits of address   */
    bank;        /* bank that hit, or bank chosen for replacement */

    if( type == 0 ){
        cache_reads++;
    }else{
        cache_writes++;
    }
    
    // addr_index = (address >> 3) & 0x7
    addr_index = (address >> 4) & 0xF;
    //addr_index = (address >> 4) & 0x3;
    addr_tag = address >> 6;

    /* check bank 0 hit */

    if( valid[0][addr_index] && (addr_tag==tag[0][addr_index]) ){
        hits++;
        bank = 0;

        /* check bank 1 hit */

    }else if( valid[1][addr_index] && (addr_tag==tag[1][addr_index]) ){
        hits++;
        bank = 1;

        /* check bank 2 hit */

    }else if( valid[2][addr_index] && (addr_tag==tag[2][addr_index]) ){
        hits++;
        bank = 2;

        /* check bank 3 hit */

    }else if( valid[3][addr_index] && (addr_tag==tag[3][addr_index]) ){
        hits++;
        bank = 3;

        /* miss - choose replacement bank */

    }else{
        misses++;

        if( !valid[0][addr_index] ) bank = 0;
        else if( !valid[1][addr_index] ) bank = 1;
        else if( !valid[2][addr_index] ) bank = 2;
        else if( !valid[3][addr_index] ) bank = 3;
        else bank = plru_bank[ plru_state[addr_index] ];

        if( valid[bank][addr_index] && dirty[bank][addr_index] ){
            write_backs++;
        }

        valid[bank][addr_index] = 1;
        dirty[bank][addr_index] = 0;
        tag[bank][addr_index] = addr_tag;
    }

    /* update replacement state for this set (i.e., index value) */

    plru_state[addr_index] = next_state[ (plru_state[addr_index]<<2) | bank ];

    /* update dirty bit on a write */

    if( type == 1 ) dirty[bank][addr_index] = 1;
}

/* behavioral simulation of MC88100 subset for CPSC 3300 at Clemson
 *
 * reference manual is http://www.bitsavers.org/components/motorola/
 *   88000/MC88100_RISC_Microprocessor_Users_Manual_2ed_1990.pdf
 *
 * processor state subset
 *   32 x 32-bit general registers
 *     note that r0 is always 0
 *   two 32-bit instruction address registers
 *     fip - fetch address pointer
 *     xip - execute address pointer
 * 
 * memory
 *   byte-addressable
 *   big-endian addressing
 *   aligned accesses
 *   - an instruction starts on an address that is a multiple of 4
 *   - a data word starts on an address that is a multiple of 4
 *
 *   note - we limit this simulation to a 1 MiB memory
 *
 *   note - instructions are one word (four bytes) in length and
 *     we also limit the instruction subset in this simulation to
 *     operations on on word-length data
 *
 * we implement all three addressing modes, see pages 3-7 to 3-10
 *
 *   register indirect with zero-extended immediate index
 *     eff_addr = reg[ src1 ] + immed_16_bit
 *
 *   register indirect with index
 *     eff_addr = reg[ src1 ] + reg[ src2 ]
 *
 *   register indirect with scaled index (word scaling = 2)
 *     eff_addr = reg[ src1 ] + ( reg[ src2 ] << 2 )
 *
 * we implement 20 instructions derived from 12 base instructions
 *
 *   halt is added for the simulation
 *   add  is described on pages 3-29 to 3-30
 *   bcnd is described on pages 3-13 to 3-14 and 3-35 to 3-36
 *   br   is described on pages 3-16 and 3-37
 *   ext  is described on pages 3-44 to 3-45
 *   extu is described on pages 3-46 to 3-47
 *   mak  is described on pages 3-70 to 3-71
 *   rot  is described on page 3-76
 *   ld   is described on pages 3-65 to 3-66
 *   lda  is described on pages 3-67 to 3-68
 *   st   is described on pages 3-79 to 3-80
 *   sub  is described on pages 3-82 to 3-83
 *
 * decoding and instruction formats (op1 is first six bits)
 *
 *   op1 = 0 => halt
 *
 *   op1 = 0x05, 0x09, 0x0d, 0x1c, 0x1d =>
 *     opcodes are ld, st, lda, add, sub, respectively
 *     format has two registers and a 16-bit immediate
 *     immediate value is zero-extended
 *     signed words in normal mode used for load/stores,
 *       so p = 01, ty = 01, and u = 0
 *     carry and borrow are not used, so i = 0 and o = 0
 *
 *   op1 = 0x30 => br
 *     format has a single 26-bit displacement
 *     displacement is sign-extended
 *     displacement is in words and calculated from the
 *       address of the current instruction rather than
 *
 *   op1 = 0x3a => bcnd
 *     format has mask, register, and 16-bit displacement
 *     displacement is sign-extended
 *     displacement is in words and calculated from the
 *       address of the current instruction rather than
 *       the updated fetch address
 *     assert checks that displacement is non-zero
 *     delayed branching is not used, so n = 0
 *
 *   op1 = 0x3c => ext, extu, mak, rot
 *     format has two registers and a 5-bit immediate
 *     ext, extu, and mak are used as shifts so w5 = 0
 *       the updated fetch address
 *     assert checks that displacement is non-zero
 *     delayed branching is not used, so n = 0
 *
 *   op1 = 0x3d => ld, st, lda, add, sub
 *     format has three registers
 *     for add and sub:
 *       carry and borrow are not used, so i = 0 and o = 0
 *     for ld, sta, and lda:
 *       signed words in normal mode used for load/stores,
 *         so p = 01, ty = 01, and u = 0
 *       if bit 9 = 1, the third register is scaled
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


/* since the simulation deals only with one-word instructions and */
/*   one-word operands, we represent memory as an array of words  */

#define MEM_SIZE_IN_WORDS (256*1024)

int mem[MEM_SIZE_IN_WORDS];

/* processor state, simulation state, and instruction fields    */

int reg[32]   = {0}, /* general register set, r0 is always 0    */
//    xip       = 0,   /* execute instruction pointer             */
//    fip       = 0,   /* fetch instruction pointer               */
    fp_reg[32] = {0};   //floating point register file
unsigned int
    //The six control registers
    psr,            //Controls Miscellaneous State information for current process (p.35)
    epsr,           // ^ but additional information when needed (p.38)
    db,             // Data Breakpoint register (p.39)
    dirbase,        // controls address translation, caching and bus options (p.39)
    fir,            // Fault Instruction Register, information on trap (p.41)
    fsr,            // Floating Point status, contatins floating and rounding data (p.41)
    //Four sepcial purpose registers
    kr,             // "K"onstant
    ki,             // "K"onstant
    t,              // Temporary
    merge,          // only used for graphics
    halt_flag = 0,   /* set by halt instruction                 */
    verbose   = 0;  /* governs amount of detail in output      */
int
    op,             // Opcode
    dest,              //destination register
    src1,             //source 1 register
    src2,             //source 2 register
    //immed_16_bit,           /* 16-bit immediate field                  */
    eff_addr;        /* 32-bit effective address                */

    // Other global Variables used as
 int
    //offset_16_bit,
    //offset_26_bit,
    //immed_5_bit,
    ip = 0,             // Instruction Pointer
    cc = 0;             //
unsigned int ir;
unsigned int immed_5_bit;
unsigned int offset_26_bit;
unsigned int offset_16_bit;

unsigned int immed_16_bit;

/* dynamic execution statistics */

int inst_fetches = 0,
    memory_reads = 0,
    memory_writes = 0,
    branches = 0,
    taken_branches = 0;


/* load memory from stdin */

#define INPUT_WORD_LIMIT 255
void get_mem(char *inputfile){
    FILE *in;
    if( (in = fopen(inputfile, "r")) == NULL){
        printf("Error opening input file, Please check that %s exists \n", inputfile);
        exit(-1);
    }

  int w, count = 0;
  if( verbose > 1 ) printf( "reading words in hex from stdin:\n" );
  while(fscanf(in, "%x", &w) != EOF){
    if( verbose > 1 ) printf( "  0%08x\n", w );
    if( count > INPUT_WORD_LIMIT ){
      printf( "too many words loaded\n" );
      exit( 0 );
    }
    mem[ count ] = w;
    count++;
  }
  if( verbose > 1 ) printf( "\n" );
}

void read_mem( int adr, int reg_index ){
  int word_addr = adr >> 2;
  if( verbose ) printf( "  read access at address %x\n", adr );
  assert( ( word_addr >= 0 ) && ( word_addr < MEM_SIZE_IN_WORDS ) );
  reg[ reg_index ] = mem[ word_addr ];
  memory_reads++;
}

void write_mem( int adr, int reg_index ){
  int word_addr = adr >> 2;
  if( verbose ) printf( "  write access at address %x\n", adr );
  assert( ( word_addr >= 0 ) && ( word_addr < MEM_SIZE_IN_WORDS ) );
  mem[ word_addr ] = reg[ reg_index ];
  memory_writes++;
}

/* extract fields - switch statements are in main loop */
// Decode images is on 170
void decode(){
    op              = (ir >> 26) & 0x3f;
    src2            = (ir >> 21) & 0x1f;
    dest            = (ir >> 16) & 0x1f;
    src1            = (ir >> 11) & 0x1f;
    immed_16_bit    = ir & 0xffff;
    offset_16_bit   = ir & 0xffff;
    offset_26_bit   = ir & 0x03ffffff;
    immed_5_bit     = (ir & 0x0000f800) >> 11;
}
void halt(){
  if( verbose ) printf( "halt\n" );
  halt_flag = 1;
}
/*
 * Load OpCode 0x04
 * This is load integer
 * Found on Page 61
 * rdest <- mem.x(src1 + src2)
 */
void ld(){
    if(verbose) printf("ld.l  r%x(r%x),r%x\n", src1, src2, dest );

    eff_addr = reg[src1] + reg[src2];

    read_mem(eff_addr, dest);
    cache_access(eff_addr,0);
}

/*
 * Immediate Load OpCode 0x05
 * This is load integer, immediate format
 * Found on Page 61
 * rdest <- mem.x(src1 + src2)
 * however since this is the immediate load
 * it instead uses the immed_16_bit for 16 bit immediate address
 * creating
 * rdest <- mem.x (src2 + immed_16_bit)
 */
void imm_ld(){  /* pages 3-65 to 3-66 */
  if( verbose ) printf("ld.l  %x(r%x),r%x\n", immed_16_bit, src2, dest);

  int seBit = immed_16_bit;
  seBit = ((seBit << 16) >> 16);
  seBit = seBit - 1;
  eff_addr = reg[src2] + seBit;
  read_mem(eff_addr, dest);
}
unsigned short removeSignificantBit(unsigned short num) {
    unsigned int r = num >> 1;
    r = r | (r >> 1);
    r = r | (r >> 2);
    r = r | (r >> 4);
    r = r | (r >> 8);
    r = r | (r >> 16);
    // Remove most significant bit
    unsigned int value = r & num;
    return value;
}

/*
 *  Store OpCode 0x07
 *  This is store integer
 * Found on Page 62
 * mem.x(src2 + #const) <- src1ni
 * however since this is the immediate store
 * it instead uses the immed_16_bit for 16 bit immediate address
 * creating
 * mem.x (src2 + immed_16_bit) -> src1
 */
void st(){  /* pages 3-79 to 3-80 */
    //printf("\n%08x\n",ir);
    //printf("\n %x,%x,%x,%x,%x  \n",op,src2,dest,src1,immed_16_bit);

    unsigned int temp = dest;
    temp = temp << 11;
    temp = temp | offset_16_bit;

    offset_16_bit = temp;
    unsigned int mask = 0x000000FF;
    offset_16_bit = offset_16_bit & mask;

    //printf("\n %x,%x,%x",offset_16_bit,immed_5_bit,offset_26_bit);
    //printf("\n %x, %x\n", removeSignificantBit(offset_16_bit), offset_16_bit);
    // A bug I couldnt figure out the cause of but could program around
    //offset_16_bit = removeSignificantBit(offset_16_bit);
    //immed_16_bit = removeSignificantBit(immed_16_bit);

  if( verbose ) printf("st.l  r%x,%x(r%x)\n", src1,offset_16_bit, src2 );


    eff_addr = reg[src2] + (signed) offset_16_bit;

    eff_addr = eff_addr - 1;

  write_mem(eff_addr, src1);

  cache_access(eff_addr,1);
}

/*
 *  BTNE opcode 0x14
 *  This is branch if not equal, bte format
 *  Found on page 71
 *  IF src1 s != src2
 *  THEN continue execution at brx(sbroff)
 */
void btne(){

    int temp = dest;
    temp = temp << 11;
    temp = temp | offset_16_bit;

    offset_16_bit = temp;
    //printf("\n%08x\n",ir);
    //printf("\n %x,%x,%x,%x,%x,%x  \n",op,src2,dest,src1,offset_16_bit, immed_5_bit);
    offset_16_bit = offset_16_bit & 0x0000FFFF;

    if (verbose) printf("btne  r%x,r%x,%x", src1, src2, offset_16_bit);
    if (verbose) if(offset_16_bit > 9) printf(" (= decimal %d)",(signed short)offset_16_bit);
    if (verbose) printf("\n");

    branches = branches + 1;
    if (reg[src1] != reg[src2]){

        int offsetFix = offset_16_bit;
        // First shift left by two
        offsetFix = offsetFix << 2;
        //sign extend to 32
        offsetFix = ((offsetFix << 16) >> 16);


        ip = ip + offsetFix;

        taken_branches = taken_branches + 1;
    }
}
/*
 * BTNEI Opcode 0x15
 * This is branch if not equal, immediate format
 * Found on Page 71
 *  IF src1 s != src2
 *  THEN continue execution at brx(sbroff)
 *
 */
//int repeat = 0;
void btnei(){

/*
    unsigned short temp;
    unsigned short temp2;
    temp = offset_16_bit & 0x7FF;
    //0x0000F800
    temp2 = immed_5_bit & 0x1F;
    temp2 = temp2 << 11;
    temp = temp | temp2;
    */
    int temp = dest;
    temp = temp << 11;
    temp = temp | offset_16_bit;

    offset_16_bit = temp;
    //printf("\n%08x\n",ir);
    //printf("\n %x,%x,%x,%x,%x,%x  \n",op,src2,dest,src1,offset_16_bit, immed_5_bit);
    offset_16_bit = offset_16_bit & 0x0000FFFF;

    //printf("\n%08x\n",ir);
    //printf("\n %x,%x,%x,%x,%x,%x  \n",op,src2,dest,src1,offset_16_bit, immed_5_bit);



    if (verbose) printf("btnei %x,r%x,%x", immed_5_bit, src2, offset_16_bit);
    if (verbose) if( (signed short) offset_16_bit > 9 || (signed short) offset_16_bit < 0) printf(" (= decimal %d)",(signed short) offset_16_bit);
    if (verbose) printf("\n");

    branches = branches + 1;
    int temp1 = (int) immed_5_bit;
    int temp2 = (int) reg[src2];
    if (temp1 != temp2) /*&& repeat < 3)*/{
        //printf("\n%d == %d , %d == %d \n",temp1,temp2,(int) immed_5_bit, (int) reg[src2]);

        signed int offsetFix = (signed short) offset_16_bit;
        offsetFix = offsetFix & 0xffff;
        // First shift left by two
        offsetFix = offsetFix << 2;

        //sign extend to 32
        offsetFix = ((offsetFix << 16) >> 16);

        //repeat++;
        ip = ip + offsetFix;

        taken_branches = taken_branches + 1;
    }
}
/*
 * BTE Opcode 0x16
 * This is branch if equal, bte format
 * Found on page 71
 *IF srcls = src2
    THEN continue execution at brx( sbroff
 *
 */
void bte(){
    offset_16_bit = offset_16_bit & 0x7FF;
    if (verbose) printf("bte   r%x,r%x,%x", src1, src2, offset_16_bit );

    printf("\n");

    branches = branches + 1;
    if (reg[src1] == reg[src2]){

        int offsetFix = offset_16_bit;
        // First shift left by two
        offsetFix = offsetFix << 2;
        //sign extend to 32
        offsetFix = ((offsetFix << 16) >> 16);

        ip = ip + offsetFix;

        taken_branches = taken_branches + 1;
    }
}
/*
 * BTE Opcode 0x17
 * This is branch if equal, immediate format
 * Found on page 71
 *
 */
void btei(){

    unsigned short temp;
    unsigned short temp2;
    temp = offset_16_bit & 0x7FF;
    //0x0000F800
    temp2 = dest & 0x1F;
    temp2 = temp2 << 11;
    temp = temp | temp2;

    offset_16_bit = temp;
    //printf("\nPre: %x,%x\n",immed_16_bit,immed_5_bit);
    //printf("Post: %x, %x\n",temp, temp2);

    //printf("\n%08x\n",ir);
    //printf("\n %x,%x,%x,%x,%x,%x  \n",op,src2,dest,src1,offset_16_bit, immed_5_bit);

    //offset_16_bit = removeSignificantBit(temp);

    if (verbose) printf("btei  %x,r%x,%x", immed_5_bit, src2, offset_16_bit );
    if (verbose) if(offset_16_bit > 9) printf(" (= decimal %d)",(signed short)offset_16_bit);
    printf("\n");

    branches = branches + 1;
    //printf("\n%d == %d\n", (unsigned int)immed_5_bit, (int) reg[src2]);
    if ((unsigned int) immed_5_bit == (int) reg[src2]){

        int offsetFix = offset_16_bit;
        // First shift left by two
        offsetFix = offsetFix << 2;
        //sign extend to 32
        offsetFix = ((offsetFix << 16) >> 16);

        //printf("\n%x, %x\n",ip, offsetFix);
        // then add to the current instruction pointer plus 4
        ip = ip + offsetFix;
        //printf("\n%x \n",ip);
        taken_branches = taken_branches + 1;
    }
}
/*
 * BR Opcode 0x1a
 * This is Unconditional Branching, ctrl format,
 * do not delay branch, the effect is immediate
 * Found on Page 71
 *
 */
void br() {
    if (verbose) printf("br    %x", offset_26_bit);
    if (verbose) if(offset_26_bit > 9) printf(" (= decimal %d)",(short)offset_26_bit);
    printf("\n");

    branches = branches + 1;
    taken_branches = taken_branches + 1;

    int offsetFix = offset_26_bit;
    // First shift left by two
    offsetFix = offsetFix << 2;
    //sign extend to 32
    offsetFix = ((offsetFix << 6) >> 6);


    // then add to the current instruction pointer plus 4
    ip = ip + offsetFix;

}
/*
 * Bc Opcode 0x1c
 * This is branch on CC set, ctrl format
 * Found on Page 71
 *
 *
 */
void bc() {
    if (verbose) printf("bc    %x", offset_26_bit );
    if (verbose) if(offset_26_bit > 9) printf(" (= decimal %d)",(int)offset_26_bit);
    printf("\n");

    branches = branches + 1;

    if (cc == 1){
        int offsetFix = offset_26_bit;
        // First shift left by two
        offsetFix = offsetFix << 2;
        //sign extend to 32
        offsetFix = ((offsetFix << 6) >> 6);


        // then add to the current instruction pointer plus 4
        ip = ip + offsetFix;

        taken_branches = taken_branches + 1;
    }
}
/*
 * Bnc Opcode 0x1e
 * This is branch on CC clear, ctrl format
 * Found on Page 71
 *
 *
 */
void bnc() {
    if (verbose) printf("bnc   %x", offset_26_bit );
    if (verbose) if(offset_26_bit > 9) printf(" (= decimal %d)",(short)offset_26_bit);
    printf("\n");

    branches = branches + 1;



    if (cc == 0){
        int offsetFix = offset_26_bit;
        // First shift left by two
        offsetFix = offsetFix << 2;
        //sign extend to 32
        offsetFix = ((offsetFix << 6) >> 6);


        // then add to the current instruction pointer plus 4
        ip = ip + offsetFix;

        taken_branches = taken_branches + 1;
    }
}
/*
 *  Adds 0x24
 *  Add signed, general
 *  Found on page 66
 */
void adds() {
    if (verbose) printf("adds  r%x,r%x,r%x\n", src1, src2, dest);

    if(reg[src2] < reg[src1]){
        cc = 1;
    }
    else{
        cc = 0;
    }
    reg[dest] = reg[src1] + reg[src2];
}
/*
 * Adds Immediate 0x25
 * add signed, immediate
 * Found on page 66
 */
void addsi(){
    if(verbose) printf("adds  %x,r%x,r%x\n", immed_16_bit, src2, dest );

    int SE_immed_16_bit = immed_16_bit;
    SE_immed_16_bit = ((SE_immed_16_bit << 16) >> 16);

    //printf("%d < %d\n",reg[src2], (int) immed_16_bit);

    //printf("%d \n",cc);
    //printf("\n%d = %d + %d\n",reg[dest], (int) immed_16_bit, reg[src2]);
    //printf(" %08x,%08x \n",immed_16_bit,SE_immed_16_bit);
    reg[dest] = SE_immed_16_bit + reg[src2];

    unsigned int unsignedCCTest = reg[dest];
    cc = unsignedCCTest & 0x80000000;
    if(cc != 0){
        cc = 1;
    }

    //printf("\n%d < %d\n",reg[src2], (int) immed_16_bit);
    /*
    int src1val = 0 - immed_16_bit;
    int isrc2 = reg[src2];
    if ( isrc2 < src1val){
        //if(cc == 1){
            //cc = 0;
        //}
        //else {
            cc = 1;
        //}
    }
    if(reg[src2] >= -immed_16_bit){
        cc = 0;
    }
     */
}
/*
 * Subtract Signed 0x26
 * subtract Signed, general format
 * found on page 66
 */
void subs(){
    if(verbose) printf( "subs  r%x,r%x,r%x\n", src1, src2, dest );

    if(reg[src2] > reg[src1]){
        cc = 1;
    }
    else{
        cc = 0;
    }
    reg[dest] = reg[src1] - reg[src2];
}
/*
 * Subtract Signed 0x27
 * subtract Signed, immediate format
 * found on page 66
 */
void subsi(){
    if(verbose) printf( "subs  %x,r%x,r%x\n", immed_16_bit, src2, dest );

    if(reg[src2] > immed_16_bit){
        cc = 1;
    }
    else{
        cc = 0;
    }

    reg[dest] = immed_16_bit - reg[src2];
}

/* for rotate
 *
 *   (32-n) bits   n bits
 * +-------------+-------+
 * |      A      |   B   |
 * +-------------+-------+
 *
 * value 1 = AB shift left (32-n) bits
 * +-------+-------------+
 * |   B   |      0      |
 * +-------+-------------+
 *
 * value 2 = AB logical shift right n bits
 * +-------+-------------+
 * |   0   |      A      |
 * +-------+-------------+
 *
 * now or the two values together
 * +-------+-------------+
 * |   B   |      A      |
 * +-------+-------------+
 */
/*
 * SHL 0x28
 * shift left general format
 * found on page 67
 * rdest <- src2 shifted left by src 1 bits
 */
void shl(){
    if (verbose) printf( "shl   r%x,r%x,r%x\n", src1, src2, dest );

    reg[dest] = reg[src2] << reg[src1];
}
/*
 * SHLI 0x29
 * shift left immediate format
 * found on page 67
 *
 */
void shli(){
    if (verbose) printf( "shli  %x,r%x,r%x\n", immed_16_bit, src2, dest );

    reg[dest] = reg[src2] << immed_16_bit;
}

/*
 * SHR 0x2a
 * shift right logical, general format
 * found on page 67
 * dest <- src2 shifted right by srcJ bits
 */
void shr(){
    if (verbose) printf( "shr   r%x,r%x,r%x\n", src1, src2, dest );

    unsigned int temp;
    temp = reg[src2];
    temp = temp >> reg[src1];
    reg[dest] = (int) temp;
}

/*
 * SHR 0x2b
 * shift right logical, immediate format
 * found on page 67
 *
 */
void shri(){
    if (verbose) printf( "shri  %x,r%x,r%x\n", immed_16_bit, src2, dest );

    unsigned int temp;
    temp = reg[src2];
    temp = temp >> immed_16_bit;
    reg[dest] = (int) temp;
}

/*
 * SHRA 0x2f
 * shift right arithmetic, general format
 * found on page 67
 * rdest <- src2 arithmetically shifted right by src 1 bits
 */
void shra(){
    if (verbose) printf( "shra  r%x,r%x,r%x\n", src1, src2, dest );

    reg[dest] = reg[src2] >> reg[src1];
}

/*
 * SHRAI 0x2f
 * shift right arithmetic, immediate format
 * found on page 67
 *
 */
void shrai(){
    if (verbose) printf( "shrai %x,r%x,r%x\n", immed_16_bit, src2, dest );

    reg[dest] = reg[src2] >> immed_16_bit;
}

void unknown_op(){
  printf("unknown instruction %08x\n", ir );
  printf( " op=%x",  op);
  printf(" dest=%x", dest );
  printf(" src1=%x", src1 );
  printf(" src2=%x\n", src2 );
  printf( "program terminates\n" );
  exit( -1 );
}


int main( int argc, char **argv ){

  if( argc > 1 ){
    if( ( argv[1][0] == '-' ) && ( argv[1][1] == 't' ) ){
      verbose = 1;
    }else if( ( argv[1][0] == '-' ) && ( argv[1][1] == 'v' ) ){
      verbose = 2;
    }/*else{
      printf( "usage:\n");
      printf( "  %s for just execution statistics\n", argv[0] );
      printf( "  %s -t for instruction trace\n", argv[0] );
      printf( "  %s -v for instructions, registers, and memory\n", argv[0] );
      printf( "input is read as hex 32-bit values from stdin\n" );
    }*/
  }
  if(verbose > 0) {
      get_mem(argv[2]);
  }
  else{
      get_mem(argv[1]);
  }
  cache_init();
  if (verbose) printf( "instruction trace:\n" );
  while( !halt_flag ){
    if(verbose) printf("at %02x, ", ip);
    // get instruction
      // retrieve new address
      dirbase = ip >> 2;
      // use it to access memory
      ir = mem[dirbase];
      // increment program counter
      ip = ip + 4;
      //update fetch stat
      inst_fetches = inst_fetches + 1;
    //decode
    decode();
    //execute
    switch( op ){

      case 0x00:        halt();     break;
      case 0x04:        ld();       break;
      case 0x05:        imm_ld();   break;
      case 0x07:        st();       break;
      case 0x14:        btne();     break;
      case 0x15:        btnei();    break;
      case 0x16:        bte();      break;
      case 0x17:        btei();     break;
      case 0x1a:        br();       break;
      case 0x1c:        bc();       break;
      case 0x1e:        bnc();      break;
      case 0x24:        adds();     break;
      case 0x25:        addsi();    break;
      case 0x26:        subs();     break;
      case 0x27:        subsi();    break;
      case 0x28:        shl();      break;
      case 0x29:        shli();     break;
      case 0x2a:        shr();      break;
      case 0x2b:        shri();     break;
      case 0x2e:        shra();     break;
      case 0x2f:        shrai();    break;

      default:          unknown_op();
    }

    reg[ 0 ] = 0;  /* make sure that r0 stays 0 */

    if( ( verbose > 1 ) || ( halt_flag && ( verbose == 1 )) ){
      for( int i = 0; i < 8 ; i++ ){
        printf( "  r%x: %08x", i , reg[ i ] );
        printf( "  r%x: %08x", i + 8 , reg[ i + 8 ] );
        printf( "  r%x: %08x", i + 16, reg[ i + 16 ] );
        printf( "  r%x: %08x\n", i + 24, reg[ i + 24 ] );
      }
      printf("  cc: %d\n",cc);
    }
  }

  if( verbose ) printf( "\n" );
  printf( "execution statistics (in decimal):\n" );
  printf( "  instruction fetches = %d\n", inst_fetches );
  printf( "  data words read     = %d\n", memory_reads );
  printf( "  data words written  = %d\n", memory_writes );
  printf( "  branches executed   = %d\n", branches );
  if( taken_branches == 0 ){
    printf( "  branches taken      = 0\n" );
  }else{
    printf( "  branches taken      = %d (%.1f%%)\n",
      taken_branches, 100.0*((float)taken_branches)/((float)branches) );
  }
  cache_stats();
  return 0;
}
