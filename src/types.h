/*
** UPDATE (Hustle):
** Now, we have a new hierarchy:
** - Values:
**   - Immediates (end in 000)
**     - Integers etc.
**   - Pointers (end in non-000)
**     - Box  (001)
**     - Pair (010)
**     - Strings (011)
**     - Functions (100)
**
** Masks are the same, but shifted by three bits, which allow us to
** represent pointers
**
** Integers:
**  - Mask: 1
**  - Type tag: 0 (i.e. ends in 0)
**
** Chars (rightmost bits are 01):
**  - Mask: 11
**  - Type tag: 01
**
** Booleans: Their rightmost bit is 1
** - True: 3 (i.e. 011)
** - False: 7 (i.e. 111)
**
** I/O related values are *not* disjoint from the others!
** EOF:  1011
** Void: 1111
*/

// IMMEDIATES
#define IMMEDIATE_SHIFT 3 // So, this produces ...000


// POINTERS
// There's only a pointer type mask because once we know something is a pointer
// we must ask whether it is a pair or a box (i.e. there's no ptr type tag per se)
#define PTR_TYPE_MASK ((1 << IMMEDIATE_SHIFT) - 1) // i.e. 1000 - 1 -> 111


// INTEGERS
#define INT_SHIFT        (1 + IMMEDIATE_SHIFT)    // 4 = _000
#define INT_TYPE_MASK    ((1 << INT_SHIFT) - 1) // 1
#define INT_TYPE_TAG     (0 << (INT_SHIFT - 1)) // 0
#define NONINT_TYPE_TAG  (1 << (INT_SHIFT - 1)) // 1


// CHARS
// Since all values depend on the int shift and char shift,
// by expressing the int shift in terms of the immediate shift, we can
// keep our original definitions intact
#define CHAR_SHIFT (INT_SHIFT + 1)

// 100 - 1 = 11
#define CHAR_TYPE_MASK ((1 << CHAR_SHIFT) - 1)

// 0 -> 00 -> 00 | 1 -> 01
#define CHAR_TYPE_TAG ((0 << (CHAR_SHIFT - 1)) | NONINT_TYPE_TAG) //

// 1 -> 10 -> 10 | 1 -> 11
#define NONCHAR_TYPE_TAG ((1 << (CHAR_SHIFT - 1)) | NONINT_TYPE_TAG)


// STRINGS
#define STRING_TYPE_TAG 3


// BOOLEANS

// 0 -> 000 -> 000 | 011 -> 011 -> 011
#define VAL_TRUE  ((0 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)

// 1 -> 100 -> 100 | 011 -> 111 -> 111
#define VAL_FALSE ((1 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)


// I/O

// 10 -> 1000 -> 1000 | 11 -> 1011
#define VAL_EOF ((2 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)

// 11 -> 1100 -> 1100 | 11 -> 1111
#define VAL_VOID ((3 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)


// INDUCTIVE DATA
#define BOX_TYPE_TAG 1
#define CONS_TYPE_TAG 2
#define VAL_EMPTY ((4 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)

// FUNCTIONS
#define PROC_TYPE_TAG 4
