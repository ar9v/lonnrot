/*
** Bit representations of values
**
** Integers:
**  - Mask: 1
**  - Type tag: 0
** Chars (rightmost bits are 01):
**  - Mask: 11
**  - Type tag: 01
** Booleans: Their rightmost bit is 1
** - True: 3 (i.e. 011)
** - False: 7 (i.e. 111)
*/

// INTEGERS
#define INT_SHIFT        1
#define INT_TYPE_MASK    ((1 << INT_SHIFT) - 1) // 1
#define INT_TYPE_TAG     (0 << (INT_SHIFT - 1)) // 0
#define NONINT_TYPE_TAG  (1 << (INT_SHIFT - 1)) // 1

// CHARS
#define CHAR_SHIFT 2

// 100 - 1 = 11
#define CHAR_TYPE_MASK ((1 << CHAR_SHIFT) - 1)

// 0 -> 00 -> 00 | 1 -> 01
#define CHAR_TYPE_TAG ((0 << (CHAR_SHIFT - 1)) | NONINT_TYPE_TAG) //

// 1 -> 10 -> 10 | 1 -> 11
#define NONCHAR_TYPE_TAG ((1 << (CHAR_SHIFT - 1)) | NONINT_TYPE_TAG)

// BOOLEANS

// 0 -> 000 -> 000 | 011 -> 011 -> 011
#define VAL_TRUE  ((0 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)

// 1 -> 100 -> 100 | 011 -> 111 -> 111
#define VAL_FALSE ((1 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)

// NIL
#define VAL_NIL ((2 << CHAR_SHIFT) | NONCHAR_TYPE_TAG)
