/*
** Bit representations of values
**
** Integers: Their rightmost bit is 0
** Booleans: Their rightmost bit is 1
** - True: 1 (i.e. 01)
** - False: 3 (i.e. 11)
*/

#define INT_SHIFT        1
#define INT_TYPE_MASK    ((1 << INT_SHIFT) - 1)
#define INT_TYPE_TAG     (0 << (INT_SHIFT - 1))
#define NONINT_TYPE_TAG  (1 << (INT_SHIFT - 1))
#define VAL_TRUE  ((0 << INT_SHIFT) | NONINT_TYPE_TAG)
#define VAL_FALSE ((1 << INT_SHIFT) | NONINT_TYPE_TAG)
