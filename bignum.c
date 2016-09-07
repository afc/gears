/* bignum.c
 * i12n of big integer arithmetic:
 *  add, sub, mul, div
 * feel free to build with ANSI C
 * code borrowed from Steven Skiena
 *  url reference: goo.gl/DuIF0W
 * */

#include <stdio.h>

typedef char     tint;          /*     tiny integer */
typedef unsigned uint;          /* unsigned integer */

#ifndef bool                    /* boolean support */
    #define false (0)
    #define true  (1)
    typedef char bool;
    #define bool bool
#endif 

#define MAX_DIGITS (128)        /* maximum length of bignum */
#define POSITIVE   (+1)         /* positive sign bit */
#define NEGATIVE   (-1)

/* TODO: using a tab(char *) to allocate space dynamicly */
typedef struct {
    char digits[MAX_DIGITS];    /* represent of the number */
    tint signbit;               /* +1 for zero and positive integer */
    uint lastdigit;             /* index of high-order digit */
} bignum;

#define abs(x) ((x) < 0 ? -(x) : (x))

#define is_zero(n)  ((n)->lastdigit == 0 && (n)->digits[0] == (char) 0)
#define is_one(n)   ((n)->lastdigit == 0 && (n)->digits[0] == (char) 1)

void print_bignum(const bignum *n)
{
    int i;
    if (n->signbit == NEGATIVE) putchar('-');
    for (i = n->lastdigit; i >= 0; i--)
        putchar('0' + n->digits[i]);
    putchar('\n');
}

/* BUG: if m = -2147483648 (assume 32-bits) */
void int_to_bignum(int m, bignum *n)
{   /* only special is m == 0 */
    n->signbit = NEGATIVE;
    if (m >= 0)
        n->signbit = POSITIVE;
    
    uint i;
    for (i = 0; i < MAX_DIGITS; i++)
        n->digits[i] = (char) 0;

    uint t = abs(m);
    i = -1;         /* ok with unsigned int */

    while (t > 0) {
        n->digits[++i] = t % 10;
        t /= 10;
    }
    n->lastdigit = (m != 0) ? i : 0;
}

#define initialize_bignum(n) int_to_bignum(0, n)

#define max(a, b) (a > b ? a : b)

#define zero_justify(n) ({                                      \
    while (n->lastdigit > 0 && n->digits[n->lastdigit] == 0)    \
        n->lastdigit--;                                         \
    /* avoid print_bignum() represent -0  */                    \
    if (is_zero(n))                                             \
        n->signbit = POSITIVE;                                  \
})

/* bignum comparison return signbit of (a - b) */
tint compare_bignum(const bignum *a, const bignum *b)
{
    tint signbit = a->signbit;
    if (a->signbit != b->signbit) return signbit;
    
    /* now a and b have the same signbit */
    if (a->lastdigit > b->lastdigit) return POSITIVE * signbit;
    if (a->lastdigit < b->lastdigit) return NEGATIVE * signbit;
    
    /* now a and b have same signbit and same length */
    uint i;
    /* since i is a unsigned  int i >= 0 will always be true
     *  so i replace it with ~i != 0  terminated when i = -1
     * */
    for (i = a->lastdigit; ~i != 0; i--) {
        if (a->digits[i] > b->digits[i]) return POSITIVE * signbit;
        if (a->digits[i] < b->digits[i]) return NEGATIVE * signbit;
    }

    return 0;       /* bignum a and b are the same */
}

void sub_bignum(bignum *, bignum *, bignum *);

void add_bignum(bignum *r, bignum  *a, bignum *b)
{
    initialize_bignum(r);

    if (a->signbit == b->signbit)
        r->signbit =  a->signbit;
    else {
        if (a->signbit == NEGATIVE) {
            a->signbit = POSITIVE;
            sub_bignum(r, b, a);
            a->signbit = NEGATIVE;   /* restore */
        } else {
            b->signbit = POSITIVE;
            sub_bignum(r, a, b);
            b->signbit = NEGATIVE;
        }
        return ;
    }

    /* extra 1 for carry bit */
    int len = r->lastdigit = max(a->lastdigit, b->lastdigit) + 1;
    int carry = 0, i, t;
    for (i = 0; i <= len; i++) {
        t = carry + a->digits[i] + b->digits[i];
        r->digits[i] = (char) t % 10;
        carry = t / 10;
    }

    zero_justify(r);
}

void sub_bignum(bignum *r, bignum *a, bignum *b)
{
    initialize_bignum(r);
    
    /* consider as (-, +) (+, -) (-, -)
     *  (-, +) as to (-, -) final signbit is -
     *  (+, -) as to (+, +) final signbit is +
     *  (-, -) will cause a recursive call
     * */
    if (a->signbit == NEGATIVE || b->signbit == NEGATIVE) {
        b->signbit = -1 * b->signbit;
        add_bignum(r, a, b);
        b->signbit = -1 * b->signbit;   /* restore */
        return ;
    }
    
    /* if a - b < 0 */
    if (compare_bignum(a, b) == NEGATIVE) {
        sub_bignum(r, b, a);
        r->signbit = NEGATIVE;
        return ;
    }

    uint len = r->lastdigit = max(a->lastdigit, b->lastdigit);
    uint i;
    tint borrow = 0, t;
    for (i = 0; i <= len; i++) {
        t = a->digits[i] - b->digits[i] - borrow;
        if (a->digits > 0)  borrow = 0;
        if (t < 0) t += 10, borrow = 1;
        r->digits[i] = (char) t % 10;
    }

    zero_justify(r);
}

/* multiply n by 10^d */
void decimal_shl(bignum *n, uint d)
{
    if (is_zero(n) || d == 0) return ;
    uint i;
    for (i = n->lastdigit; ~i != 0; i--)
        n->digits[i+d] = n->digits[i];
    for (i = 0; i < d; i++)
        n->digits[i] = (char) 0;
    n->lastdigit += d;
}

void mul_bignum(bignum *r, bignum *a, bignum *b)
{
    initialize_bignum(r);   /* initialized to zero */
    /* deal with special cases */
    if (is_zero(a) || is_zero(b)) return ;
    if (is_one(a)) { *r = *b; r->signbit *= a->signbit; return ; }
    if (is_one(b)) { *r = *a; r->signbit *= b->signbit; return ; }

    bignum row = *a, tmp;
    uint i;
    char j;
    /* using addition to i7t multiplication */
    for (i = 0; i <= b->lastdigit; i++) {
        for (j = 1; j <= b->digits[i]; j++) {
            add_bignum(&tmp, r, &row);
            *r = tmp;
        }
        decimal_shl(&row, 1);   /* multiply row by 10 */
    }

    r->signbit = a->signbit * b->signbit;

    zero_justify(r);
}

void div_bignum(bignum *r, bignum *a, bignum *b)
{
    initialize_bignum(r);
    /* if a == 0 || a - b < 0  r must be 0 */
    if (is_zero(a) || compare_bignum(a, b) == NEGATIVE) return ;
    if (is_one(b)) { *r = *a; r->signbit *= b->signbit; return ; }

    r->signbit = a->signbit * b->signbit;

    tint asign = a->signbit, 
         bsign = b->signbit;

    a->signbit = 
    b->signbit = POSITIVE;
    
    bignum row, tmp;
    initialize_bignum(&row);
    initialize_bignum(&tmp);
    
    /* :. a / b its lastdigit deoends on a 
     *  which means that at most length at a->lastdigit
     * note that if a->lastdigit < b->lastdigit will
     *  cause r to zero  e.g. integer division
     * */
    r->lastdigit = a->lastdigit;
    
    /* a inefficient but intuitive a7m */
    uint i;
    for (i = a->lastdigit; ~i != 0; i--) {
        decimal_shl(&row, 1);
        row.digits[0] = a->digits[i];
        r->digits[i]  = 0;
        /* while row - b >= 0 */
        while (compare_bignum(&row, b) != NEGATIVE) {
            r->digits[i]++;
            sub_bignum(&tmp, &row, b);
            row = tmp;
        }
    }

    zero_justify(r);

    a->signbit = asign;     /* restore */
    b->signbit = bsign;
}

#define PS1 "> "

int main(void)
{
    int a, b;
    bignum n1, n2, n;
    
    printf(PS1);
    while (scanf("%d %d", &a, &b) == 2) {
        printf("a  = %d\nb  = %d\n", a, b);
        int_to_bignum(a, &n1);
        int_to_bignum(b, &n2);

        printf("a' = ");
        print_bignum(&n1);
        printf("b' = ");
        print_bignum(&n2);

        printf("compare: %d\n", compare_bignum(&n1, &n2));

        add_bignum(&n, &n1, &n2);
        printf("add: ");
        print_bignum(&n);

        sub_bignum(&n, &n1, &n2);
        printf("sub: ");
        print_bignum(&n);

        mul_bignum(&n, &n1, &n2);
        printf("mul: ");
        print_bignum(&n);

        if (is_zero(&n2))
            printf("div: inf\n");
        else {
            div_bignum(&n, &n1, &n2);
            printf("div: ");
            print_bignum(&n);
        }

        puts("------------------------");
        printf(PS1);
    }

    return 0;
}

