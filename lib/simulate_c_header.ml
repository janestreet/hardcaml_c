let header =
  Rope.of_string
    {|
#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/mman.h>

static void long_add(uint32_t* dst, uint32_t* a, uint32_t *b, int width) {
  uint64_t carry = 0;
  int index = 0;

  while (width > 0) {
    uint64_t x = (uint64_t) a[index] + (uint64_t) b[index] + carry;
    dst[index] = (uint32_t) x;
    carry = x >> 32;
    if (width < 32) {
        dst[index] &= (1u<<width)-1;
    }
    index++;
    width -= 32;
  }
}

static void long_sub(uint32_t* dst, uint32_t* a, uint32_t *b, int width) {
  uint64_t borrow = 0;
  int index = 0;

  while (width > 0) {
    uint64_t x = (uint64_t) a[index] - (uint64_t) b[index] - borrow;
    dst[index] = (uint32_t) x;
    borrow = (x >> 32) & 1;
    if (width < 32) {
        dst[index] &= (1u<<width)-1;
    }
    index++;
    width -= 32;
  }
}

static int long_lt(uint64_t* a, uint64_t* b, int width) {
  int index = (width + 63) >> 6;
  index --;

  while (index >= 0) {
    if (a[index] < b[index]) { return 1; }
    if (a[index] > b[index]) { return 0; }
    index --;
  }
  return 0;
}

static void copy_bits(uint64_t *dst, uint64_t *src, int width) {
  int index = 0;
  while (width > 0) {
    dst[index] = src[index];
    if (width < 64) {
        dst[index] &= (1ull<<width)-1;
    }
    index++;
    width -= 64;
  }
}

/* Test bit at (width-1) and return bits that should be or'd with [a] to sign
 * extend it. */
static uint64_t sign_mask(int width, uint64_t *a) {
  int word;
  int bit;
  int is_negative;

  width = width - 1;
  word = width >> 6;
  bit = width & 0x3f;
  is_negative = (a[word] >> bit) & 1;

  return is_negative ? ((uint64_t) 0xFFFFFFFFFFFFFFFF) << bit : 0;
}

// the following is copied from hardcaml/src/bits_stub.c:
/* Multiword unsigned multiplication done 32 bits per iteration.
 *
 * This is obfuscated due to a direct translation from Fortran.
 *
 * [w] is the destination, and [u], [v] are inputs with corresponding lengths
 * [m], [n].
 * */
static void mulu(uint32_t *w, uint32_t *u, uint32_t *v, int m, int n)
{ uint64_t k, t; int i, j;

  for (j = 0; j < n; j++) {
    k = 0;
    for (i = 0; i < m; i++) {
      t = ((uint64_t) u[i] * (uint64_t) v[j]) + (uint64_t) w[i + j] + k;
      w[i + j] = t;
      k = t >> 32;
    }
    w[j + m] = k;
  }
  return;
}

/* Multiword signed multiplication - corrects the unsigned result. Inputs must
 * be sign extended to 32 bits.
 *
 * Arguments are the same as [mulu].
 * */
static void muls(uint32_t *w, uint32_t *u, uint32_t *v, int m, int n) {
  uint64_t t, b;
  int i, j;

  mulu(w, u, v, m, n);

  if ((int32_t)u[m - 1] < 0) {
    b = 0;
    for (j = 0; j < n; j++) {
      t = (uint64_t) w[j + m] - (uint64_t) v[j] - b;
      w[j + m] = t;
      b = t >> 63;
    }
  }
  if ((int32_t)v[n - 1] < 0) {
    b = 0;
    for (i = 0; i < m; i++) {
      t = (uint64_t) w[i + n] - (uint64_t) u[i] - b;
      w[i + n] = t;
      b = t >> 63;
    }
  }
  return;
}

/* Unsigned multiplication.  Result is computed in a temporary array on the
 * stack as the output width may not be exactly the width required for the
 * computation. */
static void long_mulu(uint64_t *dst, uint64_t *_a, uint64_t *_b,
                      int width_a, int width_b) {
  int words_a = (width_a + 63) >> 6;
  int words_b = (width_b + 63) >> 6;
  int words_r = words_a + words_b;
  int words_dst = (width_a + width_b + 63) >> 6; /* not always == words_r */
  /* Allocate temporary result arrays on the stack, clear and initialize them. */
  int bytes_a = sizeof(uint64_t) * words_a;
  int bytes_b = sizeof(uint64_t) * words_b;
  int bytes_r = sizeof(uint64_t) * words_r;
  uint64_t *r = alloca(bytes_r);

  memset(r, 0, bytes_r);

  mulu((uint32_t *) r,
       (uint32_t *) _a,
       (uint32_t *) _b,
       bytes_a >> 2, bytes_b >> 2);

  copy_bits(dst, r, width_a + width_b);
}

/* Signed multiplication. Arguments are copied to temporary arrays so they can be modified
   for signed extension as necesseary. */
static void long_muls(uint64_t * dst, uint64_t * _a, uint64_t * _b,
                      int width_a, int width_b) {
  int words_a = (width_a + 63) >> 6;
  int words_b = (width_b + 63) >> 6;
  int words_r = words_a + words_b;
  int words_dst = (width_a + width_b + 63) >> 6; /* not always == words_r */
  /* Allocate temporary result arrays on the stack, clear and initialize them. */
  int bytes_a = sizeof(uint64_t) * words_a;
  int bytes_b = sizeof(uint64_t) * words_b;
  int bytes_r = sizeof(uint64_t) * words_r;
  uint64_t *a = alloca(bytes_a);
  uint64_t *b = alloca(bytes_b);
  uint64_t *r = alloca(bytes_r);

  uint64_t sign_mask_a;
  uint64_t sign_mask_b;

  memset(r, 0, bytes_r);
  memcpy(a, _a, bytes_a);
  sign_mask_a = sign_mask(width_a, a);
  a[words_a-1] = a[words_a-1] | sign_mask_a;

  memcpy(b, _b, bytes_b);
  sign_mask_b = sign_mask(width_b, b);
  b[words_b-1] = b[words_b-1] | sign_mask_b;

  muls((uint32_t *) r,
       (uint32_t *) a,
       (uint32_t *) b,
       bytes_a >> 2, bytes_b >> 2);

  copy_bits(dst, r, width_a + width_b);
}
|}
;;
