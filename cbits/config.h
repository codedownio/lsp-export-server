#ifndef CONFIG_H
#define CONFIG_H

/* Basic configuration for xdelta3 */
#define HAVE_CONFIG_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Size definitions for 64-bit system */
#define SIZEOF_SIZE_T 8
#define SIZEOF_UNSIGNED_INT 4  
#define SIZEOF_UNSIGNED_LONG 8
#define SIZEOF_UNSIGNED_LONG_LONG 8

/* Use 64-bit types */
#define XD3_USE_LARGESIZET 1

/* Disable external compression for simplicity */
#define XD3_DEFAULT_SECONDARY_DJW 0
#define XD3_DEFAULT_SECONDARY_FGK 0  
#define XD3_DEFAULT_SECONDARY_LZMA 0

/* Disable various optional features to simplify build */
#define XD3_ENCODER 1
#define XD3_DECODER 1
#define XD3_STDIO 0
#define XD3_POSIX 0
#define XD3_WIN32 0

/* Type compatibility */
#include <stdint.h>
#include <stddef.h>

/* Fix static_assert issues */
#ifdef __STDC_VERSION__
#if __STDC_VERSION__ >= 201112L
/* C11 has static_assert */
#include <assert.h>
#else
/* Pre-C11, define our own */
#define static_assert(cond, msg) typedef char static_assertion_##__LINE__[(cond)?1:-1]
#endif
#else
/* No __STDC_VERSION__, assume older C */
#define static_assert(cond, msg) typedef char static_assertion_##__LINE__[(cond)?1:-1]
#endif

#endif /* CONFIG_H */