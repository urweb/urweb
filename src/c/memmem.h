#ifndef URWEB_MEMMEM_H
#define URWEB_MEMMEM_H

#include "config.h"

#ifdef HAVE_MEMMEM

#include <string.h>

#else  // !defined(HAVE_MEMMEM)

#include <stddef.h>

/*
 * memmem() returns the location of the first occurence of data
 * pattern b2 of size len2 in memory block b1 of size len1 or
 * NULL if none is found.
 */
void *memmem(const void *b1, size_t len1, const void *b2, size_t len2);

#endif  // !defined(HAVE_MEMMEM)

#endif  // URWEB_MEMMEM_H
