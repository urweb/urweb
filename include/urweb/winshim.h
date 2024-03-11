#if (defined(_WIN32) || defined(__WIN32__) || defined(__WINDOWS__) && !defined(URWEB_C_WINSHIM_H))
#define URWEB_C_WINSHIM_H

#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
#define strcasecmp _stricmp

#endif /* URWEB_C_WINSHIM_H */
