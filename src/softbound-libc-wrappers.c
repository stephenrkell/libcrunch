#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <dirent.h>
#include "libcrunch_private.h"
#include "libcrunch_cil_inlines.h"

/* This is a rewrite of softboundcets-wrappers.c, by Santosh Nagarakatte 
 * and Milo M. K. Martin. It reproduces most of the libc wrapping logic
 * used by SoftBound, but using libcrunch's internal APIs, for use by the
 * SoftBound emulation mode of crunchcc (crunchsbcc). 
 *
 * Copyright 2016, Stephen Kell
 */

#define MANGLE(sym) __wrap_ ## sym
#define REAL(sym) __real_ ## sym
#define DECLARE(ret, name, ...) \
    ret REAL(name)(__VA_ARGS__); \
	ret MANGLE(name)(__VA_ARGS__)
#define BEGIN(name) _Bool caller_is_inst = __tweak_argument_bounds_cookie(MANGLE( name ))
#define RETURN_PTR(p, base, limit) __push_result_bounds_base_limit(caller_is_inst, (p), \
		(uintptr_t) (base), (uintptr_t) (limit)); return ret_ptr
#define RETURN_PTR_ARGBOUNDS(p, off, argname) __push_local_result_bounds(caller_is_inst, \
		__peek_argument_bounds(caller_is_inst, (off), argname, "argument " #argname )); \
		return ret_ptr
#define RETURN_NULL __push_result_bounds_base_limit(caller_is_inst, NULL, 0, 1); return NULL

/* Some common wrapper pattens:
 * 
 * - provide expressions for the return bounds;
 * - propagate an argument bound to the return bounds; 
 */

DECLARE(FILE*, tmpfile, void)
{
	BEGIN(tmpfile);
	void* ret_ptr = REAL(tmpfile)();
	RETURN_PTR(ret_ptr, ret_ptr, (char*) ret_ptr + sizeof(FILE));
}

DECLARE(DIR*, fdopendir, int fd)
{
	BEGIN(fdopendir);
	void* ret_ptr = REAL(fdopendir)(fd);
	RETURN_PTR(ret_ptr, ret_ptr, (char*) ret_ptr + /* HACK */ 1024 * 1024);
}

DECLARE(char*, strchr, const char *s, int c)
{
	BEGIN(strchr);
	char *ret_ptr = REAL(strchr)(s, c);
	if (ret_ptr)
	{
		RETURN_PTR_ARGBOUNDS(ret_ptr, 0, s);
	}
	else
	{
		RETURN_NULL;
	}
}

#if defined(__linux__)

DECLARE(int*, __errno_location, void)
{
	BEGIN(__errno_location);
	int *ret = REAL(__errno_location)();
	RETURN_PTR(ret, ret, (int*) ret + 1);
}

#endif

DECLARE(short const **, __ctype_b_loc, void)
{
	BEGIN(__ctype_b_loc);
	short const** ret_ptr = REAL(__ctype_b_loc)();
	RETURN_PTR(ret_ptr, ret_ptr, (char*) ret_ptr + /* HACK */ 1024 * 1024);
}

DECLARE(int **, __ctype_toupper_loc, void)
{
	BEGIN(__ctype_toupper_loc);
	int ** ret_ptr = REAL(__ctype_toupper_loc)();
	RETURN_PTR(ret_ptr, ret_ptr, (char*) (ret_ptr + 1));
}

DECLARE(int **, __ctype_tolower_loc, void)
{
	BEGIN(__ctype_tolower_loc);
	int ** ret_ptr = REAL(__ctype_tolower_loc)();
	RETURN_PTR(ret_ptr, ret_ptr, (char*) (ret_ptr + 1));
}
// 
// __WEAK_INLINE int const**  softboundcets___ctype_tolower_loc(void) {
//   
//   int const ** ret_ptr  =  __ctype_tolower_loc();  
//   __softboundcets_store_return_metadata((void*) ret_ptr, 
//                                         (void*) ((char*)ret_ptr + 1024*1024),
//                                         1, __softboundcets_global_lock);
//   return ret_ptr;
// 
// }
// #endif
// 
// __WEAK_INLINE FILE* softboundcets_fopen(const char* path, const char* mode){                                  
// 
//   void* ret_ptr = (void*) fopen(path, mode);
//   void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);
// 
//   __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound, 
//                                         1, (void*) __softboundcets_global_lock);
//   return (FILE*)ret_ptr;
// }
// 
// // return arg
// __WEAK_INLINE  char * softboundcets_mkdtemp(char *template){
//   
//   char* ret_ptr = mkdtemp(template);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// // return exprs
// __WEAK_INLINE struct lconv *softboundcets_localeconv(void){
//   struct lconv* temp = localeconv();
// 
//   __softboundcets_store_return_metadata(temp, temp + 1024, 
//                                         1, (void*) __softboundcets_global_lock);
//   
//   return temp;
// }
// 
// __WEAK_INLINE struct tm *softboundcets_gmtime(const time_t *timep){
// 
//   struct tm * temp = gmtime(timep);
// 
//   __softboundcets_store_return_metadata(temp, temp + 1024, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return temp;
// }
// 
// __WEAK_INLINE void *
// softboundcets_bsearch(const void *key, const void *base,
//                       size_t nmemb, size_t size,
//                       int (*compar)(const void *, const void *)){
//   
//   void* ret_ptr = bsearch(key, base, nmemb, size, compar);
// 
//   __softboundcets_propagate_metadata_shadow_stack_from(2, 0);
//     return ret_ptr;
// 
// }
// 
// __WEAK_INLINE 
// struct group *softboundcets_getgrnam(const char *name){
//   void* ret_ptr = getgrnam(name);
//   __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return ret_ptr;  
// }
// 
// __WEAK_INLINE 
// struct passwd * softboundcets_getpwnam(const char *name){
//   void* ret_ptr = getpwnam(name);
//   __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return ret_ptr;  
// 
//   
// }
// 
// 
// __WEAK_INLINE struct passwd *softboundcets_getpwuid(uid_t uid){
//   void* ret_ptr= getpwuid(uid);
// 
//   __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return ret_ptr;  
// }
// 
// __WEAK_INLINE struct group *softboundcets_getgrgid(gid_t gid){
//   
//   void* ret_ptr = getgrgid(gid);
//   __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return ret_ptr;  
// 
// }
// 
// 
// 
// 
// __WEAK_INLINE FILE* softboundcets_fdopen(int fildes, const char* mode){
// 
//   void* ret_ptr = (void*) fdopen(fildes, mode);
//   void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);
// 
//   __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound, 
//                                         1, (void*)__softboundcets_global_lock);
//   return (FILE*)ret_ptr;
// }
// 
// 
// __WEAK_INLINE FILE* softboundcets_popen(const char* command, const char* type){
// 
//   void* ret_ptr = (void*) popen(command, type);
//   void* ret_ptr_bound = (char*)ret_ptr + sizeof(FILE);
// 
//   __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound, 
//                                         1, (void*) __softboundcets_global_lock);  
//   return (FILE*)ret_ptr;
// 
// }
// 
// __WEAK_INLINE struct dirent*  softboundcets_readdir(DIR* dir){
// 
//   void* ret_ptr = (void*) readdir(dir);
//   void* ret_ptr_bound = (char*)ret_ptr + sizeof(struct dirent);
// 
//   __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound, 
//                                         1, (void*) __softboundcets_global_lock);
// 
//   return (struct dirent*)ret_ptr;
// 
// }
// 
// 
// __WEAK_INLINE DIR* softboundcets_opendir(const char* name){
// 
//   void* ret_ptr = opendir(name);
// 
//   /* FIX Required, don't know the sizeof(DIR) */
//   void* ret_ptr_bound = (char*) ret_ptr + 1024* 1024;
// 
//   __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound, 
//                                         1,  (void*)__softboundcets_global_lock);
// 
//   return (DIR*)ret_ptr;
// }
// 
// 
// __WEAK_INLINE char* softboundcets_getcwd(char* buf, size_t size){ 
// 
//   if(buf == NULL) {
//     printf("This case not handled, requesting memory from system\n");
//     __softboundcets_abort();
//   }
// 
// #ifdef __SOFTBOUNDCETS_SPATIAL
//   
//   char* base = (char*)__softboundcets_load_base_shadow_stack(1);
//   char* bound = (char*)__softboundcets_load_bound_shadow_stack(1);
//   
//   if (buf < base || buf + size > bound){
//     __softboundcets_printf("[getcwd], overflow in buf in getcwd\n");
//     __softboundcets_abort();
//   }
//   
// #endif
// 
// #ifdef __SOFTBOUNDCETS_SPATIAL_TEMPORAL
//   char* base = (char *)__softboundcets_load_base_shadow_stack(1);
//   char* bound = (char *) __softboundcets_load_bound_shadow_stack(1);
//   
//   if (buf < base || buf + size > bound){
//     __softboundcets_printf("[getcwd], overflow in buf in getcwd\n");
//     __softboundcets_abort();
//   }
// 
// 
// #endif
// 
//   char* ret_ptr = getcwd(buf, size);
// 
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
// 
//   return ret_ptr;
// }
// 
// 
// __WEAK_INLINE char* softboundcets_strpbrk(const char* s, const char* accept){ 
// 
//   char* ret_ptr = strpbrk(s, accept);
//   if(ret_ptr != NULL) {
// 
//     __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   }
//   else {
// 
//     __softboundcets_store_null_return_metadata();
//   }
// 
//   return ret_ptr;
// }
// 
// 
// 
// __WEAK_INLINE char* softboundcets_gets(char* s){ 
// 
//   printf("[SBCETS] gets used and should not be used\n");
//   __softboundcets_abort();
// #if 0
//   printf("[Softboundcets][Warning] Should not use gets\n");
//   char* ret_ptr = gets(s);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// #endif
//   return NULL;
// 
// }
// 
// __WEAK_INLINE char* softboundcets_fgets(char* s, int size, FILE* stream){
// 
//   char* ret_ptr = fgets(s, size, stream);
//   __softboundcets_propagate_metadata_shadow_stack_from(1,0);
// 
//   return ret_ptr;
// }
// 
// 
// #ifdef _GNU_SOURCE
// 
// __WEAK_INLINE void* softboundcets_mempcpy(void * dest, const void * src, size_t n){
// 
//   // IMP: need to copy the metadata 
//   void* ret_ptr = mempcpy(dest, src, n);
//   __softboundcets_propagate_metadata_shadow_stack_from(1,0);
//   return ret_ptr;
// }
// 
// #endif
// 
// #ifdef _GNU_SOURCE
// 
// __WEAK_INLINE void* softboundcets_memrchr(const void * s, int c, size_t n){  
//   void* ret_ptr = memrchr(s, c, n);
//   if(ret_ptr != NULL) {
//     __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   }
//   else{
//     __softboundcets_store_null_return_metadata();
//   }
//   return ret_ptr;
// }
// #endif
// 
// 
// __WEAK_INLINE void* softboundcets_memchr(const void * s, int c, size_t n){  
//   void* ret_ptr = memchr(s, c, n);
//   if(ret_ptr != NULL) {
//     __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   }
//   else{
//     __softboundcets_store_null_return_metadata();
//   }
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* softboundcets_rindex(char* s, int c){
// 
//   char* ret_ptr = rindex(s,c);
//   __softboundcets_propagate_metadata_shadow_stack_from(1,0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE ssize_t 
// softboundcets___getdelim(char **lineptr, size_t *n, int delim, FILE *stream){
//   
//   int metadata_prop = 1;
//   if(*lineptr == NULL){
//     metadata_prop = 0;
//   }
// 
//   ssize_t ret_val = getdelim(lineptr, n, delim, stream);
//   
//   if(metadata_prop){
//     __softboundcets_read_shadow_stack_metadata_store(lineptr, 1);
//   }
//   else{
//     __softboundcets_store_return_metadata(*lineptr, 
//                                           (*lineptr) + strlen(*lineptr),
//                                           1, __softboundcets_global_lock);
//   }
//   
//   return ret_val;
//   
// 
// }
// 
// __WEAK_INLINE unsigned long int 
// softboundcets_strtoul(const char* nptr, char ** endptr, int base){
// 
//   unsigned long temp = strtoul(nptr, endptr, base);
//   if(endptr != NULL){
//     __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
//     
//   }
// 
//   return temp;
// }
// 
// __WEAK_INLINE double softboundcets_strtod(const char* nptr, char** endptr){
// 
//   double temp = strtod(nptr, endptr);
//   
//   if(endptr != NULL) {
//     __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
//   }
//   return temp;
//  }
//  
// __WEAK_INLINE long 
// softboundcets_strtol(const char* nptr, char **endptr, int base){
//  
//    long temp = strtol(nptr, endptr, base);
//    if(endptr != NULL) {
//      //    __softboundcets_printf("*endptr=%p\n", *endptr);
//      __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
//   }
//   return temp;
// }
// 
// #ifdef _GNU_SOURCE
// 
// __WEAK_INLINE char* softboundcets_strchrnul(const char* s, int c){
// 
//   char* ret_ptr = strchrnul(s, c);
//    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//    return ret_ptr;
// }
// #endif
// 
// 
// __WEAK_INLINE char* softboundcets_strrchr(const char* s, int c){
// 
//   char* ret_ptr = strrchr(s, c);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* softboundcets_stpcpy(char* dest, char* src){
// 
//   void* ret_ptr = stpcpy(dest, src);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* softboundcets_strcpy(char* dest, char* src){
//   
// #ifdef __SOFTBOUNDCETS_SPATIAL  
//   char* dest_base = __softboundcets_load_base_shadow_stack(1);
//   char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
// 
//   char* src_base = __softboundcets_load_base_shadow_stack(2);
//   char* src_bound = __softboundcets_load_bound_shadow_stack(2);
// 
//   /* There will be an out-of-bound read before we trigger an error as
//      we currently use strlen. Can either (dest + size) or (src + size)
//      overflow?
//   */
//   size_t size = strlen(src);
//   if(dest < dest_base || (dest > dest_bound - size -1) || (size > (size_t) dest_bound)){
//     printf("[strcpy] overflow in strcpy with dest\n");
//     __softboundcets_abort();
//   }  
//   if(src < src_base || (src > src_bound -size -1) || (size > (size_t) src_bound)){
//     printf("[strcpy] overflow in strcpy with src\n");
//     __softboundcets_abort();
//   }
// #endif
// 
// #ifdef __SOFTBOUNDCETS_SPATIAL_TEMPORAL
//   
//   char* dest_base = __softboundcets_load_base_shadow_stack(1);
//   char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
// 
//   char* src_base = __softboundcets_load_base_shadow_stack(2);
//   char* src_bound = __softboundcets_load_bound_shadow_stack(2);
// 
//   /* There will be an out-of-bound read before we trigger an error as
//      we currently use strlen. Can either (dest + size) or (src + size)
//      overflow?
//   */
// #ifndef __NOSIM_CHECKS
//   size_t size = strlen(src);
//   if(dest < dest_base || (dest > dest_bound - size -1) || (size > (size_t) dest_bound)){
//     printf("[strcpy] overflow in strcpy with dest\n");
//     __softboundcets_abort();
//   }  
//   if(src < src_base || (src > src_bound -size -1 ) || (size > (size_t) src_bound)){
//     printf("[strcpy] overflow in strcpy with src\n");
//     __softboundcets_abort();
//   }
// #endif
// #endif
//         
//   void * ret_ptr = strcpy(dest, src);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// 
//  __WEAK_INLINE int softboundcets_atoi(const char* ptr){
// 
//   if(ptr == NULL) {
//     __softboundcets_abort();
//   }
//   return atoi(ptr);    
//  }
// 
// __WEAK_INLINE char*  softboundcets_strtok(char* str, const char* delim){
//   
//   char* ret_ptr = strtok(str, delim);   
//   __softboundcets_store_return_metadata((void*)0, (void*)(281474976710656), 
//                                         1, __softboundcets_global_lock);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE void __softboundcets_strdup_handler(void* ret_ptr){
//   key_type ptr_key;
//   lock_type ptr_lock;
//   
//   if(ret_ptr == NULL) {
//     __softboundcets_store_null_return_metadata();
//   }
//   else {
//     //    printf("strndup malloced pointer %p\n", ret_ptr);    
//     __softboundcets_memory_allocation(ret_ptr, &ptr_lock, &ptr_key);
//     __softboundcets_store_return_metadata(ret_ptr, 
//                                           (void*)
//                                           ((char*)ret_ptr + strlen(ret_ptr) + 1), 
//                                           ptr_key, ptr_lock); 
//   } 
// }
// 
// //strdup, allocates memory from the system using malloc, thus can be freed
// __WEAK_INLINE char* softboundcets_strndup(const char* s, size_t n){
//   
//   /* IMP: strndup just copies the string s */  
//   char* ret_ptr = strndup(s, n);
//   __softboundcets_strdup_handler(ret_ptr);  
//   return ret_ptr;
//  }
// 
// 
// //strdup, allocates memory from the system using malloc, thus can be freed
// __WEAK_INLINE char* softboundcets_strdup(const char* s){
//   
//   /* IMP: strdup just copies the string s */  
//   void* ret_ptr = strdup(s);
//   
//   __softboundcets_strdup_handler(ret_ptr);
//   return ret_ptr;
//  }
// 
// __WEAK_INLINE char* softboundcets___strdup(const char* s){
// 
//   void* ret_ptr = strdup(s);
//   __softboundcets_strdup_handler(ret_ptr);
//   return ret_ptr;
// }
// 
// 
//  __WEAK_INLINE char* softboundcets_strcat (char* dest, const char* src){
// 
// #if 0
//   if(dest + strlen(dest) + strlen(src) > dest_bound){
//     printf("overflow with strcat, dest = %p, strlen(dest)=%d, 
//             strlen(src)=%d, dest_bound=%p \n", 
//            dest, strlen(dest), strlen(src), dest_bound);
//     __softboundcets_abort();
//   } 
// #endif
//   
//   char* ret_ptr = strcat(dest, src);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* 
// softboundcets_strncat (char* dest,const char* src, size_t n){
// 
//   char* ret_ptr = strncat(dest, src, n);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* 
// softboundcets_strncpy(char* dest, char* src, size_t n){
//  
// #ifdef __SOFTBOUNDCETS_SPATIAL  
//   char* dest_base = __softboundcets_load_base_shadow_stack(1);
//   char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
// 
//   char* src_base = __softboundcets_load_base_shadow_stack(2);
//   char* src_bound = __softboundcets_load_bound_shadow_stack(2);
// 
//   /* Can either (dest + n) or (src + n) overflow? */
//   if(dest < dest_base || (dest > dest_bound - n) || (n > (size_t) dest_bound)){
//     printf("[strncpy] overflow in strncpy with dest\n");
//     __softboundcets_abort();
//   }  
//   if(src < src_base || (src > src_bound -n) || (n > (size_t) src_bound)){
//     __softboundcets_abort();
//   }
// #endif
// 
// #ifdef __SOFTBOUNDCETS_SPATIAL_TEMPORAL
// 
// #ifdef __SOFTBOUNDCETS_WRAPPER_CHECKS
// 
//   char* dest_base = __softboundcets_load_base_shadow_stack(1);
//   char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
// 
//   char* src_base = __softboundcets_load_base_shadow_stack(2);
//   char* src_bound = __softboundcets_load_bound_shadow_stack(2);
// 
//   /* Can either (dest + n) or (src + n) overflow? */
//   if(dest < dest_base || dest + n > dest_bound){
//     printf("[strncpy] overflow in strncpy with dest\n");
//     __softboundcets_abort();
//   }  
//   if(src < src_base || src + n > src_bound){
//     //    printf("[strncpy] overflow in strncpy with src, src=%zx, src_base=%zx, src_bound=%zx\n", src, src_base, src_bound);
//     __softboundcets_abort();
//   }
// #endif
// #endif
//   
//   char* ret_ptr = strncpy(dest, src, n);
//   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* 
// softboundcets_strstr(const char* haystack, const char* needle){
//   
//   char* ret_ptr = strstr(haystack, needle);
//   if(ret_ptr != NULL) {    
//     __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
//   }
//   else {
//     __softboundcets_store_null_return_metadata();
//   }
//   return ret_ptr;
// }
// 
// __WEAK_INLINE sighandler_t 
// softboundcets_signal(int signum, sighandler_t handler){
// 
//   sighandler_t ptr = signal(signum, handler);
//   __softboundcets_store_return_metadata((void*)ptr, (void*) ptr, 
//                                         1, __softboundcets_global_lock);
//   return ptr;
// }
// 
// __WEAK_INLINE void* softboundcets_mmap(void* addr, size_t length, 
//                                        int prot, int flags, int fd, 
//                                        off_t offset){
// 
//   key_type ptr_key=1;
//   lock_type ptr_lock=__softboundcets_global_lock;
//   char* ret_ptr = mmap(addr, length, prot, flags, fd, offset);
//   if(ret_ptr == (void*) -1){
//     __softboundcets_store_null_return_metadata();
//   }
//   else{
// 
//     char* ret_bound = ret_ptr + length;
//     __softboundcets_store_return_metadata(ret_ptr, ret_bound, 
//                                           ptr_key, ptr_lock);
//     
//   }
//   return ret_ptr;
// }
//  
//  
// __WEAK_INLINE struct tm* softboundcets_localtime(const time_t* timep){
// 
//   struct tm * ret_ptr = localtime(timep);
//   __softboundcets_store_return_metadata(ret_ptr, 
//                                         (char*)ret_ptr + sizeof(struct tm), 
//                                         1, __softboundcets_global_lock); 
//   return ret_ptr;
// }
// 
// __WEAK_INLINE char* softboundcets_ctime( const time_t* timep){
//   
//   char* ret_ptr = ctime(timep);
// 
//   if(ret_ptr == NULL){
//     __softboundcets_store_null_return_metadata();
//   }
//   else {
//     __softboundcets_store_return_metadata(ret_ptr, ret_ptr + strlen(ret_ptr) + 1, 
//                                           1, __softboundcets_global_lock);
//   }
//   return ret_ptr;
// 
// }
// 
// __WEAK_INLINE char* softboundcets_getenv(const char* name){
//    
//   char* ret_ptr = getenv(name);
//    
//   if(ret_ptr != NULL){
//     __softboundcets_store_return_metadata(ret_ptr, 
//                                           ret_ptr + strlen(ret_ptr) + 1, 
//                                           1, __softboundcets_global_lock);
//   }
//   else {
//     __softboundcets_store_null_return_metadata();
//   }
// 
//   return ret_ptr;
// }
// 
// #ifdef _GNU_SOURCE
// __WEAK_INLINE char* softboundcets_strerror_r(int errnum, char* buf, 
//                                              size_t buf_len) {
// 
//   void* ret_ptr = strerror_r(errnum, buf, buf_len);
//   __softboundcets_store_return_metadata(ret_ptr, 
//                                         (void*)
//                                         ((char*)ret_ptr + strlen(ret_ptr) +1),
//                                         1, __softboundcets_global_lock);
//   return ret_ptr;
// }
// #endif
// 
// __WEAK_INLINE char* softboundcets_strerror(int errnum) {
// 
//   void* ret_ptr = strerror(errnum);
//   __softboundcets_store_return_metadata(ret_ptr, 
//                                         (void*)
//                                         ((char*)ret_ptr + strlen(ret_ptr) +1),
//                                         1, __softboundcets_global_lock);
//   return ret_ptr;
// }
// 
// #if defined (__linux__)
// 
// __WEAK_INLINE char* 
// softboundcets_setlocale(int category, const char* locale){
//   
//   void* ret_ptr = setlocale(category, locale);
//   __softboundcets_store_return_metadata(ret_ptr, 
//                                         (void*) 
//                                         ((char*) ret_ptr+ strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
//   return ret_ptr;  
// 
// }
// 
// __WEAK_INLINE char*
// softboundcets_textdomain(const char* domainname){
//   
//   void* ret_ptr = textdomain(domainname);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         (void *)
//                                         ((char*) ret_ptr + strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
//   
//   return ret_ptr;
//   
// }
// 
// 
// __WEAK_INLINE char*
// softboundcets_bindtextdomain(const char* domainname, const char* dirname){
//   
//   void* ret_ptr = bindtextdomain(domainname, dirname);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         (void *)
//                                         ((char*) ret_ptr + strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
// 
//   return ret_ptr;
// 
// }
// 
// __WEAK_INLINE char * 
// softboundcets_gettext(const char * msgid){
//   
//   void* ret_ptr = gettext(msgid);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         (void*)
//                                         ((char*) ret_ptr + strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
// 
//   return ret_ptr;  
// 
// }
// 
// 
// __WEAK_INLINE char * 
// softboundcets_dcngettext (const char * domainname,
//                           const char * msgid, const char * msgid_plural,
//                           unsigned long int n, int category){
//   
//   void* ret_ptr = dcngettext(domainname, msgid, msgid_plural, n, category);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         (void*)
//                                         ((char*) ret_ptr + strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
// 
//   return ret_ptr;  
//     
// }
// 
// 
// /* IMP: struct hostent may have pointers in the structure being returned,
//    we need to store the metadata for all those pointers */
// __WEAK_INLINE 
// struct hostent * softboundcets_gethostbyname(const char *name){
//   
//   struct hostent * ret_ptr = gethostbyname(name);
// 
//   void* ret_bound = ret_ptr + sizeof(struct hostent);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         ret_bound,
//                                         1, __softboundcets_global_lock);
//   
//   return ret_ptr;  
// }
// 
// 
// 
// __WEAK_INLINE char*
// softboundcets_dcgettext (const char * domainname, 
//                          const char * msgid,
//                          int category) {
// 
//   void* ret_ptr = dcgettext(domainname, msgid, category);
//   __softboundcets_store_return_metadata(ret_ptr,
//                                         (void*)
//                                         ((char*) ret_ptr + strlen(ret_ptr)),
//                                         1, __softboundcets_global_lock);
// 
//   return ret_ptr;  
//   
// }
// 
// #endif
// 
// 
// /* This is a custom implementation of qsort */
// 
// static int 
// compare_elements_helper(void* base, size_t element_size, 
//                         int idx1, int idx2, 
//                         int (*comparer)(const void*, const void*)){
//   
//   char* base_bytes = base;
//   return comparer(&base_bytes[idx1 * element_size], 
//                   &base_bytes[idx2*element_size]);
// }
// 
// #define element_less_than(i,j) (compare_elements_helper(base, element_size, (i), (j), comparer) < 0)
// 
// static void 
// exchange_elements_helper(void* base, size_t element_size, 
//                          int idx1, int idx2){
// 
//   char* base_bytes = base;
//   size_t i;
// 
//   for (i=0; i < element_size; i++){
//     char temp = base_bytes[idx1* element_size + i];
//     base_bytes[idx1 * element_size + i] = base_bytes[idx2 * element_size + i];
//     base_bytes[idx2 * element_size + i] = temp;
//   }  
// 
//   for(i=0; i < element_size; i+= 8){
//     void* base_idx1;
//     void* bound_idx1;
// 
//     void* base_idx2;
//     void* bound_idx2;
// 
//     size_t key_idx1=1;
//     size_t key_idx2=1;
// 
//     void* lock_idx1=NULL;
//     void* lock_idx2=NULL;
// 
//     char* addr_idx1 = &base_bytes[idx1 * element_size + i];
//     char* addr_idx2 = &base_bytes[idx2 * element_size + i];
// 
//     //    printf("addr_idx1= %p, addr_idx2=%p\n", addr_idx1, addr_idx2);
// 
// #ifdef __SOFTBOUNDCETS_SPATIAL
//     __softboundcets_metadata_load(addr_idx1, &base_idx1, &bound_idx1);
//     __softboundcets_metadata_load(addr_idx2, &base_idx2, &bound_idx2);
// 
//     __softboundcets_metadata_store(addr_idx1, base_idx2, bound_idx2);
//     __softboundcets_metadata_store(addr_idx2, base_idx1, bound_idx1);
// 
// #elif __SOFTBOUNDCETS_TEMPORAL
// 
//     __softboundcets_metadata_load(addr_idx1, &key_idx1, &lock_idx1);
//     __softboundcets_metadata_load(addr_idx2, &key_idx2, &lock_idx2);
// 
//     __softboundcets_metadata_store(addr_idx1, key_idx2, lock_idx2);
//     __softboundcets_metadata_store(addr_idx2, key_idx1, lock_idx1);
// 
// #elif __SOFTBOUNDCETS_SPATIAL_TEMPORAL
//     
//     __softboundcets_metadata_load(addr_idx1, &base_idx1, &bound_idx1, 
//                                   &key_idx1, &lock_idx1);
//     __softboundcets_metadata_load(addr_idx2, &base_idx2, &bound_idx2, 
//                                   &key_idx2, &lock_idx2);
// 
//     __softboundcets_metadata_store(addr_idx1, base_idx2, bound_idx2, 
//                                    key_idx2, lock_idx2);
//     __softboundcets_metadata_store(addr_idx2, base_idx1, bound_idx1, 
//                                    key_idx1, lock_idx1);
// 
// #else
//     __softboundcets_printf("not implemented\n");
//     __softboundcets_abort();
// #endif
//         
//   }
// 
// }
// 
// #define exchange_elements(i,j) (exchange_elements_helper(base, element_size, (i), (j)))
// 
// #define MIN_QSORT_LIST_SIZE 32
// 
// __WEAK__ 
// void my_qsort(void* base, size_t num_elements, 
//               size_t element_size, 
//               int (*comparer)(const void*, const void*)){
// 
//   size_t i;
// 
//   for(i = 0; i < num_elements; i++){
//     int j;
//     for (j = i - 1; j >= 0; j--){      
//       if(element_less_than(j, j + 1)) break;
//       exchange_elements(j, j + 1);
//     }
//   }
//   /* may be implement qsort here */
// 
// }
// 
// 
// __WEAK_INLINE void 
// softboundcets_qsort(void* base, size_t nmemb, size_t size, 
//                     int (*compar)(const void*, const void*)){
// 
//   my_qsort(base, nmemb, size, compar);
// }
// 
// __WEAK_INLINE 
// char * softboundcets_nl_langinfo(nl_item item){
//   
//   char* ret_ptr = nl_langinfo(item);
// 
//   __softboundcets_store_return_metadata(ret_ptr, 
//                                         ret_ptr + 1024 * 1024,
//                                         1, __softboundcets_global_lock);
//   
//   return ret_ptr;
// }
// 
// 
