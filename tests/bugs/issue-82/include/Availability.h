/*
 * Copyright (c) 2007-2009 by Apple Inc.. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */
 
#ifndef __AVAILABILITY__
#define __AVAILABILITY__
 /*     
    These macros are for use in OS header files. They enable function prototypes
    and Objective-C methods to be tagged with the OS version in which they
    were first available; and, if applicable, the OS version in which they 
    became deprecated.  
     
    The desktop Mac OS X and the iPhone OS X each have different version numbers.
    The __OSX_AVAILABLE_STARTING() macro allows you to specify both the desktop
    and phone OS version numbers.  For instance:
        __OSX_AVAILABLE_STARTING(__MAC_10_2,__IPHONE_2_0)
    means the function/method was first available on Mac OS X 10.2 on the desktop
    and first available in OS X 2.0 on the iPhone.
    
    If a function is available on one platform, but not the other a _NA (not
    applicable) parameter is used.  For instance:
            __OSX_AVAILABLE_STARTING(__MAC_10_3,__IPHONE_NA)
    means that the function/method was first available on Mac OS X 10.3, and it
    currently not implemented on the iPhone.

    At some point, a function/method may be deprecated.  That means Apple
    recommends applications stop using the function, either because there is a 
    better replacement or the functionality is being phased out.  Deprecated
    functions/methods can be tagged with a __OSX_AVAILABLE_BUT_DEPRECATED()
    macro which specifies the OS version where the function became available
    as well as the OS version in which it became deprecated.  For instance:
        __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0,__MAC_10_5,__IPHONE_NA,__IPHONE_NA)
    means that the function/method was introduced in Mac OS X 10.0, then
    became deprecated beginning in Mac OS X 10.5.  On the iPhone the function 
    has never been available.
    
    For these macros to function properly, a program must specify the OS version range 
    it is targeting.  The min OS version is specified as an option to the compiler:
    -mmacosx-version-min=10.x when building for Mac OS X, and -miphone-version-min=1.x.x
    when building for the iPhone.  The upper bound for the OS version is rarely needed,
    but it can be set on the command line via: -D__MAC_OS_X_VERSION_MAX_ALLOWED=10xx for
    Mac OS X and __IPHONE_OS_VERSION_MAX_ALLOWED = 1xxx for iPhone.  
    
    Examples:

        A function available in Mac OS X 10.5 and later, but not on the phone:
        
            extern void mymacfunc() __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_NA);


        An Objective-C method in Mac OS X 10.5 and later, but not on the phone:
        
            @interface MyClass : NSObject
            -(void) mymacmethod __OSX_AVAILABLE_STARTING(__MAC_10_5,__IPHONE_NA);
            @end

        
        An enum available on the phone in 2.1 and later, but not available on Mac OS X:
        
            #if __IPHONE_OS_VERSION_MIN_REQUIRED >= 20100
                enum { myEnum = 1 };
            #endif
        Note: this works when targeting the Mac OS X platform because 
        __IPHONE_OS_VERSION_MIN_REQUIRED is undefined which evaluates to zero, 
        so test becomes #if 0 >= 20100 which is false.  Also, we use
        20100 instead of __IPHONE_2_1 to be safe.  The __IPHONE_2_1 macro did not
        exist in <Availability.h> prior to the 2.1 SDK.  So, if somehow this 
        conditional was used with an earilier SDK, it would evaluate incorrectly.


    It is also possible to use the *_VERSION_MIN_REQUIRED in source code to make one
    source base that can be compiled to target a range of OS versions.  It is best
    to not use the _MAC_* and __IPHONE_* macros for comparisons, but rather their values.
    That is because you might get compiled on an old OS that does not define a later
    OS version macro, and in the C preprocessor undefined values evaluate to zero
    in expresssions, which could cause the #if expression to evaluate in an unexpected
    way.
    
        #ifdef __MAC_OS_X_VERSION_MIN_REQUIRED
            // code only compiled when targeting Mac OS X and not iPhone
            // note use of 1050 instead of __MAC_10_5
            #if __MAC_OS_X_VERSION_MIN_REQUIRED < 1050
                // code in here might run on pre-Leopard OS
            #else
                // code here can assume Leopard or later
            #endif
        #endif


*/

#define __MAC_10_0      1000
#define __MAC_10_1      1010
#define __MAC_10_2      1020
#define __MAC_10_3      1030
#define __MAC_10_4      1040
#define __MAC_10_5      1050
#define __MAC_10_6      1060
#define __MAC_NA        9999   /* not available */

#define __IPHONE_2_0     20000  
#define __IPHONE_2_1     20100  
#define __IPHONE_2_2     20200  
#define __IPHONE_3_0     30000  
#define __IPHONE_NA      99999  /* not available */

#include <AvailabilityInternal.h>


#ifdef __IPHONE_OS_VERSION_MIN_REQUIRED
    #define __OSX_AVAILABLE_STARTING(_mac, _iphone) __AVAILABILITY_INTERNAL##_iphone
    #define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep) \
                                                    __AVAILABILITY_INTERNAL##_iphoneIntro##_DEP##_iphoneDep

#elif defined(__MAC_OS_X_VERSION_MIN_REQUIRED)
    #define __OSX_AVAILABLE_STARTING(_mac, _iphone) __AVAILABILITY_INTERNAL##_mac
    #define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep) \
                                                    __AVAILABILITY_INTERNAL##_macIntro##_DEP##_macDep

#else
    #define __OSX_AVAILABLE_STARTING(_mac, _iphone)
    #define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep) 
#endif


#endif /* __AVAILABILITY__ */
