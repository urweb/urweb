cmake_minimum_required(VERSION 3.13)
project(urweb_c VERSION 0.0.0 LANGUAGES C)

##################
# Compiler flags #
##################

set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

add_library("${PROJECT_LOWER_NAME}_compiler_flags" INTERFACE)
target_compile_features("${PROJECT_LOWER_NAME}_compiler_flags" INTERFACE "c_std_90")

# add compiler warning flags just when building this project via
# the BUILD_INTERFACE genex
set(gcc_like "$<COMPILE_LANG_AND_ID:C,CXX,ARMClang,AppleClang,Clang,GNU>")
set(msvc "$<COMPILE_LANG_AND_ID:C,CXX,MSVC>")
# -g -fsanitize=address -fno-omit-frame-pointer
target_compile_options(
        "${PROJECT_LOWER_NAME}_compiler_flags"
        INTERFACE
        "$<${gcc_like}:$<BUILD_INTERFACE:-Wall;-Wextra;-Wshadow;-Wformat=2;-Wunused>>"
        # Zc:forScope
        "$<${msvc}:$<BUILD_INTERFACE:-W3;-WX;-Zi;-permissive-;-GS;-Zc:wchar_t;-Gm-;-Od;-Zc:inline;-fp:precise;-RTC1;-Gd;-MDd;-FC;-EHsc>>"
)
if (MSVC)
    # /D "_WINDLL" /D "_MBCS"
    add_compile_options("$<$<CONFIG:RELEASE>:-MD>")
    add_compile_options("$<$<CONFIG:DEBUG>:-MDd>")
    add_link_options("$<$<CONFIG:RELEASE>:-OPT:REF;-OPT:ICF>")
endif (MSVC)

################################################################################
# Set target arch type if empty. Visual studio solution generator provides it.
################################################################################
if(NOT CMAKE_VS_PLATFORM_NAME)
    set(CMAKE_VS_PLATFORM_NAME "x64")
endif()
message("${CMAKE_VS_PLATFORM_NAME} architecture in use")

####################
# Output directory #
####################

# control where the static and shared libraries are built so that on windows
# we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")

#    # First for the generic no-config case (e.g. with mingw)
#    set( CMAKE_RUNTIME_OUTPUT_DIRECTORY "$ENV{PREMSDKBUILDPATH}" )
#    set( CMAKE_LIBRARY_OUTPUT_DIRECTORY "$ENV{PREMSDKBUILDPATH}" )
#    set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY "$ENV{PREMSDKBUILDPATH}" )
#    # Second, for multi-config builds (e.g. msvc)
#    foreach( OUTPUTCONFIG ${CMAKE_CONFIGURATION_TYPES} )
#        string( TOUPPER ${OUTPUTCONFIG} OUTPUTCONFIG )
#        set( CMAKE_RUNTIME_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "$ENV{PREMSDKBUILDPATH}" )
#        set( CMAKE_LIBRARY_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "$ENV{PREMSDKBUILDPATH}" )
#        set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY_${OUTPUTCONFIG} "$ENV{PREMSDKBUILDPATH}" )
#    endforeach( OUTPUTCONFIG CMAKE_CONFIGURATION_TYPES )

##################
# Create targets #
##################

set(global_include_dir ../../include/urweb)
include_directories("${global_include_dir}")

set(targets "cgi" "fastcgi" "http" "static")

foreach(EXEC_NAME ${targets})
    if (EXEC_NAME STREQUAL "cgi")
        #######
        # CGI #
        #######

        #set("${EXEC_NAME}Header_Files" "urweb.c")
        set("${EXEC_NAME}Source_Files" "cgi.c")

    elseif (EXEC_NAME STREQUAL "fastcgi")
        ###########
        # FastCGI #
        ###########

        #set("${EXEC_NAME}Header_Files" "urweb.c")
        set("${EXEC_NAME}Source_Files" "fastcgi.c")

    elseif (EXEC_NAME STREQUAL "http")
        ########
        # HTTP #
        ########

        #set("${EXEC_NAME}Header_Files" "urweb.c")
        set("${EXEC_NAME}Source_Files" "http.c")

    elseif (EXEC_NAME STREQUAL "static")
        ##########
        # static #
        ##########
        message(STATUS "GOT HERE")

        #set("${EXEC_NAME}Header_Files" "urweb.c")
        set("${EXEC_NAME}Source_Files" "static.c")

    endif ()

    message(STATUS "${EXEC_NAME}Header_Files = ${${EXEC_NAME}Header_Files}")
    message(STATUS "${EXEC_NAME}Source_Files = ${${EXEC_NAME}Source_Files}")

    #source_group("${EXEC_NAME} Header Files" FILES "${EXEC_NAME}Header_Files")
    source_group("${EXEC_NAME} Source Files" FILES "${EXEC_NAME}Source_Files")

    add_executable(
            "${EXEC_NAME}"
            #"${EXEC_NAME}Header_Files"
            "${EXEC_NAME}Source_Files"
    )

    target_link_libraries("${EXEC_NAME}" PUBLIC "${PROJECT_LOWER_NAME}_compiler_flags")

    set_target_properties(
            "${EXEC_NAME}"
            PROPERTIES
            LINKER_LANGUAGE
            C
    )

    ###########
    # Install #
    ###########

    # install rules
    set(installable_libs "${EXEC_NAME}" "${PROJECT_LOWER_NAME}_compiler_flags")

    install(TARGETS ${installable_libs}
            DESTINATION "bin"
            EXPORT "${EXEC_NAME}Targets")
endforeach()
