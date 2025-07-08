cmake_minimum_required(VERSION 3.10)
project(beam_egl LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

# We let Rebar3 inform CMake where to find the Erlang/OTP installation.
# Therefore, it requires the ERLANG_ERTS_DIR and ERL_INTERFACE_DIR values
# specified with the -D flags.
if(NOT DEFINED ERLANG_ERTS_DIR)
    message(FATAL_ERROR "ERLANG_ERTS_DIR must be defined (pass -DERLANG_ERTS_DIR=...)")
endif()
if(NOT DEFINED ERL_INTERFACE_DIR)
    message(FATAL_ERROR "ERL_INTERFACE_DIR must be defined (pass -DERL_INTERFACE_DIR=...)")
endif()

set(ERTS_INCLUDE_DIR "${ERLANG_ERTS_DIR}/include")
set(ERL_INTERFACE_INCLUDE_DIR "${ERL_INTERFACE_DIR}/include")
set(ERL_INTERFACE_LIB_DIR "${ERL_INTERFACE_DIR}/lib")

include_directories(
    ${ERTS_INCLUDE_DIR}
    ${ERL_INTERFACE_INCLUDE_DIR}
)
link_directories(${ERL_INTERFACE_LIB_DIR})

# If ANGLE is used, we also need to include its directories.
if(APPLE OR WIN32)
    include_directories(${ANGLE_INCLUDE_DIR})
    link_directories(${ANGLE_LIB_DIR})
    # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DANGLE_ENABLED")
else()
    # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DANGLE_DISABLED")
endif()

set(BEAM_EGL_SOURCES
    c_src/egl.c
    c_src/active_context_map.c
    c_src/context_map.c
    c_src/command_executor.c
)
add_library(beam-egl SHARED ${BEAM_EGL_SOURCES})

set_target_properties(beam-egl PROPERTIES
    OUTPUT_NAME "beam-egl"
    PREFIX ""
)

# The Rebar3 build system (as configured in rebar.config) does not make use
# of the "install" step. Instead, it produces the NIF library in the priv/
# folder directly during the "build" step.
set_target_properties(beam-egl PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
)

# On macOS, we need to add special linker flags to avoid issues with symbol
# resolution.
if(APPLE)
    set_target_properties(beam-egl PROPERTIES
        LINK_FLAGS "-flat_namespace -undefined suppress"
    )
endif()

target_link_libraries(beam-egl PRIVATE ei EGL)
