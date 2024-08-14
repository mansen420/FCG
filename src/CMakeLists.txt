cmake_minimum_required(VERSION 3.19)

set(exec_name prototype)

file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/*.cpp)

add_executable(${exec_name})
target_sources(${exec_name} PRIVATE ${SRC_FILES})
target_link_libraries(${exec_name} PRIVATE SDL2::SDL2)
set_target_properties(${exec_name} PROPERTIES CXX_STANDARD 20)
set_target_properties(${exec_name} PROPERTIES CMAKE_CXX_STANDARD_REQUIRED ON)