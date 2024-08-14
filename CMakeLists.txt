cmake_minimum_required(VERSION 3.19)

set(project_name FCG)

project({$project_name} LANGUAGES CXX)

add_subdirectory(vendor/SDL)
add_subdirectory(src)