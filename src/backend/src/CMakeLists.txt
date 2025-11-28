CMAKE_MINIMUM_REQUIRED(VERSION 3.10)
PROJECT(JASPSyntaxBackendLib CXX)

set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Get the minGW/gcc compilers in order to achieve highest degree of uniformity while users only have to install Rtools.
include(cmake/find_toolchain.cmake)


# find your binaries here
find_package(Qt6 REQUIRED COMPONENTS Quick Widgets)
find_package(SQLite3 3.37.0 REQUIRED)

qt_standard_project_setup(REQUIRES 6.10)

message(STATUS "QT_DEFAULT_MAJOR_VERSION: ${QT_DEFAULT_MAJOR_VERSION}")

# Make a shared library
file(GLOB_RECURSE SOURCES "*.cpp")
qt_add_library(jaspSyntaxBackend SHARED ${SOURCES})

# In order to load the right Qt plugins, add a QML file which contains the needed imports, and then call qt_import_qml_plugins
qt_add_resources(jaspSyntaxBackend "Dummy" FILES Dummy.qml) 


message(STATUS "JASP_SOURCE_DIR: $ENV{JASP_SOURCE_DIR}")
message(STATUS "JASP_BUILD_DIR: $ENV{JASP_BUILD_DIR}")


# The header includes go here
# Leave the two /include and /src/internal ones in here; the rest can be altered.
target_include_directories(jaspSyntaxBackend
  PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    "$ENV{JASP_SOURCE_DIR}/SyntaxInterface"
)

target_link_libraries(jaspSyntaxBackend
    PUBLIC 
    Qt::Core
    Qt::Widgets
    SQLite::SQLite3
    $ENV{JASP_BUILD_DIR}/SyntaxInterface/libSyntaxInterface.a
    $ENV{JASP_BUILD_DIR}/QMLComponents/JASP/Controls/libQMLComponentsplugin.a
    $ENV{JASP_BUILD_DIR}/QMLComponents/libQMLComponents.a
    $ENV{JASP_BUILD_DIR}/Common/libCommon.a
    $ENV{JASP_BUILD_DIR}/CommonData/libCommonData.a
    $ENV{JASP_BUILD_DIR}/R-Interface/libR-InterfaceNoRInside.a
    $ENV{R_HOME}/lib/libR.dylib
    ${CMAKE_PREFIX_PATH}/plugins/platforms/libqminimal.a
)

qt_import_qml_plugins(jaspSyntaxBackend)

#include(GNUInstallDirs)


if(WIN32)
  SET_TARGET_PROPERTIES(jaspSyntaxBackend PROPERTIES
    OUTPUT_NAME "jaspSyntaxBackend"
    PREFIX ""
  )
endif()
