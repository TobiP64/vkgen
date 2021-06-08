# vkgen

Generates Rust source code from the Vulkan/OpenXR registry.

[Repository](https://gitlab.com/TobiP64/vkgen) 

## General Information

### Licence

This software is [MIT licenced](https://mit-license.org/).

### Dependencies and Requirements

##### Rust Version

The latest Rust version the generated code was tested with is 1.40 and 1.42-nightly.

##### Cargo Crates

`log` is used for logging.

`libloading` is used to load the Vulkan/OpenXR shared library, but the generated code can
easily be modified to use something different.

The generator itself only uses `serde` for parsing the registry.

##### Environment (Shared Libraries etc.)

No C/C++ headers/source code or any binaries are required to compile and run the generated
code, only the Vulkan/OpenXR shared library. (`libvulkan.so` on Linux and `vulkan-1.dll` on
Win32)

##### Other

No other dependencies are required to use the generated code, if the code fails to compile
or crashes at runtime nonetheless and API misuse can be ruled out, please submit an issue
on Gitlab.

## Usage

```
$ vkgen <input file (optional)> [options]
```

If no input file is specified, the registry will be read from `stdin`.

### Options

- `--help`, `-h` display the help page
- `--out=<output file>`, `-o=<output file>` specify the output file
  If no output file is specified, the generated code will be written to `<input file>.rs`
- `--out-cargo=<output cargo file>`, `-oc=<output cargo file>` specify the output cargo file
  If no output file is specified, the generated code will be written to `<input file>.toml`

### Examples

```
$ vkgen ./vk.xml

# specify output files
$ vkgen ./vk.xml -out=vk.rs -out-cargo=vk.toml

# download the Vulkan registry and pipe it to vkgen
$ wget -qO- https://raw.githubusercontent.com/KhronosGroup/Vulkan-Headers/master/registry/vk.xml | vkgen

# download the OpenXR regsitry and pipe it to vkgen
$ wget -qO- https://raw.githubusercontent.com/KhronosGroup/OpenXR-SDK-Source/master/specification/registry/xr.xml | vkgen
```

### Alter library loading method

If you do not want to use libloading, remove the dependency from the generated toml file
and edit `vkInit`/`xrInit` to load the function pointers with your preferred method.

## Details

### Function Loading

When a static function (functions that do not require a dispatchable handle to be passed)
is called for the first time, the Vulkan/OpenXR shared library is loaded in `vkInit`/`xrInit`.
To use non static functions, a dispatchable handle needs to be wrapped with
`VkXxxImpl::new(handle)` or `XrXxxImpl::new(handle)`.

### Wrapper Structs

#### Methods

Wrapper structs are generated for all disptachable handles, containing a function table or
pointing to the function table of their parent handle. When a wrapper method is called, the
equivalent function in the function table is dispatched. The wrapper methods' signatures
only differ from the ones in the table in slice parameters that can not be `null`, where
the functions in the table take a pointer and a length, but the wrapper methods take a
slice, to provide some safety. No other safety guards are provided.

#### Method Tables

The special wrappers `VkInstanceImpl`, `VkDeviceImpl` and `XrInstanceImpl` will populate a
table with function pointers obtained via `vkGetInstanceProcAddr`, `vkGetDeviceProcAddr`
and `xrGetInstanceProcAddr`, respectively, on creation.

#### Other

All wrapper structs implement `Debug` for debugging purposes obviously and `Deref`/`DerefMut`, 
to access the wrapped handles. The table entries are not `pub` and thus cannot be accessed
but with the wrapper methods.

### Structs and Enums

All structs and enums implement `Copy`, `Clone`, `Default` and `Debug` for convenience.

#### Common Mistakes

Initializing a struct with a member that is a pointer to an array might cause undefined
behaviour:

```rust
// never do this

VkSomeStruct {
    arrayLen: 0,
    pArray:   [
        // some elements
    ].as_ptr() // <-- the slice will be dropped here, as it is no longer required,
               //     this sometimes only happens when the release target is built
               //     and is thus very hard to debug
};

// always do this

let array = [
    // some elements
];

VkSomeStruct {
    arrayLen: array.len() as _,
    pArray:   array.as_ptr() // the slice is not dropped, because the declaration
                             // is in the outer scope
};
```

#### Enum Bitfields

Since some enums represent bits in a bitfield, that can be combined, variants have to be
casted to `u32` in order to use them.

```
VK_SOME_ENUM_VARIANT1_BIT as u32 | VK_SOME_ENUM_VARIANT2_BIT as u32
```

#### Struct Bitfields

Newer versions of the Vulkan Registry contain structures with C bitfields. Since Rust does
not support such bitfields, these structs are generated incorrectly and should thus not be
used. As of Mar 2021, only `VkAccelerationStructureInstanceKHR` is affected.

#### VkResult

The enums `VkResult` and `XrResult` implement `Error` and `ops::Try`, which means they can
be used with the `?` operator. To utilize this, a nightly compiler is required and the
`try_trait` feature must be enabled.

```
#![feature("try_trait")]
...
VkInstanceImpl::create(&info, allocator, &mut instance)?; // <-- returns early if the result was an error
```

### Versions and Extensions

Almost every command has a `#[cfg(feature = "VK_VERSION_X_X")]` or
`#[cfg(feature = "VK_SOME_FEATURE_KHR"")]` attribute, which means in order to use this
command a feature must be enabled. The versions of the Vulkan API,
`VK_VERSION_1_0`, `VK_VERSION_1_1` and `VK_VERSION_1_2`, are enabled by default.

### Constants

The type of constants will be inferred by looking at their values. If the generator fails
to infer the type, `u32` will be used instead. This might result in constants with a wrong
type, which prevents them from being passed to functions or used in structs. In this case,
please submit an issue on Gitlab.

### Using Strings

All strings in Vulkan/OpenXR are null-terminated `*const u8`s. The wrapper does not convert
between Rust's and Vulkan's string representations, all strings passed to or returned
from functions or structs have to be converted by the user.

## Examples

This simple example demonstrates how to output the instance version (1.1.0) and create an
instance. vk.rs is the file containing the generated Rust source code. The shared library
will be loaded on the first method call (`VkInstanceImpl::enumerateVersion` in this case).

Cargo.toml:
```toml
[package]
name = "vkgen_test"
version = "0.1.0"
authors = ["tobias"]
edition = "2018"

[features]
default = ["VK_VERSION_1_0", "VK_VERSION_1_1"]
VK_VERSION_1_0 = []
VK_VERSION_1_1 = []
...

[dependencies]
libloading = "0.5.0"
```
main.rs:
```rust
mod vk;

use self::vk::*;
use std::ptr::null;

fn main() {
    let mut v: u32 = 0;
    VkInstanceImpl::enumerateVersion(&mut v);
    println!("vulkan instance version is {}.{}.{}", VK_VERSION_MAJOR(v), VK_VERSION_MINOR(v), VK_VERSION_PATCH(v));
    
    let instance_info = VkInstanceCreateInfo {
        sType:                   VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        pNext:                   null(),
        flags:                   0,
        pApplicationInfo: &VkApplicationInfo {
            sType:              VK_STRUCTURE_TYPE_APPLICATION_INFO,
            pNext:              null(),
            pApplicationName:   "test app\0".as_ptr(),
            applicationVersion: VK_MAKE_VERSION(0, 0, 1),
            pEngineName:        "test engine\0".as_ptr(),
            engineVersion:      VK_MAKE_VERSION(0, 0, 1),
            apiVersion:         VK_MAKE_VERSION(1, 1, 0),
        },
        enabledLayerCount:       0,
        ppEnabledLayerNames:     null(),
        enabledExtensionCount:   0,
        ppEnabledExtensionNames: null()
    };
    
    let mut instance = VK_NULL_HANDLE;
    if VkInstanceImpl::create(&instance_info, null(), &mut instance) != VK_SUCCESS {
        panic!("something went wrong :-/");
    };
    let instance = unsafe { VkInstanceImpl::new(instance) };
}
```
