# Ape
A lisp with a focus on libc.

Most of our world runs on / exposes a C API, Ape aims to be a lisp with the functionality of C.

## Examples

### Hello, World!

```clojure
(extern printf Void Str ...)
(extern exit Void Int)

(fn main [] Void
    (printf "Hello, World!\n")
    (exit 0))
```

## Roadmap

* [x] Integer literals
* [x] String literals
* [x] C function interop
* [x] Pointers
* [x] Array literals
* [ ] Ape funcions
* [ ] Macros
* [ ] Enums, Records
* [ ] Hashmap, Cons List
...
* [ ] **Bootstrap**
