;; echo.wat
;; Prints back stdin -> stdout, in one go
;; For some reason the max size read by fd_read is 1024, even though I set the buffer size to be bigger, so I need a loop to read it
;; But, afterwards, I print it back out with fd_write, in one go
(module
    ;; define the expected type for fd_write
    (type $write_type (func (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (type $write_type)))

    (type $read_type (func (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (type $read_type)))

    ;; Define a memory that is one page in size (64KiB).
    (memory $mem 1)
    (export "memory" (memory $mem))

    (data (i32.const 12) "Hello world!\n")

    (func $main (export "_start") ;; `_start` is WASI's entry point (I think)

      (i32.store (i32.const 4) (i32.const 12))
      (i32.store (i32.const 8) (i32.const 13))

      ;; --- 5. CALL fd_write (Write to stdout) ---
      ;; fd_write(fd=1/stdout, iovs=4, iovs_len=1, nwritten_ptr=0)
      (i32.const 1)  ;; File descriptor 1 (stdout)
      (i32.const 4)  ;; Address of iovec array (4)
      (i32.const 1)  ;; Number of iovecs (1)
      (i32.const 0)  ;; Address to store nwritten (0)
      (call $fd_write) ;; call fd_write and drop the result code
      (drop)

    )

    (start $main) ;; wasm's module init
)
