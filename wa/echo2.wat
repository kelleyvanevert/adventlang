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

    ;; The starting address of our buffer
    (global $bufferAddress i32 (i32.const 12))
    (global $bufferSize i32 (i32.const 65536))

    (func $main (export "_start")
        (local $total i32)
        (local.set $total (i32.const 0))

        ;; *buf = address of buf = 12
        (i32.store (i32.const 4) (global.get $bufferAddress))
        ;; buf_len = 1024
        (i32.store (i32.const 8) (global.get $bufferSize))

        (loop $read_loop
          (block $exit_loop
            ;; --- 2. CALL fd_read (Read from stdin) ---
            ;; fd_read(fd=0/stdin, iovs=4, iovs_len=1, nread_ptr=0)
            (i32.const 0)  ;; File descriptor 0 (stdin)
            (i32.const 4)  ;; Address of iovec array (4)
            (i32.const 1)  ;; Number of iovecs (1)
            (i32.const 0)  ;; Address to store nread (0)
            (call $fd_read)
            (drop)           ;; Drop the result of fd_read (the error code)

            (br_if $exit_loop (i32.eq (i32.load (i32.const 0)) (i32.const 0)))

            ;; move the buffer start
            (i32.store (i32.const 4)
              (i32.add
                (i32.load (i32.const 4))
                (i32.load (i32.const 0))
              )
            )
            ;; increase the total num bytes read
            (local.set $total
              (i32.add
                (local.get $total)
                (i32.load (i32.const 0))
              )
            )

            (br $read_loop)
          )
        )

        ;; reset the iovec to point to the start, and the length to be the total bytes read
        (i32.store (i32.const 4)
          (i32.const 12)
        )
        (i32.store (i32.const 8)
          (local.get $total)
        )

        ;; --- 5. CALL fd_write (Write to stdout) ---
        ;; fd_write(fd=1/stdout, iovs=4, iovs_len=1, nwritten_ptr=0)
        (i32.const 1)  ;; File descriptor 1 (stdout)
        (i32.const 4)  ;; Address of iovec array (4)
        (i32.const 1)  ;; Number of iovecs (1)
        (i32.const 0)  ;; Address to store nwritten (0)
        (call $fd_write) ;; call fd_write and drop the result code
        (drop)
    )
)
