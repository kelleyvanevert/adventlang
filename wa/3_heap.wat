;; echo.wat
;; Prints back stdin -> stdout, in one go
;; For some reason the max size read by fd_read is 1024, even though I set the buffer size to be bigger, so I need a loop to read it
;; But, afterwards, I print it back out with fd_write, in one go
(module
  (import "console" "log" (func $log (param i32 i32)))

  ;; Define a memory that is one page in size (64KiB).
  (memory $mem 1)
  (export "memory" (memory $mem))

  ;; Constants
  (global $HEADER_SIZE i32 (i32.const 8))
  (global $NULL i32 (i32.const 0))

  ;; The head of our linked list of free blocks.
  ;; We initialize it to 8 because address 0 is reserved for "NULL".
  (global $free_list_head (mut i32) (i32.const 8))

  ;; ----------------------------------------------------------------
  ;; 1. INITIALIZATION
  ;; Sets up the initial massive free block covering the whole memory.
  ;; ----------------------------------------------------------------
  (func $init
    ;; Initial block size = total size of memory - offset (8)
    (i32.store
      (global.get $free_list_head)
      (i32.sub (i32.mul (memory.size) (i32.const 65536)) (global.get $free_list_head))
    )

    ;; Initial next block pointer = NULL
    (i32.store
      (i32.add (global.get $free_list_head) (i32.const 4))
      (global.get $NULL)
    )
  )

  (start $init)

  ;; ----------------------------------------------------------------
  ;; 2. ALLOCATE
  ;; Finds a block, splits it if necessary, and returns pointer to payload.
  ;; Returns 0 if out of memory.
  ;; ----------------------------------------------------------------
  (func $malloc (export "malloc") (param $req_size i32) (result i32)
    (local $prev_ptr i32)
    (local $curr_ptr i32)
    (local $curr_size i32)
    (local $needed_total i32)
    (local $remaining_size i32)
    (local $new_block_ptr i32)

    ;; Add header size to requested size
    (local.set $needed_total (i32.add (local.get $req_size) (global.get $HEADER_SIZE)))

    ;; Start searching from the head
    (local.set $curr_ptr (global.get $free_list_head))
    (local.set $prev_ptr (global.get $NULL))

    (block $found
      (loop $search_loop
        ;; If curr_ptr is NULL, we reached end of list (OOM)
        (if (i32.eq (local.get $curr_ptr) (global.get $NULL))
          (then (return (i32.const 0)))
        )

        ;; Load size of current block
        (local.set $curr_size (i32.load (local.get $curr_ptr)))

        ;; Check if this block fits
        (if (i32.ge_u (local.get $curr_size) (local.get $needed_total))
          (then
            ;; FOUND IT! Break the loop
            (br $found)
          )
        )

        ;; Move to next block
        (local.set $prev_ptr (local.get $curr_ptr))
        (local.set $curr_ptr (i32.load (i32.add (local.get $curr_ptr) (i32.const 4))))
        (br $search_loop)
      )
    )

    ;; --- LOGIC FOR SPLITTING BLOCK ---

    ;; Calculate remaining space
    (local.set $remaining_size (i32.sub (local.get $curr_size) (local.get $needed_total)))

    ;; Check if remaining space is big enough for a new block (header + 1 byte)
    (if (i32.gt_u (local.get $remaining_size) (global.get $HEADER_SIZE))
      (then
        ;; SPLIT: Resize the current block to what is needed
        (i32.store (local.get $curr_ptr) (local.get $needed_total))

        ;; Create the new split block ahead
        (local.set $new_block_ptr (i32.add (local.get $curr_ptr) (local.get $needed_total)))

        ;; Set size of new block
        (i32.store (local.get $new_block_ptr) (local.get $remaining_size))

        ;; Set 'next' of new block to match current block's 'next'
        (i32.store
          (i32.add (local.get $new_block_ptr) (i32.const 4))
          (i32.load (i32.add (local.get $curr_ptr) (i32.const 4)))
        )

        ;; Update current block to point to the new split block
        (i32.store
          (i32.add (local.get $curr_ptr) (i32.const 4))
          (local.get $new_block_ptr)
        )
      )
    )

    ;; --- UNLINKING FROM LIST ---

    ;; If prev_ptr is NULL, we are removing the head
    (if (i32.eq (local.get $prev_ptr) (global.get $NULL))
      (then
        ;; Head = curr->next
        (global.set $free_list_head
          (i32.load (i32.add (local.get $curr_ptr) (i32.const 4)))
        )
      )
      (else
        ;; Prev->next = curr->next
        (i32.store
          (i32.add (local.get $prev_ptr) (i32.const 4))
          (i32.load (i32.add (local.get $curr_ptr) (i32.const 4)))
        )
      )
    )

    ;; Return the payload pointer (current + 8)
    (i32.add (local.get $curr_ptr) (global.get $HEADER_SIZE))
  )

  ;; ----------------------------------------------------------------
  ;; 3. FREE
  ;; Puts the block back at the start of the list.
  ;; ----------------------------------------------------------------
  (func $free (export "free") (param $ptr i32)
    (local $block_ptr i32)

    ;; Safety check: ignore NULL
    (if (i32.eq (local.get $ptr) (i32.const 0)) (then (return)))

    ;; Calculate actual block start (ptr - 8)
    (local.set $block_ptr (i32.sub (local.get $ptr) (global.get $HEADER_SIZE)))

    ;; Point this block's 'next' to the current head
    (i32.store
      (i32.add (local.get $block_ptr) (i32.const 4))
      (global.get $free_list_head)
    )

    ;; Move head to this block
    (global.set $free_list_head (local.get $block_ptr))
  )
)
