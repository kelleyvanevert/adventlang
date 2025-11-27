;; echo.wat
;; Prints back stdin -> stdout, in one go
;; For some reason the max size read by fd_read is 1024, even though I set the buffer size to be bigger, so I need a loop to read it
;; But, afterwards, I print it back out with fd_write, in one go
(module
  (import "console" "log" (func $log (param i32 i32)))
  (import "js" "mem" (memory 1))

  ;; Constants
  (global $HEADER_SIZE i32 (i32.const 8))
  (global $NULL i32 (i32.const 0))

  ;; The head of our linked list of free blocks.
  ;; We initialize it to 8 because address 0 is reserved for "NULL".
  (global $free_list_head (mut i32) (i32.const 80))

  (data (i32.const 12) "Growing memory!")

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

  (func $grow_memory (result i32)
    (local $old_pages i32)
    (local $new_page_addr i32)
    (local $page_size i32)
    (local $total_new_bytes i32)

    (call $log (i32.const 12) (i32.const 16))

    (local.set $page_size (i32.const 65536)) ;; 64KB

    ;; Try to grow memory by 1 page (returns old size in pages)
    (local.set $old_pages (memory.grow (i32.const 1)))

    ;; Check if memory.grow failed (returns -1 on failure)
    (if (i32.eq (local.get $old_pages) (i32.const -1))
        (then
            (return (i32.const 0)) ;; Growth failed (actual OOM)
        )
    )

    ;; Calculate the starting address of the new block
    (local.set $new_page_addr (i32.mul (local.get $old_pages) (local.get $page_size)))

    ;; The size of the new block (total new bytes)
    (local.set $total_new_bytes (local.get $page_size))

    ;; 1. Store the size of the new block at its start
    (i32.store (local.get $new_page_addr) (local.get $total_new_bytes))

    ;; 2. Link the current free list head to the new block's next pointer
    (i32.store
        (i32.add (local.get $new_page_addr) (i32.const 4)) ;; Address + 4 (Next ptr)
        (global.get $free_list_head)
    )

    ;; 3. Update the global head to point to the new block
    (global.set $free_list_head (local.get $new_page_addr))

    (i32.const 1) ;; Return 1 (success)
  )

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
    (local $page_size i32)

    (local.set $page_size (i32.const 65536))

    ;; 1. SANITY CHECK: CAN WE ALLOCATE THIS FROM A SINGLE PAGE?
    ;; If req_size > (PAGE_SIZE - HEADER_SIZE), it's impossible to fulfill.
    (if (i32.gt_u
          (local.get $req_size)
          (i32.sub (local.get $page_size) (global.get $HEADER_SIZE))
        )
      (then
        (return (i32.const 0)) ;; Request is too large for a dynamic growth model
      )
    )

    ;; Add header size to requested size
    (local.set $needed_total (i32.add (local.get $req_size) (global.get $HEADER_SIZE)))

    ;; Start searching from the head
    (local.set $curr_ptr (global.get $free_list_head))
    (local.set $prev_ptr (global.get $NULL))

    (block $found
      (loop $search_loop
        ;; If curr_ptr is NULL, we reached end of list (OOM)
        ;; NEW OOM / GROW CHECK
        (if (i32.eq (local.get $curr_ptr) (global.get $NULL))
          (then
            (if (i32.eq (call $grow_memory) (i32.const 0)) ;; Check return value (0 = failure)
              (then
                (return (i32.const 0)) ;; Real OOM, cannot grow memory
              )
              (else
                ;; Memory grew! Restart the loop to search the new block
                (local.set $curr_ptr (global.get $free_list_head))
                (local.set $prev_ptr (global.get $NULL))
                (br $search_loop)
              )
            )
          )
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


  ;; ===
  ;; ===
  ;; ===
  ;; ===

  ;; ------------------------------------------------------------
  ;; HELPER: MEMCPY
  ;; Copies $len bytes from $src to $dest
  ;; ------------------------------------------------------------
  (func $memcpy (param $dst i32) (param $src i32) (param $len i32)
    (local $i i32)
    (local.set $i (i32.const 0))

    (block $done
      (loop $loop
        ;; If i == len, stop
        (br_if $done (i32.eq (local.get $i) (local.get $len)))

        ;; load byte from src + i
        ;; store byte to dest + i
        (i32.store8
          (i32.add (local.get $dst) (local.get $i))
          (i32.load8_u (i32.add (local.get $src) (local.get $i)))
        )

        ;; i++
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $loop)
      )
    )
  )

  ;; ------------------------------------------------------------
  ;; VEC PUSH
  ;; Params:
  ;;   $ptr, $cap, $len: The "Fat Pointer" components
  ;;   $val: new element data
  ;; Returns:
  ;;   (ptr, cap, len): The updated "Fat Pointer"
  ;; ------------------------------------------------------------
  (global $N i32 (i32.const 4)) ;; corresponds with `$val` being i32 taking 4 bytes
  (func $push (export "push")
    (param $ptr i32) (param $cap i32) (param $len i32) (param $val i32)
    (result i32 i32 i32) ;; Returns updated (ptr, cap, len)

    (local $new_cap i32)
    (local $new_ptr i32)
    (local $byte_size i32)

    ;; 1. CHECK IF GROWTH NEEDED
    ;; -------------------------
    (if (i32.eq (local.get $len) (local.get $cap))
      (then
        ;; -- CALCULATE NEW CAP --
        ;; If cap is 0, start with 4, else cap * 2
        (if (i32.eq (local.get $cap) (i32.const 0))
          (then (local.set $new_cap (i32.const 4)))
          (else (local.set $new_cap (i32.mul (local.get $cap) (i32.const 2))))
        )

        ;; -- ALLOCATE NEW BUFFER --
        ;; size = new_cap * N
        (local.set $byte_size (i32.mul (local.get $new_cap) (global.get $N)))
        (local.set $new_ptr (call $malloc (local.get $byte_size)))

        ;; -- COPY OLD DATA (If exists) --
        (if (i32.gt_u (local.get $len) (i32.const 0))
          (then
            (call $memcpy
              (local.get $new_ptr)
              (local.get $ptr)
              (i32.mul (local.get $len) (global.get $N)) ;; bytes to copy
            )
            ;; Free the old array
            (call $free (local.get $ptr))
          )
        )

        ;; -- UPDATE LOCALS --
        (local.set $ptr (local.get $new_ptr))
        (local.set $cap (local.get $new_cap))
      )
    )

    ;; 2. INSERT NEW ELEMENT
    ;; ---------------------
    ;; Target Address = ptr + (len * N)
    ;; (call $memcpy
    ;;   (i32.add (local.get $ptr) (i32.mul (local.get $len) (global.get $N))) ;; Dest
    ;;   (local.get $val) ;; Src (the new element)
    ;;   (global.get $N)      ;; Size
    ;; )
    (i32.store
      (i32.add (local.get $ptr) (i32.mul (local.get $len) (global.get $N))) ;; dest
      (local.get $val)
    )

    ;; 3. INCREMENT LEN
    ;; ----------------
    (local.set $len (i32.add (local.get $len) (i32.const 1)))

    ;; 4. RETURN FAT POINTER
    ;; ---------------------
    (local.get $ptr)
    (local.get $cap)
    (local.get $len)
  )


  ;; ------------------------------------------------------------
  ;; Create a new vec
  ;; ------------------------------------------------------------
  (func $new_vec (export "new_vec")
    (param $initial_cap i32)
    (result i32 i32 i32) ;; Returns (ptr, cap, len)

    (local $ptr i32)
    (local $byte_size i32)

    ;; Check if capacity is greater than 0
    (if (i32.gt_u (local.get $initial_cap) (i32.const 0))
      (then
        ;; Calculate byte size: cap * N
        (local.set $byte_size
          (i32.mul (local.get $initial_cap) (global.get $N))
        )

        ;; Allocate memory for the buffer
        (local.set $ptr (call $malloc (local.get $byte_size)))
      )
      (else
        ;; If initial_cap is 0, the pointer must be NULL (0)
        (local.set $ptr (i32.const 0))
      )
    )

    ;; Stack push: ptr
    (local.get $ptr)
    ;; Stack push: cap
    (local.get $initial_cap)
    ;; Stack push: len (always 0 for a new vector)
    (i32.const 0)
  )
)
