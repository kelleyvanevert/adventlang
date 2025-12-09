use parser::AdventlangParser;

use crate::{
    Env, TypeCheckerCtx,
    types::{FnMeta, Type},
};

pub fn add_stdlib_types(env: &mut Env, ctx: &mut TypeCheckerCtx) {
    let stdlib = "
        // MISC.
        // ======

        print:          fn(int) -> nil                          {al_print_int}
        print:          fn(str) -> nil                          {al_print_str}
        assert:         fn(bool) -> nil                         {al_assert}
        assert_eq:      fn<T>(T, T) -> nil                      {al_assert_T}
        panic:          fn<T>() -> T                            {al_panic_T}
        panic:          fn<T>(str) -> T                         {al_panic_str_T}
        unreachable:    fn<T>() -> T                            {al_unreachable_T}
        unreachable:    fn<T>(str) -> T                         {al_unreachable_str_T}


        // STRINGS
        // ======

        stdin:          fn() -> str                             {al_stdin}
        read_file:      fn(str) -> str                          {al_read_file}
        trim:           fn(str) -> str                          {al_trim}
        split:          fn(str, str) -> [str]                   {al_split_by_str}
        split:          fn(str, regex) -> [str]                 {al_split_by_regex}
        replace:        fn(str, (str, str)) -> str              {al_replace_by_str}
        replace:        fn(str, (regex, str)) -> str            {al_replace_by_regex}
        lines:          fn(str) -> [str]                        {al_lines}
        chars:          fn(str) -> [str]                        {al_chars}
        starts_with:    fn(str, str) -> bool                    {al_starts_with}

        slice:          fn(str, int) -> str                     {al_slice_str_index}
        slice:          fn(str, (int, int)) -> str              {al_slice_str_range}
        slice:          fn<A>([A], int) -> [A]                  {al_slice_list_index}
        slice:          fn<A>([A], (int, int)) -> [A]           {al_slice_list_range}

        match:          fn(str, regex) -> ?[str]                {al_match}
        matches:        fn(str, regex) -> bool                  {al_matches}
        match_all:      fn(str, regex) -> [[str]]               {al_match_all}


        // CONVERSIONS
        // (TODO make concrete implementations)
        // ======

        str:            fn<T>(T) -> str                         {al_convert_to_str}
        int:            fn(str) -> int                          {al_convert_to_int}
        bool:           fn<A>(A) -> bool                        {al_convert_to_bool}
        float:          fn<A>(A) -> float                       {al_convert_to_float}

        clone:          fn<A>(A) -> A                           {al_clone_A}

        as_map:         fn<K, V>([(K, V)]) -> map[K, V]         {al_convert_as_map}
        as_set:         fn<K>([K]) -> set[K]                    {al_convert_as_set}


        // LISTS
        // ======

        new_list:       fn<A>() -> [A]                          {al_new_list_A}
        push:           fn<A>([A], A) -> [A]                    {al_push_A}
        in:             fn<A>(A, [A]) -> bool                   {al_list_in_A}
        in:             fn<A>(A, set[A]) -> bool                {al_set_in_A}
        filter:         fn<A, B>([A], fn(A) -> bool) -> [A]     {al_filter_A_B}
        map:            fn<A, B>([A], fn(A) -> B) -> [B]        {al_map_A_B}
        flat_map:       fn<A, B>([A], fn(A) -> [B]) -> [B]      {al_flat_map_A_B}
        filter_map:     fn<A, B>([A], fn(A) -> B) -> [B]        {al_filter_map_A_B}
        find_map:       fn<A, B>([A], fn(A) -> B) -> ?B         {al_find_map_A_B}
        find:           fn<A>([A], fn(A) -> bool) -> ?A         {al_find_A}
        any:            fn<A>([A], fn(A) -> bool) -> bool       {al_any_A}
        all:            fn<A>([A], fn(A) -> bool) -> bool       {al_all_A}
        all:            fn([bool]) -> bool                      {al_all}
        fold:           fn<T, R>([T], R, fn(R, T) -> R) -> R    {al_fold_T_R}
        enumerate:      fn<T>([T]) -> [(int, T)]                {al_enumerate_T}
        sort_by_key:    fn<T, K>([T], fn(T) -> K) -> [T]        {al_sort_by_key_T_K}
        chunks:         fn<T>([T], int) -> [[T]]                {al_chunks_T}
        zip:            fn<A, B>([A], [B]) -> [(A, B)]          {al_zip_A_B}
        reverse:        fn<T>([T]) -> [T]                       {al_reverse_T}


        // NUMERICS
        // ======

        sum:            fn([int]) -> int                        {al_sum}
        range:          fn(int, int) -> [int]                   {al_range}

        len:            fn(str) -> int                          {al_str_len}
        len:            fn<A>([A]) -> int                       {al_list_len_A}

        max:            fn(int, int) -> int                     {al_max_int_int}

        min:            fn(int, int) -> int                     {al_min_int_int}
        min:            fn(float, int) -> float                 {al_min_float_int}
        min:            fn(int, float) -> float                 {al_min_int_float}
        min:            fn(float, float) -> float               {al_min_float_float}
        min:            fn([int]) -> int                        {al_min_int_list}
        min:            fn([float]) -> float                    {al_min_float_list}

        abs:            fn(int) -> int                          {al_abs_int}
        abs:            fn(float) -> float                      {al_abs_float}

        ceil:           fn(float) -> int                        {al_ceil}
        floor:          fn(float) -> int                        {al_floor}
        round:          fn(float) -> int                        {al_round}

        sqrt:           fn(int) -> float                        {al_sqrt_int}
        sqrt:           fn(float) -> float                      {al_sqrt_float}

        is_some:        fn<A>(?A) -> bool                       {al_is_some}
        unwrap:         fn<A>(?A) -> A                          {al_unwrap}


        // CONSTANTS
        // ======

        MAX_INT: int
        PI: float
        TAU: float


        // OPERATORS
        // ======

        ==: fn<T>(T, T) -> bool
        !=: fn<T>(T, T) -> bool
        &&: fn(bool, bool) -> bool
        ||: fn(bool, bool) -> bool

        >: fn(int, int) -> bool
        >: fn(float, float) -> bool
        <: fn(int, int) -> bool
        <: fn(float, float) -> bool
        <=: fn(int, int) -> bool
        <=: fn(float, float) -> bool
        >=: fn(int, int) -> bool
        >=: fn(float, float) -> bool

        -: fn(int, int) -> int
        -: fn(int, float) -> float
        -: fn(float, int) -> float
        -: fn(float, float) -> float

        -: fn(int) -> int
        -: fn(float) -> float

        !: fn(bool) -> bool

        +: fn(int, int) -> int
        +: fn(int, float) -> float
        +: fn(float, int) -> float
        +: fn(float, float) -> float
        +: fn(str, str) -> str

        ^: fn(int, int) -> int
        ^: fn(int, float) -> float
        ^: fn(float, int) -> float
        ^: fn(float, float) -> float

        *: fn(int, int) -> int
        *: fn(int, float) -> float
        *: fn(float, int) -> float
        *: fn(float, float) -> float

        /: fn(int, int) -> int
        /: fn(int, float) -> float
        /: fn(float, int) -> float
        /: fn(float, float) -> float

        %: fn(int, int) -> int
        %: fn(int, float) -> float
        %: fn(float, int) -> float
        %: fn(float, float) -> float

        <<: fn(int, int) -> int
        >>: fn(int, int) -> int
    ";

    let mut parser = AdventlangParser::new();

    for line in stdlib.trim().lines() {
        if line.trim().len() == 0 || line.trim().starts_with("//") {
            continue;
        }

        let (name, hint) = line.trim().split_once(": ").unwrap();
        let (hint, runtime_name) = hint.split_once("{").unwrap_or((hint, ""));

        match ctx
            .convert_hint_to_type(env, &parser.parse_type(hint.trim()).unwrap())
            .unwrap()
        {
            Type::Fn(mut def) => {
                def.meta = FnMeta::stdlib(name.to_string());
                env.add_named_fn_local(0, name.to_string(), def);
            }
            ty => {
                env.add_local(name.to_string(), (0, ty));
            }
        };
    }
}
