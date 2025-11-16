use itertools::Itertools;

pub fn find_unique_match<T, P>(list: &Vec<T>, mut pred: P) -> Option<usize>
where
    P: FnMut(&T) -> bool,
{
    let mut found = None;

    for (i, item) in list.iter().enumerate().filter(|(i, item)| pred(item)) {
        if found.is_some() {
            return None;
        } else {
            found = Some(i);
        }
    }

    found
}
