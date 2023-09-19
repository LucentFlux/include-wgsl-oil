pub mod arena;
pub mod parsing;
pub mod spans;

/// Performs nice formatting to take a list of items and join them into a descriptive list,
/// like the ones used when Rust-c says "expected `a`, `b`, `c`, or one of 96 others"
fn join_into_readable_list<'a, 'b: 'a>(vs: impl IntoIterator<Item = &'a &'b str>) -> String {
    let vs = vs.into_iter().collect::<Vec<_>>();

    if vs.len() == 0 {
        return "nothing - no valid values".to_owned();
    }

    let mut res = String::new();

    if vs.len() > 1 {
        res += "one of "
    }

    let i_last = vs.len() - 1;
    for (i, var) in vs.iter().enumerate() {
        res += "`";
        res += var;
        res += "`";

        if i >= 3 && vs.len() >= 6 {
            res += " or one of ";
            res += &format!("{}", vs.len() - i - 1);
            res += " others";
            break;
        }

        if i != i_last {
            if i < i_last - 1 {
                res += ", "
            } else {
                res += " or "
            }
        }
    }
    return res;
}

/// https://en.wikipedia.org/wiki/Levenshtein_distance
fn edit_distance(
    a: &str,
    b: &str,
    deletion_cost: usize,
    insertion_cost: usize,
    substitution_cost: usize,
) -> usize {
    fn edit_distance_inner(
        a: &str,
        b: &str,
        deletion_cost: usize,
        insertion_cost: usize,
        substitution_cost: usize,
    ) -> usize {
        let width = b.len() + 1;

        let mut matrix = vec![0; width];

        for i in 0..width {
            matrix[i] = i;
        }

        for (i, a_char) in a.chars().enumerate() {
            let mut pre = matrix[0];
            matrix[0] = i + 1;
            for (j, b_char) in b.chars().enumerate() {
                let tmp = matrix[j + 1];
                matrix[j + 1] = std::cmp::min(
                    // deletion
                    tmp + deletion_cost,
                    std::cmp::min(
                        // insertion
                        matrix[j] + insertion_cost,
                        // match or substitution
                        pre + if a_char == b_char {
                            0
                        } else {
                            substitution_cost
                        },
                    ),
                );
                pre = tmp;
            }
        }

        return matrix[b.len()];
    }

    let len_a = a.chars().count();
    let len_b = b.chars().count();

    if len_a == 0 || len_b == 0 {
        return len_a + len_b;
    }

    if len_a < len_b {
        return edit_distance_inner(b, a, deletion_cost, insertion_cost, substitution_cost);
    } else {
        return edit_distance_inner(a, b, deletion_cost, insertion_cost, substitution_cost);
    }
}

/// Finds an alternative with minimum edit distance to the word given, in the list of options.
fn get_recommended_alternative<'a>(found: &str, alternatives: &[&'a str]) -> Option<&'a str> {
    let mut min_dist = usize::MAX;
    let mut min_found = None;

    for alternative in alternatives.into_iter() {
        let dist = edit_distance(found, alternative, 1, 1, 1);
        if dist < min_dist {
            min_dist = dist;
            min_found = Some(*alternative)
        }
    }

    if min_dist > 10 {
        return None;
    }
    return min_found;
}

/// Like [`Eq`], but for objects which refer into some sort of context, such as handles into arenas.
pub(crate) trait EqIn<'a>: 'a {
    type Context<'b>
    where
        'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool;
}

impl<'a, T: EqIn<'a>> EqIn<'a> for Option<T> {
    type Context<'b> = T::Context<'b> where 'a: 'b;

    fn eq_in<'b>(
        &'b self,
        own_context: &'b Self::Context<'b>,
        other: &'b Self,
        other_context: &'b Self::Context<'b>,
    ) -> bool {
        match (self, other) {
            (None, None) => true,
            (Some(lhs), Some(rhs)) => lhs.eq_in(own_context, rhs, other_context),
            _ => false,
        }
    }
}
