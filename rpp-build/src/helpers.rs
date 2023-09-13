pub fn split_in_path(s: impl AsRef<str>) -> String {
    let mut result = Vec::<String>::new();
    let mut item = String::new();
    let mut count = 0;
    for c in s.as_ref().chars() {
        if c == '<' {
            count += 1;
        } else if c == '>' {
            count -= 1;
        } else if c == ':' && count == 0 && !item.is_empty() {
            result.push(
                item.strip_prefix(':')
                    .unwrap_or(item.as_str())
                    .chars()
                    .take_while(|x| *x != '<')
                    .collect(),
            );
            item.clear();
            continue;
        }
        item.push(c);
    }
    if !item.is_empty() {
        result.push(
            item.strip_prefix(':')
                .unwrap_or(item.as_str())
                .chars()
                .take_while(|x| *x != '<')
                .collect(),
        );
    }
    result.join("::")
}
