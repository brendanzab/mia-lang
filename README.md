# mia-lang

A little concatenative language inspired by [Cat](https://web.archive.org/web/20150205160323/http://www.cat-language.com/).

```
$ cargo run
     Running `target/debug/mia`

|\/|. _
|  ||(_|  - a concatenative language

mia> 1 2 3 * +
7
mia> 1 2 3 swap dup
1 3 2 2
mia>  2 3 [ pop dup ] apply
2 2
mia> 2 [ dup dup ] [ * * ] compose apply
8
mia>
```

## Todo

- [x] Arithmetic primitive opterators
- [ ] String literals
- [ ] Function definitions
- [ ] Type checking (see the [Cat papers](https://web.archive.org/web/20150205063918/http://cat-language.com/paper.html) for ideas)
- [ ] Source compilation
