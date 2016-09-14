Floron
======

This is my static website generator. There are many like it, but this one is mine.

It is used to generate [Molten Matter](https://steinuil.github.io/molten-matter).

It is written in Scheme. [chibi-scheme](https://github.com/ashinn/chibi-scheme),
to be precise. But I'm sure one wouldn't have any problems porting it to other
schemes, as the only portions specific to chibi-scheme are `(chibi sxml)`,
`(chibi time)` and `(chibi filesystem)`, all of which are quite ubiquitous.

I do not advise using the generator. I suggest you just write your own. Make your
own static (or dynamic) blog. Write your own theme. Use any language you see fit
for the job. Seriously, it's not hard.

## Running

- Move `config.example.scm` to `config.scm` and edit it, or something.
- Create `posts/` and a directory for each post id, and write your post in `post.md`.
- Run `chibi-scheme floron.scm`

## But why
Because your mother sucks dwarf cock.

> Why Scheme?

Because it's fun.

> Why write your own?

Because there's no point in having a personal blog if you didn't write it.

> Why not just use chibi scheme's parser combinator?

I've already written my own and I'm too lazy to learn how chibi's works.

> Why "floron"?

It's the old french name of the character at the end of the post.
