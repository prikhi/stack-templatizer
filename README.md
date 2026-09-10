# stack-templatizer

[![stack-templatizer Build Status](https://github.com/prikhi/stack-templatizer/actions/workflows/main.yml/badge.svg)](https://github.com/prikhi/stack-templatizer/actions/workflows/main.yml)


Stack Templatizer is a small application that lets you generate stack template
`hsfiles` from a folder.

Install or clone & build the project using `stack`:

```sh
# Install from Stack Nightly
stack install stack-templatizer --resolver nightly

# Or build and install from source
git clone https://github.com/prikhi/stack-templatizer
cd stack-templatizer
stack install
```

Once installed, you can run `stack-templatizer my-template-folder` to generate
a `my-template-folder.hsfiles` stack template.

Files that are not valid UTF-8 (images, archives, etc.) are embedded as
base64-encoded `{-# START_FILE BASE64 <name> #-}` sections, which `stack new`
decodes back into the original binary files.

Files matched by a `.gitignore` are skipped, including nested `.gitignore`
files in subdirectories, with nearer `.gitignore` files taking precedence
over farther ones. If a top-level `.gitignore` is present, `.git` is
skipped as well.

Occurrences of a name token (`--name-token`, default `PACKAGENAME`) in file
names and UTF-8 file contents are replaced with `{{name}}`. This lets the
source folder stay a normal, compilable package — using the token as a
literal placeholder name — while the generated template still uses Stack's
own `{{name}}` substitution when unpacked with `stack new`.


For an example repository that generates a stack template, see
[hpack-template](https://github.com/prikhi/hpack-template).


## LICENSE

BSD-3-Clause
