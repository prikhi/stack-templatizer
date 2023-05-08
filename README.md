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


For an example repository that generates a stack template, see
[hpack-template](https://github.com/prikhi/hpack-template).


## LICENSE

BSD-3-Clause
