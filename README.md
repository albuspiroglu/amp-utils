
# amp-utils

My utilities

## Motive

## Description

## Installation

### ASDF

First, git clone this repo to your computer

> git clone https://github.com/albuspiroglu/amp-utils.git

Then make asdf find the new system. One way is to create/edit a conf file in ~/.config/common-lisp/source-registry.conf.d/some.conf and add this line:

    (:directory "path-to-download/")

And on your REPL:

```
(asdf:clear-source-registry)
(asdf:load-system "amp-utils")
```

## Usage


### Utilities


## Implementations tested

## Extending / Hacking

## Contributing

## TODO

- [ ] Move dev helper code out of util, to amp-dev package. 
  * e.g. maturity-levels and related code.


## History
### [2022-10-08]


## References

# License

Copyright (c) [2021] [Albus M Piroglu]

Licensed under the MIT License.
