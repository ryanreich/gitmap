# gitmap

A coordinator for "mapping" git over multiple repositories.  It is
intended to coordinate with a build manager: currently, it supports
only the Haskell 'stack'.  By passing the -y option, gitmap can be
directed to generate a stack.yaml from its own gitmap.yaml file, which
contains slightly more information than it.

## gitmap.yaml

To enable gitmap in a directory, place a file 'gitmap.yaml' with the
following fields:

### repositories: (mandatory)

This field contains a list of git repositories that gitmap is to
manage.  These entries have the following fields:

#### git-url: (mandatory)

This may be any URL acceptable to git,
e.g. `git@github.com:ryanreich/gitmap` or
`https://github.com/ryanreich/gitmap`.

#### extra-git-args: (optional)

This specifies arguments to git that are passed when it is run with
various commands.  It may contain a field for any git command
(e.g. "clone", "pull", etc.), which itself may contain one or both of the fields

##### simple: (optional)

A single space-separated string of arguments.  If an argument must
contain spaces, then it should be in:

##### complex: (optional)

A list of option/value pairs, where the option is everything up to the
first space and the value is everything else, spaces included.

#### extra-deps: (optional)

Exactly the same as the "extra-deps" field in stack.yaml.  These
fields, across all repositories, are concatenated into this field eventually.

#### flags: (optional)

This repository's "flags" entry in the stack.yaml.

### global: (mandatory)

This contains settings that are not specific to individual
repositories.  All of these options are placed into stack.yaml and do
not affect the behavior of gitmap.  Any field that is valid in
stack.yaml may occur here.  Since at least a "resolver" is required,
this section is also mandatory.