# embed_script

(needs a better name)

## Motivation

* embeddable
* simple
* can run untrusted code - no access to sensitive memory, no DoS attacks
* fast

(finish writing this section)

## Description

* The syntax is inspired by [HyperTalk](https://en.wikipedia.org/wiki/HyperTalk).
* The bytecode is inspired by Lua.
* The way it's used by the host system resembles the Actor pattern.

Example:
```
event load
    set status: 0
    if asdf > 3
    end if
end event
```

(finish writing the example)

## Advantages

1. Execution is deterministic - the same starting state and inputs always produce the same ending
state and outputs.
2. A program always takes up an amount of memory that is known ahead of time so all memory can be
statically allocated.
3. A program is guaranteed to complete execution - a program won't hang indefinitely because there
is no infinite looping or recursion.
4. Denial of Service and memory access attacks should be difficult if not practically impossible to
pull off, making it generally safe to run code that comes from untrusted sources (i.e. the
internet).

## Limitations

In order to have the above advantages, the following limitations must exist:

1. All loops have a fixed upper bound.
2. No recursion, either direct or indirect.
3. No dynamic memory allocation.
4. No direct function calls to the host environment, only messages in an outbox.
5. Messages in the outbox won't be processed by the host environment until the script stops.

Interaction with a scripting engine is completely transactional: An event is sent to the scripting
engine, then the response to the event executes all the way to completion, then all of the outbound
messages become available for processing by the environment that the VM is embedded in.

## Usage

(show an example of embedding a script in a Rust program)
