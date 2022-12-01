aoc
===

An OTP application :eyes:

Build
-----

    $ rebar3 compile

Play
----

I've not made this a runnable app, but having rebar3 compile it does help.

To play, use the shell in the context of the built ebin folder.

```
1> cd("_build/default/lib/aoc/ebin").
2> aoc21:run().
3> aoc22:run().
```

Once rebar3 has built it, manually recompile any edited files:

```
4> c(aoc22_01).
5> aoc22:run().
```

Erlang quality
--------------

My Erlang is probably very shoddy :stuck_out_tongue_winking_eye:

Take it merely as a demonstration of something that *works* - not that is exemplary!

Erlang tests
------------

I haven't written any yet :shrug:

Come back later if you are interested to see what they look like :thumbsup:

--
