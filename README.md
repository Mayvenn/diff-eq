# diff-eq

Pronouced: Diff-eee-cue

Appends a diff message to tests that fail equality.

![Screenshot in cider][screenshot]

## Rationale

We were previously using [humane-test-output](https://github.com/pjstadig/humane-test-output),
but it suffers from two problems:

 - It doesn't integrate with cider test. So you can't see the diffs when using cider
 - It [hangs cider](https://github.com/pjstadig/humane-test-output/issues/4) with diffing values that contains floats or doubles.

## Usage

Add this to your lein profile:

```clojure
{:user {:dependencies [[diff-eq "0.2.2"]]
        :injections [(require 'diff-eq.core)
                     (diff-eq.core/diff!)]}}
```

Or your `project.clj`:

```clojure
(defproject ...

  :profiles {:dev {:dependencies [[diff-eq "0.2.2"]]
                   :injections [(require 'diff-eq.core)
                                (diff-eq.core/diff!)]}})
```

And then `lein deps`.

## License

Copyright © 2015 Mayvenn

Distributed under the Eclipse Public License version 1.0.


[screenshot]: images/screenshot.png
