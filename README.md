# diff-eq

Pronouced: Diff-eee-cue

Appends a diff message to tests that fail equality.

![Screenshot in cider][screenshot]

## Usage

Add this to your lein profile:

```clojure
{:user {:dependencies [[diff-eq "0.1.0"]]
        :injections [(require 'diff-eq.core)
                     (diff-eq.core/diff!)]}}
```

Or your `project.clj`:

```clojure
(defproject ...

  :profiles {:dev {:dependencies [[diff-eq "0.1.0"]]
                   :injections [(require 'diff-eq.core)
                                (diff-eq.core/diff!)]}})
```

And then `lein deps`.

## License

Copyright © 2015 Mayvenn

Distributed under the Eclipse Public License either version 1.0.


[screenshot]: images/screenshot.png
