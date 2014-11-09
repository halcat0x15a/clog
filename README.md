# clog

A continuation-based logic programming library for Clojure.

[![Build Status](https://travis-ci.org/halcat0x15a/clog.svg?branch=master)](https://travis-ci.org/halcat0x15a/clog)

[![Clojars Project](http://clojars.org/clog/latest-version.svg)](http://clojars.org/clog)

## Usage

Examples:

```clojure
(require '[clog.core :refer :all])

(defn append [xs ys zs]
  (match [xs zs]
    [() ys] succeed
    [(lcons x xs') (lcons x zs')] (append xs' ys zs')))

(run
  (fresh [x y]
    (append x y [1 2 3 4 5])
    (return [x y])))
;=> ([() [1 2 3 4 5]] [(1) [2 3 4 5]] [(1 2) [3 4 5]] [(1 2 3) [4 5]] [(1 2 3 4) [5]] [(1 2 3 4 5) []])
```

## License

Copyright © 2014 Sanshiro Yoshida.

Distributed under the Eclipse Public License.
