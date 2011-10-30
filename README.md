slingshot
=========

Enhanced throw and catch for Clojure
------------------------------------

  Provides `try+` and `throw+`. Each is 100% compatible with Clojure
  and Java's native `try` and `throw` both in source code and at
  runtime. Each also provides new capabilities intended to improve
  ease of use by leveraging Clojure's features like maps, records, and
  destructuring.

  Clojure's native `try` and `throw` behave much like those of Java:
  throw can accept objects derived from java.lang.Throwable and `try`
  selects from among catch clauses based on the class of the thrown
  object.

  In addition to fully supporting those uses (whether they originate
  from Clojure code or from Java code via interop), `try+` and
  `throw+` provide these enhanced capabilities:

  - `throw+` can throw any Java object, not just those whose class is
    derived from `java.lang.Throwable`.

    Clojure maps or records become an easy way to represent custom
    exceptions without requiring `gen-class`.

  - `catch` clauses within `try+` can catch any Java object thrown by
    `throw+`, Clojure's `throw`, or Java's `throw`. The first catch
    clause whose **selector** matches the thrown object will execute.

    a selector can be:

    - a **class name**: (e.g., `RuntimeException`, `my.clojure.record`),
      matches any instance of that class, or

    - a **key-value pair**: (two element vector), matches objects where
      `(get object key)` returns `val`, or

    - a **predicate**: (function of one argument like `map?`, `set?`),
      matches any Object for which the predicate returns a truthy
      value, or

    - a **selector form**: a form containing one or more instances of
      `%` to be replaced by the thrown object, matches any object for
      which the form evaluates to truthy.

    - the class name, key-value pair, and predicate selectors are
      shorthand for these selector forms:

          `<class name>  => (instance? <class name> %)`

          `[<key> <val>] => (= (get % <key>) <val>)`

          `<predicate>   => (<predicate> %)`

  - the binding to the caught exception in a catch clause is not
    required to be a simple symbol. It is subject to destructuring so
    the body of the catch clause can use the contents of a thrown
    collection easily.

  - in a catch clause, the context at the throw site is accessible via
    the hidden argument `&throw-context`.

  - `&throw-context` is a map containing:

    for Throwable caught objects:

        :object       the caught object;
        :message      the message, from .getMessage;
        :cause        the cause, from .getCause;
        :stack-trace  the stack trace, from .getStackTrace;
        :throwable    the caught object;

    for non-Throwable caught objects:

        :object       the caught object;
        :message      the message, from the optional argument to throw+;
        :cause        the cause, captured by throw+, see below;
        :stack-trace  the stack trace, captured by throw+;
        :throwable    the outermost Throwable wrapper of the caught object,
                      see below;
        :environment  a map from names to values for locals visible at
                      the throw+ site.

  To throw a non-`Throwable` object, `throw+` wraps it with a
  `Throwable` object of class `slingshot.Stone`. That `Stone` may in
  turn end up wrapped by other exceptions (e.g., instances of
  `RuntimeException` or `java.util.concurrent.ExecutionException`).
  `try+` sees through all such wrappers to find the object wrapped by
  the first instance of `Stone` in the outermost wrapper's cause
  chain. If needed, the outermost wrapper is available within a catch
  clause a via the `:wrapper` key in `&throw-context`. Any nested
  wrappers are accessible via its cause chain.

  When `throw+` throws a non-`Throwable` object from within a `try+`
  catch clause, the outermost wrapper of the caught object being
  processed is captured as the cause of the new throw+.

Usage
-----

  project.clj

        [slingshot "0.8.0"]

  tensor/parse.clj

        (ns tensor.parse
          (:use [slingshot.slingshot :only [throw+]]))

        (defn parse-tree [tree hint]
          (if (bad-tree? tree)
            (throw+ {:type ::bad-tree :tree tree :hint hint})
            (parse-good-tree tree hint)))

  math/expression.clj

        (ns math.expression
          (:require [tensor.parse]
                    [clojure.tools.logging :as log])
          (:use [slingshot.slingshot :only [throw+ try+]]))

        (defn read-file [file]
          (try+
            [...]
            (tensor.parse/parse-tree tree)
            [...]
            (catch [:type :tensor.parse/bad-tree] {:keys [tree hint]}
              (log/error "failed to parse tensor" tree "with hint" hint)
              (throw+))
            (catch Object _
              (log/error (:throwable &throw-context) "unexpected error")
              (throw+))))

Credits
-------

  Based on clojure.contrib.condition, data-conveying-exception,
  discussions on the clojure mailing list and wiki and discussions and
  implementations by Steve Gilardi, Phil Hagelberg, and Kevin Downey.

License
-------

  Copyright &copy; 2011 Stephen C. Gilardi, Kevin Downey, and Phil Hagelberg

  Distributed under the Eclipse Public License, the same as Clojure.
