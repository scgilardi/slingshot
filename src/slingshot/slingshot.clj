(ns slingshot.slingshot
  (:require [slingshot.support :as s]))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses and an
  optional else clause:

    - catch non-Throwable objects thrown by throw+ or data made
      throwable by ex-info as well as Throwable objects thrown by
      throw or throw+;

    - specify objects to catch by class name, key-values, predicate,
      or arbitrary selector form;

    - destructure the caught object;

    - an optional else clause may appear after all catch clauses and
      before any finally clause. Its contents will be executed (for
      side effects) immediately after the code in the try+ body
      completes only if nothing was thrown.

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

    The class name, key-values, and predicate selectors are
    shorthand for these selector forms:

      <class name>          => (instance? <class name> %)
      [<key> <val> & <kvs>] => (and (= (get % <key>) <val>) ...)
      <predicate>           => (<predicate> %)

  The binding form in a try+ catch clause is not required to be a
  simple symbol. It is subject to destructuring which allows easy
  access to the contents of a thrown collection.

  The local &throw-context is available within try+ catch clauses,
  bound to the throw context for the caught object.

  See also: throw+, get-throw-context"
  [& body]
  (let [[expressions catches else finally] (s/parse-try+ body)
        threw? (gensym "threw?")]
    `(let [~threw? (atom false)]
       (try
         ~@expressions
         ~@(s/gen-catch catches `throw+ threw?)
         ~@(s/gen-finally else finally threw?)))))

(defmacro throw+
  "Like the throw special form, but can throw any object by wrapping
  non-Throwable objects in a Throwable wrapper.

  throw+ has the same syntax and behavior as throw for Throwable
  objects. The message, cause, and stack trace are those carried by
  the Throwable.

  For non-Throwable objects, the message and cause have default values
  which can be overridden by optional arguments:

    (throw+ object cause? message-or-fmt? & fmt-args)

    - object: required, the object to throw

    - cause: optional, a Throwable, the default is:

      - within a try+ catch clause, the the outermost wrapper of
        the caught object being processed,

      - elsewhere, nil.

    - message: optional, specified either as a string or a format
      string and args for clojure.core/format:

      - % symbols anywhere within args name the thrown object

      - the default is: \"throw+: %s\" (pr-str %)

  The stack trace is that of the current thread at the time of the
  throw+ call, starting at the function that encloses it;

  Within a try+ catch clause, a throw+ call with no arguments rethrows
  the caught object within its original (possibly nested) wrappers.

  See also try+, get-throw-context"
  {:arglists '([] [object cause? message-or-fmt? & fmt-args])}
  ([object & args]
   `(let [~'% ~object]
      (s/throw-fn ~'%
                  (s/resolve-local ~'&throw-context)
                  (s/stack-trace)
                  ~@args)))
  ([]
   `(s/rethrow)))

(defn get-throw-context
  "Returns the throw context for an object thrown by throw or throw+
  given a Throwable t. Allows callers to access information about any
  thrown object as a Clojure map.

  If t or any Throwable in its cause chain wraps a non-Throwable
  object thrown by throw+ or data made throwable by ex-info, returns
  the associated context with t assoc'd as the value for :throwable,
  and the wrapper assoc'd as the value for :wrapper, else returns a
  new context based on t.

  Within a try+ catch clause, prefer using the &throw-context local to
  calling get-throw-context explicitly.

  A throw context is a map containing:

    - for Throwable objects:
      :object       the object;
      :message      the message, from .getMessage;
      :cause        the cause, from .getCause;
      :stack-trace  the stack trace, from .getStackTrace;
      :throwable    the object;

    - for non-Throwable objects (including data made throwable by ex-info):
      :object       the object;
      :message      the message, see throw+, ex-info;
      :cause        the cause, see throw+, ex-info;
      :stack-trace  the stack trace, see throw+, ex-info;
      :wrapper      the Throwable wrapper that carried the object,
                    see below;
      :throwable    the outermost Throwable whose cause chain contains
                    the wrapper, see below;

  To throw a non-Throwable object, throw+ or ex-info wraps it in a
  Throwable wrapper. The wrapper is available via the :wrapper key in
  the throw context.

  Between being thrown and caught, the wrapper may be wrapped by other
  exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). get-throw-context searches
  all nested wrappers to find the thrown object. The outermost wrapper
  is available via the :throwable key in the throw context.

  See also try+"
  [t]
  (s/get-context t))

(defn get-thrown-object
  "Returns the object thrown by throw or throw+ given a Throwable.
  Useful for processing a Throwable outside of a try+ form when the
  source of the Throwable may or may not have been throw+ or ex-info.

  See also get-throw-context"
  [t]
  (-> t get-throw-context :object))
