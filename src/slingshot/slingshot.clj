(ns slingshot.slingshot
  (:require [slingshot.support :as s]))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:

    - catch non-Throwable objects thrown by throw+ as well as
      Throwable objects thrown by throw or throw+;

    - specify objects to catch by class name, key-values,
      predicate, or arbitrary selector form;

    - destructure the caught object;

    - in a catch clause, access the names and values of the locals
      visible at the throw site, including the name of the enclosing
      function and its arguments (unless shadowed by nested locals).

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
  (let [[expressions catch-clauses finally-clauses] (s/parse-try+ body)]
    `(try
       ~@expressions
       ~@(s/transform-catch catch-clauses `throw+)
       ~@finally-clauses)))

(defmacro throw+
  "Like the throw special form, but can throw any object by wrapping
  non-Throwable objects in a Throwable wrapper.

  throw+ has the same syntax and behavior as throw for Throwable
  objects. The message, cause, and stack trace are those carried by
  the Throwable.

  For non-Throwable objects, throw+ packages the object, message,
  cause, stack trace, and environment in a Throwable wrapper:

    - message: optional, specified either by a string or a format
      string and args for clojure.core/format:

      - % symbols (at any nesting depth) within args represent the
        thrown object

      - the default is: \"throw+: %s\" (pr-str %)

    - cause: for a throw+ call within a try+ catch clause, the cause
      is the outermost wrapper of the caught object being processed.
      In any other case, the cause is nil;

    - stack trace: the stack trace of the current thread at the time
      of the throw+ call, starting at the function that encloses it;

    - environment: a map from names to values for locals visible at
      the throw+ call site, including the enclosing function and its
      arguments (unless shadowed by nested locals).

  Within a try+ catch clause, a throw+ call with no arguments rethrows
  the caught object within its original (possibly nested) wrappers.

  See also try+, get-throw-context"
  ([object]
     `(throw+ ~object "throw+: %s" (pr-str ~'%)))
  ([object message]
     `(throw+ ~object "%s" ~message))
  ([object fmt arg & args]
     `(let [environment# (s/environment)
            ~'% ~object
            message# (apply format (list ~fmt ~arg ~@args))
            stack-trace# (s/stack-trace)]
        (s/throw-context ~'% message# stack-trace# environment#)))
  ([]
     `(s/rethrow)))

(defn get-throw-context
  "Returns the throw context for an object thrown by throw or throw+
  given a Throwable t. Allows callers to access information about any
  thrown object as a Clojure map.

  If t or any Throwable in its cause chain wraps a non-Throwable
  object thrown by throw+, returns the associated context with t
  assoc'd as the value for :throwable, and the wrapper assoc'd as the
  value for :wrapper, else returns a new context based on t.

  Within a try+ catch clause, prefer using the &throw-context local to
  calling get-throw-context explicitly.

  A throw context is a map containing:

    - for Throwable objects:
      :object       the object;
      :message      the message, from .getMessage;
      :cause        the cause, from .getCause;
      :stack-trace  the stack trace, from .getStackTrace;
      :throwable    the object;

    - for non-Throwable objects:
      :object       the object;
      :message      the message, see throw+;
      :cause        the cause, see throw+;
      :stack-trace  the stack trace, see throw+;
      :environment  the environment, see throw+;
      :wrapper      the Throwable wrapper that carried the object,
                    see below;
      :throwable    the outermost Throwable whose cause chain contains
                    the wrapper, see below;

  To throw a non-Throwable object, throw+ wraps it in a Throwable
  wrapper. The wrapper is available via the :wrapper key in the throw
  context.

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
  source of the Throwable may or may not have been throw+.

  See also get-throw-context"
  [t]
  (-> t get-throw-context :object))
